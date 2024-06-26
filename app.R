# bslib version
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinyjs)
library(tibble)
library(stringr)
library(reactable)
library(htmlwidgets)
library(sparkline)
library(dplyr)
library(rmarkdown)
library(vroom)
library(plater)
library(shinyjs)
library(processx)

# call wells_colwise(12, 1) to get columns only
wells_colwise <- function(cols, rows) {
  lapply(1:cols, function(x) {str_c(LETTERS[1:rows], x)}) %>% unlist()
}

# used to generate a plater::view_plate()
make_plate <- function(cols, rows, content, ncols) {
  nwells <- cols * rows
  content <- rep(content, each = rows*ncols)
  
  length(content) <- nwells
  #wellcontent <- str_replace_na(wellcontent, replacement = ".")
  df <- tibble(
    id = wells_colwise(cols, rows),
    label = seq(nwells),
    wellcontent = content
  )
  plate <- plater::view_plate(df, well_ids_column = 'id', columns_to_display = 'wellcontent', plate_size = nwells)
  plate$wellcontent
}

# input controls
controls <- list(
  numericInput('ncols', 'Number of plate columns', value = 1, min = 1, max = 12, step = 1),
  numericInputIcon('samplevol', 'Sample volume', value = 50, min = 10, max = 180, step = 1, icon = list(NULL, 'ul')),
  numericInputIcon('beadsvol', 'Beads volume', value = 50, min = 10, max = 180, step = 1,icon = list(NULL, 'ul')),
  numericInputIcon('ebvol', 'EB volume', value = 40, min = 20, max = 100, step = 1,icon = list(NULL, 'ul')),
  numericInputIcon('inctime', 'Incubation time', value = 5, min = 1, max = 15, step = 1, icon = list(NULL, 'min')),
  selectizeInput(
    'pipette', 'Pipette', 
     choices = c('Flex 96-Channel' = 'flex_96channel_1000', 'Flex 8-Channel' = 'flex_8channel_1000')
    ),
  #selectizeInput('right_pipette', 'Right', choices = c('p20_multi_gen2', 'p300_multi_gen2')),
  numericInputIcon('aspirate_speed', 'Aspirate speed', min = 5, max = 100, value = 100, step = 5, icon = list(NULL, icon("percent"))
                   ),
  checkboxInput('dryrun','Dry run', value = F),
  checkboxInput('beadsmix', 'Mix on magnet', value = F)
  # checkboxGroupInput(
  #   'checkboxes', 'Optional', 
  #   choiceNames = c('Dry run', 'Mixing on magnet'), 
  #   choiceValues = c('True', 'True'), 
  #   inline = T)
)

sidebar <- sidebar(
    controls, 
    downloadButton('download_script', 'Download script') #style = 'margin-left:15px; margin-top:15px; color: #444;'),
)


panel1 <- list(
  tags$div(
    tags$a('Plate preview', bs_icon("info-circle")),
    uiOutput('valueboxes'),
    tags$hr(),
    #tags$a('Plate preview'),
    layout_columns(
      col_widths = c(7,5),
      tags$div(tags$a('Plate preview'), tags$hr(), reactableOutput('plate')),
      reactableOutput('test')
    )
  )
)


ui <- page_navbar(
  useShinyjs(),
  fillable = T,
  title = 'General beads cleanup - Flex',
  theme = bs_theme(font_scale = 0.9, bootswatch = 'yeti', primary = '#2E86C1'),
  sidebar = sidebar,
  #nav_spacer(),
  nav_panel('Plate preview', panel1),
  nav_panel('Protocol preview', verbatimTextOutput('protocol_preview')),
  nav_panel(
    'Simulate run',
    actionButton('simulate', 'Run simulation', width = '25%'),
    verbatimTextOutput('stdout')
  ),
  nav_panel('Deck view', htmlOutput('deck'))
)

server <- function(input, output, session) {
  # add opentrons_simulate path
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste(old_path, Sys.getenv('OPENTRONS_PATH'), sep = ":"))
  
  protocol_template <- readLines('03-flex-beads-fullhead.py', warn = F)
  
  # Reactives
  
  
  # CORE functionality
  
  myprotocol <- reactive({
    str_replace(protocol_template, "ncols=[0-9]", paste0('ncols=', input$ncols)) %>%
      #str_replace("Beads cleanup Flex 96-channel, partial loading", paste0("Beads cleanup Flex 96-channel, ", input$ncols, " columns")) %>%
      str_replace("samplevol=.*", paste0("samplevol=", input$samplevol)) %>%
      str_replace("beadsvol=.*", paste0("beadsvol=", input$beadsvol)) %>%
      str_replace("ebvol=.*", paste0("ebvol=", input$ebvol)) %>%
      str_replace("inctime=.*", paste0("inctime= ", input$inctime)) %>%
      str_replace("speed_factor_aspirate =.*", paste0("speed_factor_aspirate = ", round(100/input$aspirate_speed, 2))) %>%
      str_replace("DRY_RUN=.*", replacement = if_else(input$dryrun, "DRY_RUN=True", "DRY_RUN=False")) %>%
      str_replace("BEADSMIX =.*", replacement = if_else(input$beadsmix, "BEADSMIX = True", "BEADSMIX = False"))
  })
  
  tiptable <- reactive({
    times <- if (input$ncols >= 12) {
      11
    } else {
      input$ncols
    }
    tipslist <- list(
      c(rep(10, 4), rep(1, 8)), # always 4 columns needed
      c(rep(10, times), rep(1, 11 - times), 10),
      c(rep(10, times), rep(1, 11 - times), 10),
      c(rep(10, times), rep(1, 11 - times), 10),
      c(rep(10, times), rep(1, 11 - times), 10)
    )
    data_frame(
      position = c('A2', 'B3', 'C3', 'B2', 'C2'),
      type = c('partial', rep('full', 4)),
      tips = tipslist
      
    )
  })
  
  # Outputs
  # add appropriate mix according to pipetting_type
  output$test <- renderReactable({
    reactable(
      tiptable(),
      columns = list(
        tips = colDef(cell = function(values) {
          sparkline(values, type = 'bar', chartRangeMin = 1, chartRangeMax = 10)
        })
      )
    )
  })
  output$plate <- renderReactable({
    DF <- make_plate(
      cols = 12, rows = 8, 
      content = input$samplevol + input$beadsvol,
      ncols = input$ncols
      )
    reactable(
      DF,
      highlight = T, wrap = F, bordered = T, compact = T, fullWidth = F, sortable = F, pagination = F,
      columns = list(.rownames = colDef(style = list(color = 'black', fontSize = '90%'))),
      defaultColDef =
        colDef(
          style = function(value) {
            if (value > 180 & value < 30) { # makes no sense but works
              color <- "#E74C3C"
              fw <- "bold"
            } else {
              color <- '#52BE80'
              fw <- "bold"
            }
            list(color = color, fontWeight = fw, fontSize = '90%')
            },
            minWidth = 50,
            html = TRUE,
            headerStyle = list(background = "#f7f7f8", fontSize = '90%')
          )
      )
  })
  
  
  
  output$protocol_preview <- renderPrint({
    write(myprotocol(), file = "")
  })
  
  output$deck <- renderUI({
    HTML('<img src="deck.png" height="600">')
  })
  
  output$valueboxes <- renderUI({
    
    vbs <- list(
      value_box(
        #height = '100px',
        title = paste0(input$ncols, " columns"),
        value = paste0(input$ncols * 8, ' samples'),
        showcase = bsicons::bs_icon('crosshair', size = '70%'), 
        theme_color = 'primary'
      ),
      value_box(
        #height = '75px',
        title = 'Beads vol (10% overhead)',
        value = ceiling((input$ncols * input$beadsvol * 12) * 1.1),
        #p('Source: ', input$source_labware),
        #p('Destination: ', input$dest_labware)
        showcase = bsicons::bs_icon('water', size = '70%'),
        theme_color = 'primary'
      ),
      value_box(
        #height = '75px',
        title = 'EB vol (10% overhead)',
        value = ceiling((input$ncols * input$ebvol * 12) * 1.1),
        #p('Source: ', input$source_labware),
        #p('Destination: ', input$dest_labware)
        showcase = bsicons::bs_icon('water', size = '70%'),
        theme_color = 'primary'
      ),
      value_box(
        #height = '75px',
        title = 'EtOH vol (10% overhead)',
        value = ceiling((150 * 2 * 12 * input$ncols) * 1.1),
        #p('Source: ', input$source_labware),
        #p('Destination: ', input$dest_labware)
        showcase = bsicons::bs_icon('water', size = '70%'),
        theme_color = 'primary'
      )
    )
    layout_column_wrap(width = '250px', !!!vbs)
  })
  
  # observers
  # observe({
  #   if ( > 180) {
  #     show_alert(
  #       session = session,
  #       title = 'Check volume', text = 'Total volume is > 180 ul', type = 'warning', closeOnClickOutside = T
  #     )
  #   }
  # })
  
  observeEvent(input$simulate, {
    # clear stdout
    shinyjs::html(id = "stdout", "")
    
    # check if opentrons_simulate is in path
    if (system2('which', args = 'opentrons_simulate') == 1) {
      shinyjs::html(id = 'stdout', "opentrons_simulate executable not found. Set the OPENTRONS_PATH variable to the opentrons path.")
      return()
    }
    
    # change button
    shinyjs::disable(id = 'simulate')
    shinyjs::html(id = 'simulate', "Working...")
    tmp <- tempfile('protocol', fileext = '.py')
    write(myprotocol(), file = tmp)
    ht <- as_tibble(hot_to_r(input$hot))
    
    withCallingHandlers({
      if (all(ht$vol == 0)) {
        processx::run(
          'echo', args = ("All volumes are 0, cannot simulate this!"),
          stderr_to_stdout = TRUE, 
          error_on_status = FALSE,
          stdout_line_callback = function(line, proc) {message(line)}
        )
      } else {
        processx::run(
          'opentrons_simulate', 
          args = c('-e', '-L', 'data/labware', tmp),
          stderr_to_stdout = TRUE, 
          error_on_status = FALSE,
          stdout_line_callback = function(line, proc) {message(line)}, 
        )
      }
      shinyjs::enable(id = 'simulate')
      shinyjs::html(id = 'simulate', "Run simulation")
    },
    message = function(m) {
      shinyjs::html(id = "stdout", html = m$message, add = TRUE); 
      runjs("document.getElementById('stdout').scrollTo(0,1e9);") 
      # scroll the page to bottom with each message, 1e9 is just a big number
    }
    )
  })
  
  
  # downloads
  output$download_script <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-beads-cleanup-flex.py')
    },
    content = function(con) {
      # at download time, replace name so that it appears on the Opentrons app
      replacement <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-beads-cleanup-flex.py')
      write(myprotocol(), #%>%
              #str_replace(pattern = "Beads cleanup Flex 96-channel, full head loading", 
              #            replacement = replacement), 
            con)
    }
  )
}

shinyApp(ui, server)
