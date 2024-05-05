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
  numericInput('ncols', 'Number of plate columns', value = 1, min = 1, max = 6, step = 1),
  numericInputIcon('samplevol', 'Sample volume', value = 50, min = 20, max = 100, step = 1, icon = list(NULL, 'ul')),
  numericInputIcon('beadsvol', 'Beads volume', value = 50, min = 20, max = 100, step = 1,icon = list(NULL, 'ul')),
  numericInputIcon('inctime', 'Incubation time', value = 5, min = 1, max = 15, step = 1, icon = list(NULL, 'min')),
  selectizeInput('left_pipette', 'Left', choices = c('p20_single_gen2', 'p300_single_gen2')),
  selectizeInput('right_pipette', 'Right', choices = c('p20_multi_gen2', 'p300_multi_gen2')),
  numericInputIcon('aspirate_speed', 'Aspirate speed', 
                   min = 5, max = 100, value = 100, step = 5, 
                   icon = list(NULL, icon("percent")))
  
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
    tags$a('Plate preview'),
    reactableOutput('plate')
  )
)


ui <- page_navbar(
  useShinyjs(),
  fillable = T,
  title = 'General beads cleanup - Flex',
  theme = bs_theme(font_scale = 0.9, bootswatch = 'yeti', primary = '#2E86C1'),
  sidebar = sidebar,
  #nav_spacer(),
  nav_panel('Labware and volumes', panel1),
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
  
  protocol_template <- readLines('01-flex-beads-partial.py', warn = F)
  
  # Reactives
  
  
  # CORE functionality
  myvalues <- reactive({
    
  })
  
  myprotocol <- reactive({
    protocol_template
  })
  
  # Outputs
  # add appropriate mix according to pipetting_type
  
  output$plate <- renderReactable({
    DF <- make_plate(cols = 12, rows = 8, content = input$samplevol, ncols = input$ncols)
    reactable(
      DF,
      highlight = T, wrap = F, bordered = T, compact = T, fullWidth = F, sortable = F, pagination = F,
      columns = list(.rownames = colDef(style = list(color = 'black', fontSize = '90%'))),
      defaultColDef =
        colDef(
          style = function(value) {
            if (value > 0) {
              color <- "#229954"
              fw <- "bold"
            } else {
              color <- 'grey'
              fw <- "lighter"
            }
            list(color = color, fontWeight = fw, fontSize = '90%')
            },
            minWidth = 80,
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
        height = '70px',
        title = "Title",
        value = 20,
        showcase = bsicons::bs_icon('crosshair', size = '70%'), 
        theme_color = 'primary'
      ),
      value_box(
        height = '70px',
        title = "Title",
        value = 10,
        showcase = bsicons::bs_icon('sliders2', size = '70%'), 
        theme_color = 'primary'
      ),
      value_box(
        height = '70px',
        title = 'total volume:',
        value = 30,
        #p('Source: ', input$source_labware),
        #p('Destination: ', input$dest_labware)
        showcase = bsicons::bs_icon('water', size = '70%'),
        theme_color = 'primary'
      )
    )
    layout_column_wrap(width = '250px', !!!vbs)
  })
  # observers
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
      paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-custom-transfer.py')
    },
    content = function(con) {
      # at download time, replace name so that it appears on the Opentrons app
      replacement <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-custom-transfer.py')
      write(myprotocol() %>%
              str_replace(pattern = "10-custom-transfer.py", 
                          replacement = replacement), 
            con)
    }
  )
}

shinyApp(ui, server)
