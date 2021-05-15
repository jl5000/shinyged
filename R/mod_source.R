
source_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("records"), label = NULL, choices = NULL,
                                          multiple = TRUE, width = "500px", options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add source"),
                    shiny::actionButton(ns("delete"), "Delete source")
      )
      
    ),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary", source_summary_ui(ns("sour_summary"))),
      shiny::tabPanel("Data", source_data_ui(ns("sour_data"))),
      shiny::tabPanel("Details", source_details_ui(ns("sour_details"))),
      shiny::tabPanel("Notes", notes_ui(ns("sour_notes"))),
      shiny::tabPanel("Media", media_links_ui(ns("sour_media")))
    )
  )
}

source_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    records <- shiny::reactive({
      req(ged)
      tidyged::xrefs_sour(ged()) %>% 
        tidyged::describe_records(ged(), ., short_desc = TRUE)
    })
    
    observeEvent(ged(), {
      if(!is.null(records())) {
        shiny::updateSelectizeInput(session = session, inputId = "records", choices = records())
      } else {
        shiny::updateSelectizeInput(session = session, inputId = "records", choices = character(), selected = character())
      }
    })
    
  })
}

source_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    source_ui("sour")
  )
  server <- function(input, output, session) {
    source_server("sour", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}
