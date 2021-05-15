
multimedia_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("records"), label = NULL, choices = NULL,
                                       multiple = TRUE, width = "500px", options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add multimedia"),
                    shiny::actionButton(ns("delete"), "Delete multimedia")
      )
      
    ),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary", multimedia_summary_ui(ns("multimedia_summary"))),
      shiny::tabPanel("Description", multimedia_description_ui(ns("multimedia_description"))),
      shiny::tabPanel("Notes", notes_ui(ns("media_notes"))),
      shiny::tabPanel("Citations", citations_ui(ns("multimedia_citations")))
    )
  )
}

multimedia_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    records <- shiny::reactive({
      req(ged)
      tidyged::xrefs_media(ged()) %>% 
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

multimedia_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    multimedia_ui("media")
  )
  server <- function(input, output, session) {
    multimedia_server("media", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}
