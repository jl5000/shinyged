
multimedia_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectInput(ns("records"), label = NULL, choices = NULL, width = "500px")
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add multimedia"),
                    shiny::actionButton(ns("delete"), "Delete multimedia")
      )
      
    ),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary")
      
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
        shiny::updateSelectInput(session = session, inputId = "records", choices = records())
      } else {
        shiny::updateSelectInput(session = session, inputId = "records", choices = character(), selected = character())
      }
    })
    
  })
}

multimedia_app <- function(ged = NULL) {
  ui <- fluidPage(
    multimedia_ui("media")
  )
  server <- function(input, output, session) {
    multimedia_server("media", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}
