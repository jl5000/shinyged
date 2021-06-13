


source_data_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
  )
  
}


source_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


source_data_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    source_data_ui("source_data")
  )
  server <- function(input, output, session) {
    source_data_server("source_data", r)
  }
  shiny::shinyApp(ui, server)  
}


