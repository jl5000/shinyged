


source_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
  )
  
}


source_summary_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


source_summary_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    source_summary_ui("source_summary")
  )
  server <- function(input, output, session) {
    source_summary_server("source_summary", r)
  }
  shiny::shinyApp(ui, server)  
}


