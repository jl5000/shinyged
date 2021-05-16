


multimedia_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
  )
  
}


multimedia_summary_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


multimedia_summary_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    multimedia_summary_ui("multimedia_summary")
  )
  server <- function(input, output, session) {
    multimedia_summary_server("multimedia_summary", r)
  }
  shiny::shinyApp(ui, server)  
}


