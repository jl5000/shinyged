


source_details_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$br()
  )
  
}


source_details_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


source_details_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    source_details_ui("source_details")
  )
  server <- function(input, output, session) {
    source_details_server("source_details", r)
  }
  shiny::shinyApp(ui, server)  
}


