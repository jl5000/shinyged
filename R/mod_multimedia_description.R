


multimedia_description_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
  )
  
}


multimedia_description_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


multimedia_description_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    multimedia_description_ui("multimedia_description")
  )
  server <- function(input, output, session) {
    multimedia_description_server("multimedia_description", r)
  }
  shiny::shinyApp(ui, server)  
}


