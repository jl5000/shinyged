


repository_details_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$br()
  )
  
}


repository_details_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


repository_details_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    repository_details_ui("repository_details")
  )
  server <- function(input, output, session) {
    repository_details_server("repository_details", r)
  }
  shiny::shinyApp(ui, server)  
}


