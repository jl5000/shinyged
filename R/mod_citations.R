


citations_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$br()
  )
  
}


citations_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    

    
  })
}


citations_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    citations_ui("citations")
  )
  server <- function(input, output, session) {
    citations_server("citations", r)
  }
  shiny::shinyApp(ui, server)  
}


