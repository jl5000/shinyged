


repository_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
  )
  
}


repository_summary_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


repository_summary_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    repository_summary_ui("repository_summary")
  )
  server <- function(input, output, session) {
    repository_summary_server("repository_summary", r)
  }
  shiny::shinyApp(ui, server)  
}


