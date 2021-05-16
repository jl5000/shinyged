


family_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
  )
  
}


family_summary_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


family_summary_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    family_summary_ui("family_summary")
  )
  server <- function(input, output, session) {
    family_summary_server("family_summary", r)
  }
  shiny::shinyApp(ui, server)  
}


