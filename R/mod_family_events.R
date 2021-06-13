


family_events_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
  )
  
}


family_events_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


family_events_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    family_events_ui("family_events")
  )
  server <- function(input, output, session) {
    family_events_server("family_events", r)
  }
  shiny::shinyApp(ui, server)  
}


