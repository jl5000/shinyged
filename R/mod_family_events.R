





family_events_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
    
    
  )
}

family_events_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


family_events_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    family_events_ui("family_events")
  )
  server <- function(input, output, session) {
    family_events_server("family_events", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}


