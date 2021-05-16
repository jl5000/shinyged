





individual_facts_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
    
    
  )
}

individual_facts_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


individual_facts_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    individual_facts_ui("individual_facts")
  )
  server <- function(input, output, session) {
    individual_facts_server("individual_facts", r)
  }
  shiny::shinyApp(ui, server)  
}


