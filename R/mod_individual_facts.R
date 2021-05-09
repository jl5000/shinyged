





individual_facts_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
    
    
  )
}

individual_facts_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


individual_facts_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    individual_facts_ui("individual_facts")
  )
  server <- function(input, output, session) {
    individual_facts_server("individual_facts", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}


