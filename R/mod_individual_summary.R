





individual_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
    
    
  )
}

individual_summary_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


individual_summary_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    individual_summary_ui("individual_summary")
  )
  server <- function(input, output, session) {
    individual_summary_server("individual_summary", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}


