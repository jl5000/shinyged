





family_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
    
    
  )
}

family_summary_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


family_summary_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    family_summary_ui("family_summary")
  )
  server <- function(input, output, session) {
    family_summary_server("family_summary", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}


