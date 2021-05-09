






individual_names_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
    
    
  )
}

individual_names_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


individual_names_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    individual_names_ui("individual_names")
  )
  server <- function(input, output, session) {
    individual_names_server("individual_names", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}


