





individual_links_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
    
    
  )
}

individual_links_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


individual_links_app <- function(ged = NULL) {
  ui <- fluidPage(
    individual_links_ui("individual_links")
  )
  server <- function(input, output, session) {
    individual_links_server("individual_links", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}


