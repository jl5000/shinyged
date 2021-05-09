

media_links_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
    
  )
  
}

media_links_server <- function(id, media_links = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    
    
  })
}


media_links_app <- function(media_links = NULL) {
  ui <- shiny::fluidPage(
    media_links_ui("media")
  )
  server <- function(input, output, session) {
    media_links_server("media", shiny::reactive(media_links))
  }
  shiny::shinyApp(ui, server)  
}


