





family_members_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
    
    
  )
}

family_members_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


family_members_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    family_members_ui("family_members")
  )
  server <- function(input, output, session) {
    family_members_server("family_members", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}


