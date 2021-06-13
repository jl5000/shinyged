


family_members_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
  )
  
}


family_members_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


family_members_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    family_members_ui("family_members")
  )
  server <- function(input, output, session) {
    family_members_server("family_members", r)
  }
  shiny::shinyApp(ui, server)  
}


