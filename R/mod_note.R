
note_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::textAreaInput(ns("note_text"), "Edit note...", height = "150px") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
  )
}

note_server <- function(id, notes = NULL) {
  moduleServer(id, function(input, output, session) {
    
   
    
  })
}

note_app <- function(notes = NULL) {
  ui <- shiny::fluidPage(
    note_ui("note")
  )
  server <- function(input, output, session) {
    note_server("note", shiny::reactive(notes))
  }
  shiny::shinyApp(ui, server)  
}
