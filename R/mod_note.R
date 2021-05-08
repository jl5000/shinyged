
note_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    notes_ui(ns("notes"))
  )
}

note_server <- function(id, notes = NULL) {
  moduleServer(id, function(input, output, session) {
    
    notes_server("notes", notes)
    
  })
}

note_app <- function(notes = NULL) {
  ui <- fluidPage(
    note_ui("note")
  )
  server <- function(input, output, session) {
    note_server("note", shiny::reactive(notes))
  }
  shinyApp(ui, server)  
}
