





notes_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::actionButton(ns("add_note"), "Add note"),
    shiny::actionButton(ns("remove_note"), "Remove note"),
    #some kind of note picker
    shiny::textAreaInput(ns("note_text"), "Edit note...", height = "75px")
    )

}

notes_server <- function(id, notes = NULL) {
  moduleServer(id, function(input, output, session) {
    
   
    
    
  })
}


notes_app <- function(notes = NULL) {
  ui <- fluidPage(
    notes_ui("notes")
  )
  server <- function(input, output, session) {
    notes_server("notes", shiny::reactive(notes))
  }
  shinyApp(ui, server)  
}


