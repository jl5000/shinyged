





notes_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$br(),
    shiny::helpText("Here you can manage notes associated with the item. Use the buttons to add, remove, and edit notes via the text box and by selecting notes in the list."),
    shiny::tags$hr(),
    DT::DTOutput(ns("notes_list")),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(ns("add_note"), "Add note"),
                    shiny::actionButton(ns("remove_note"), "Remove note"),
                    shiny::actionButton(ns("update_note"), "Update note"))
    ),
    shiny::tags$br(),
    shiny::textAreaInput(ns("note_text"), "Edit note...", height = "150px") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
    )

}

notes_server <- function(id, notes = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Update table with notes
    output$notes_list <- DT::renderDataTable({
      req(notes)
      DT::datatable(data.frame(Notes = notes()), rownames = FALSE, selection = "single")
    })
    
    # Disable add_note button if no text
    observeEvent(input$note_text, {
      shinyjs::toggleState("add_note", input$note_text != "" | is.null(input$note_text))
    })
    
   # Update text box with selected note and disable update/remove buttons if nothing selected
   shiny::observeEvent({
     input$notes_list_rows_selected 
     notes}, {
     shiny::updateTextAreaInput(inputId = "note_text", value = notes()[input$notes_list_rows_selected])
     shinyjs::toggleState("update_note", !is.null(notes()) & !is.null(input$notes_list_rows_selected))
     shinyjs::toggleState("remove_note", !is.null(notes()) & !is.null(input$notes_list_rows_selected))
   })
   
   # Remove note and clear text box
   shiny::observeEvent(input$remove_note, {
     if(!is.null(input$notes_list_rows_selected)) {
       notes(notes()[-input$notes_list_rows_selected])
       shiny::updateTextAreaInput(inputId = "note_text", value = "")
     }
   })
   
   # Add note and clear text box
   shiny::observeEvent(input$add_note, {
     if(input$note_text != "") {
       notes(unique(c(notes(), input$note_text)))
       shiny::updateTextAreaInput(inputId = "note_text", value = "")
     }
   })
   
   # Update note
   shiny::observeEvent(input$update_note, {
     if(input$note_text != "") {
       nts <- notes()
       nts[input$notes_list_rows_selected] <- input$note_text
       notes(unique(nts))
     }
   })

  })
}


notes_app <- function(notes = NULL) {
  ui <- shiny::fluidPage(
    notes_ui("notes")
  )
  server <- function(input, output, session) {
    notes_server("notes", shiny::reactiveVal(notes))
  }
  shiny::shinyApp(ui, server)  
}


