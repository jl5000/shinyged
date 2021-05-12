

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
                    shinyjs::disabled(
                      shiny::actionButton(ns("remove_note"), "Remove note"),
                      shiny::actionButton(ns("update_note"), "Update note")
                    )
      )
    ),
    shiny::tags$br(),
    shiny::textAreaInput(ns("note_text"), "Edit note...", height = "150px") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
    )

}


notes_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    #523 | The vector of notes
    notes <- shiny::reactive({
      req(r$ged, r$section_rows)
      dplyr::slice(r$ged, r$section_rows) %>%
        dplyr::filter(level == .$level[1] + 1, tag == "NOTE") %>% 
        dplyr::pull(value)
    })
    
    # A running check on the text box to see if the note is valid
    valid_note <- shiny::reactive({
      !is.null(input$note_text) &&
        !input$note_text == "" &&
        !input$note_text %in% notes()
      })
    
    selected_ged_row <- shiny::reactive({
      req(r$ged, r$section_rows, input$notes_list_rows_selected)
      dplyr::mutate(r$ged, row = dplyr::row_number()) %>% 
        dplyr::slice(r$section_rows) %>%
        dplyr::filter(level == .$level[1] + 1, tag == "NOTE") %>% 
        dplyr::slice(input$notes_list_rows_selected) %>% 
        dplyr::pull(row)
    })
    
    # Update table with notes
    output$notes_list <- DT::renderDataTable({
      DT::datatable(data.frame(Notes = notes()), rownames = FALSE, selection = "single")
    })
    
    # Disable add_note button if no valid note
    shiny::observeEvent(input$note_text, {
      shinyjs::toggleState("add_note", valid_note())
    })
    
    # Disable update_note button if no valid note and no row selected
    shiny::observeEvent({
      valid_note()
      input$notes_list_rows_selected}, {
      shinyjs::toggleState("update_note", valid_note() && !is.null(input$notes_list_rows_selected))
    })
    
   # Update text box with selected note and disable update/remove buttons if nothing selected
   # Note: there currently seems to be a bug as it's not firing on deselection events
   shiny::observeEvent(input$notes_list_rows_selected, {
     if(length(input$notes_list_rows_selected) > 0) {
       shiny::updateTextAreaInput(inputId = "note_text", value = notes()[input$notes_list_rows_selected])
     } else {
       shiny::updateTextAreaInput(inputId = "note_text", value = "")
     }
     shinyjs::toggleState("remove_note", !is.null(input$notes_list_rows_selected))
   })
   
   # Add note and clear text box
   shiny::observeEvent(input$add_note, {
     # Don't trigger changes from ged because we also need to update section_rows
     shiny::isolate(
       r$ged <- tibble::add_row(r$ged,
                                tibble::tibble(record = r$ged$record[r$section_rows[1]],
                                               level = r$ged$level[r$section_rows[1]] + 1,
                                               tag = "NOTE",
                                               value = input$note_text),
                                # Need to insert new notes after final note so it
                                # doesn't shift existing row numbers
                                .after = max(r$section_rows))
     )
     r$section_rows <- c(r$section_rows, max(r$section_rows) + 1)
     shiny::updateTextAreaInput(inputId = "note_text", value = "")
   })
   
   # Remove note and clear text box
   shiny::observeEvent(input$remove_note, {
       shiny::isolate(
         r$ged <- dplyr::slice(r$ged, -selected_ged_row())
       )
       r$section_rows <- r$section_rows[-length(r$section_rows)]
       shiny::updateTextAreaInput(inputId = "note_text", value = "")
   })
   
   # Update note
   shiny::observeEvent(input$update_note, {
     r$ged$value[selected_ged_row()] <- input$note_text
   })

  })
}


notes_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    notes_ui("notes")
  )
  server <- function(input, output, session) {
    notes_server("notes", r)
  }
  shiny::shinyApp(ui, server)  
}


