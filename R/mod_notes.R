

notes_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$br(),
    shiny::helpText("Here you can manage notes associated with an item. You can either add notes via the text box or point to existing note records.", 
                    "Use the buttons to add, remove, and edit notes via the text box and by selecting notes in the list."),
    shiny::tags$hr(),
    DT::DTOutput(ns("notes_list")),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(ns("add_note"), "Add note"),
                    shiny::actionButton(ns("choose_note_ref"), "Add reference to global note"),
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


notes_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # The vector of notes
    notes_raw <- shiny::reactive({
      req(r$ged, r[[section_rows]])
      
      dplyr::slice(r$ged, r[[section_rows]]) %>%
        dplyr::filter(level == .$level[1] + 1, tag == "NOTE") %>% 
        dplyr::pull(value)
    })
    
    # The vector of notes, but with references to global notes replaced with note text
    notes_txt <- shiny::reactive({
      req(notes_raw)
      
      vapply(notes_raw(), function(nt) {if(stringr::str_detect(nt, tidyged.internals::reg_xref()))
                                        dplyr::filter(r$ged, record == nt, level == 0)$value else
                                        nt},
             character(1),
             USE.NAMES = FALSE)
    })

    # A running check on the text box to see if the note is valid
    valid_note <- shiny::reactive({
      !is.null(input$note_text) &&
        !input$note_text == "" &&
        !input$note_text %in% notes_txt()
      })
    
    # The row of the tidyged object corresponding to the selected note
    selected_ged_row <- shiny::eventReactive({
      r$ged
      r[[section_rows]]
      input$notes_list_rows_selected
    },{
      req(r$ged, r[[section_rows]], input$notes_list_rows_selected)

        dplyr::mutate(r$ged, row = dplyr::row_number()) %>% 
          dplyr::slice(r[[section_rows]]) %>%
          dplyr::filter(level == .$level[1] + 1, tag == "NOTE") %>% 
          dplyr::slice(input$notes_list_rows_selected) %>% 
          dplyr::pull(row)
    })
    
    # Update table with notes
    output$notes_list <- DT::renderDataTable({
      DT::datatable(data.frame(Notes = notes_txt()), rownames = FALSE, selection = "single")
    })
    
    # Disable add_note button if no valid note
    shiny::observeEvent(input$note_text, {
      shinyjs::toggleState("add_note", valid_note())
    })
    
    # Disable choose_note_ref button if no note records to point to
    shiny::observeEvent(r$ged, {
      shinyjs::toggleState("choose_note_ref", tidyged::num_note(r$ged) > 0)
    })
    
    # Disable update_note button if no valid note and no row selected
    shiny::observeEvent({
      valid_note()
      input$notes_list_rows_selected}, ignoreNULL = FALSE, {
      shinyjs::toggleState("update_note", valid_note() && 
                                          !is.null(input$notes_list_rows_selected) && 
                                          !stringr::str_detect(notes_raw()[input$notes_list_rows_selected], 
                                                               tidyged.internals::reg_xref()))
    })
    
   # Update text box with selected note and disable update/remove buttons if nothing selected
   shiny::observeEvent(input$notes_list_rows_selected, ignoreNULL = FALSE, {
     if(length(input$notes_list_rows_selected) > 0) {
       shiny::updateTextAreaInput(inputId = "note_text", value = notes_txt()[input$notes_list_rows_selected])
     } else {
       shiny::updateTextAreaInput(inputId = "note_text", value = "")
     }
     shinyjs::toggleState("remove_note", !is.null(input$notes_list_rows_selected))
   })
   
   # Add note and clear text box
   shiny::observeEvent(input$add_note, {
     
     r$ged <- tibble::add_row(r$ged,
                              tibble::tibble(record = r$ged$record[r[[section_rows]][1]],
                                             level = r$ged$level[r[[section_rows]][1]] + 1,
                                             tag = "NOTE",
                                             value = input$note_text),
                              # Need to insert new notes after final note so it
                              # doesn't shift existing row numbers
                              .after = max(r[[section_rows]]))
     
     shiny::updateTextAreaInput(inputId = "note_text", value = "")
   })
   
   # Show dialog to choose a note record
   shiny::observeEvent(input$choose_note_ref, {
     req(r$ged)
     shiny::showModal(
       shiny::modalDialog(
         shiny::selectizeInput(ns("note_ref_list"), label = "Add reference to existing note record...",
                               choices = dplyr::filter(r$ged, level == 0, tag == "NOTE")$value,
                               multiple = TRUE, options = list(maxItems = 1), width = "500px"),
         footer = shiny::tagList(
           shiny::modalButton("Cancel"),
           shiny::actionButton(ns("add_note_ref"), "Add reference")
         )
       )
     )
   })
   
   # Disable add_note_ref button if no note records or it's already been added
   shiny::observeEvent(input$note_ref_list, ignoreNULL = FALSE, {
     shinyjs::toggleState("add_note_ref", !is.null(input$note_ref_list) && 
                            !input$note_ref_list %in% notes_txt())
   })
   
   # Add note reference
   shiny::observeEvent(input$add_note_ref, {
     note_xref <- dplyr::filter(r$ged, level == 0, tag == "NOTE", value == input$note_ref_list)$record
     
     r$ged <- tibble::add_row(r$ged,
                              tibble::tibble(record = r$ged$record[r[[section_rows]][1]],
                                             level = r$ged$level[r[[section_rows]][1]] + 1,
                                             tag = "NOTE",
                                             value = note_xref),
                              # Need to insert new notes after final note so it
                              # doesn't shift existing row numbers
                              .after = max(r[[section_rows]]))
     
     shiny::removeModal()
   })
   
   # Remove note and clear text box
   shiny::observeEvent(input$remove_note, {
     r$ged <- dplyr::slice(r$ged, -selected_ged_row())

     shiny::updateTextAreaInput(inputId = "note_text", value = "")
   })
   
   # Update note
   shiny::observeEvent(input$update_note, {
     r$ged$value[selected_ged_row()] <- input$note_text
   })

  })
}



