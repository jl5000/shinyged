




file_details_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::textInput(ns("receiving_sys"), "Receiving system"),
    shiny::textAreaInput(ns("ged_desc"), "Description", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;'),
    shiny::textAreaInput(ns("ged_copy"), "Copyright statement", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;'),
    shiny::selectInput(ns("language"), "Language", 
                       choices = tidyged.internals::val_languages(), selected = character())
  )
}

file_details_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(r$ged, {
      shiny::updateTextInput(session = session, "receiving_sys", 
                             value = tidyged.internals::gedcom_value(r$ged, "HD", "DEST", 1))
      shiny::updateSelectInput(session = session, "language", 
                             selected = tidyged.internals::gedcom_value(r$ged, "HD", "LANG", 1))
      shiny::updateTextAreaInput(session = session, "ged_desc", 
                               value = tidyged.internals::gedcom_value(r$ged, "HD", "NOTE", 1))
      shiny::updateTextAreaInput(session = session, "ged_copy", 
                                 value = tidyged.internals::gedcom_value(r$ged, "HD", "COPR", 1))
    })
    
    shiny::observeEvent(input$receiving_sys, ignoreNULL = FALSE, ignoreInit = TRUE, {
      receiving_sys <- process_input(input$receiving_sys)
      err <- tidyged.internals::chk_receiving_system_name(receiving_sys, 1)
      shinyFeedback::feedbackDanger("receiving_sys", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      dummy <- update_ged_value(r, "head_rows", 1, "DEST", receiving_sys)
    })
    shiny::observeEvent(input$language, ignoreNULL = FALSE, ignoreInit = TRUE, {
      language <- process_input(input$language)
      err <- tidyged.internals::chk_language_of_text(language, 1)
      shinyFeedback::feedbackDanger("language", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      dummy <- update_ged_value(r, "head_rows", 1, "LANG", language)
    })
    shiny::observeEvent(input$ged_desc, ignoreNULL = FALSE, ignoreInit = TRUE, {
      ged_desc <- process_input(input$ged_desc)
      err <- tidyged.internals::chk_gedcom_content_description(ged_desc, 1)
      shinyFeedback::feedbackDanger("ged_desc", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      dummy <- update_ged_value(r, "head_rows", 1, "NOTE", ged_desc)
    })
    shiny::observeEvent(input$ged_copy, ignoreNULL = FALSE, ignoreInit = TRUE, {
      ged_copy <- process_input(input$ged_copy)
      err <- tidyged.internals::chk_copyright_gedcom_file(ged_copy, 1)
      shinyFeedback::feedbackDanger("ged_copy", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      dummy <- update_ged_value(r, "head_rows", 1, "COPR", ged_copy)
    })
    
  })
}

file_details_app <- function(ged = NULL) {
  r <- shiny::reactiveValues(ged = ged)
  ui <- shiny::fluidPage(
    file_details_ui("file_details")
  )
  server <- function(input, output, session) {
    file_details_server("file_details", r)
  }
  shiny::shinyApp(ui, server)  
}


