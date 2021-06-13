





file_data_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::splitLayout(
      shiny::textInput(ns("ged_source_name"), "Source name"),
      shiny::textInput(ns("ged_source_date"), "Publication date (e.g. 6 APR 1983)")
    ),
    shiny::textAreaInput(ns("ged_source_copy"), "Copyright", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
    
  )
}

file_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(r$ged, once = TRUE, {
      shiny::updateTextInput(session = session, "ged_source_name", 
                             value = tidyged.internals::gedcom_value(r$ged, "HD", "DATA", 2))
      shiny::updateTextInput(session = session, "ged_source_date", 
                             value = tidyged.internals::gedcom_value(r$ged, "HD", "DATE", 3))
      shiny::updateTextAreaInput(session = session, "ged_source_copy", 
                                 value = tidyged.internals::gedcom_value(r$ged, "HD", "COPR", 3))

    })
    
    shiny::observeEvent(input$ged_source_name, ignoreNULL = FALSE, ignoreInit = TRUE, {
      ged_source_name <- process_input(input$ged_source_name)
      err <- tidyged.internals::chk_name_of_source_data(ged_source_name, 1)
      err1 <- input$ged_source_name == "" & input$ged_source_date != ""
      err2 <- input$ged_source_name == "" & input$ged_source_copy != ""
      shinyFeedback::feedbackDanger("ged_source_name", !is.null(err), err)
      shinyFeedback::feedbackDanger("ged_source_date", err1, "Source name is required for this input")
      shinyFeedback::feedbackDanger("ged_source_copy", err2, "Source name is required for this input")
      req(is.null(err), !err1, !err2, cancelOutput = TRUE)
      dummy <- update_ged_value(r, "head_file_sour_rows", 2, "DATA", ged_source_name, .pkgenv$tags_file_sour)
    })
    shiny::observeEvent(input$ged_source_date, ignoreNULL = FALSE, ignoreInit = TRUE, {
      ged_source_date <- process_input(input$ged_source_date)
      err <- tidyged.internals::chk_date_exact(ged_source_date, 1)
      shinyFeedback::feedbackDanger("ged_source_date", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      dummy <- update_ged_value(r, "head_file_sour_rows", 3, "DATE", ged_source_date, .pkgenv$tags_file_sour)
    })
    shiny::observeEvent(input$ged_source_copy, ignoreNULL = FALSE, ignoreInit = TRUE, {
      ged_source_copy <- process_input(input$ged_source_copy)
      err <- tidyged.internals::chk_copyright_source_data(ged_source_copy, 1)
      shinyFeedback::feedbackDanger("ged_source_copy", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      dummy <- update_ged_value(r, "head_file_sour_rows", 3, "COPR", ged_source_copy, .pkgenv$tags_file_sour)
    })
    
    shiny::observeEvent(input$ged_source_name, {
      shinyjs::toggleState("ged_source_date", input$ged_source_name != "" | is.null(input$ged_source_name))
      shinyjs::toggleState("ged_source_copy", input$ged_source_name != "" | is.null(input$ged_source_name))
    })
    
  })
}

