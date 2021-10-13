





file_data_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::splitLayout(
      shiny::textInput(ns("ged_source_name"), "Source name"),
      shiny::textInput(ns("ged_source_date"), "Publication date (e.g. 6 APR 1983)")
    ),
    shiny::textAreaInput(ns("ged_source_copy"), "Copyright", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 85%;')
    
  )
}

file_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    shiny::observe({
      req(r$ged)
      shiny::updateTextInput(session = session, "ged_source_name", 
                             value = tidyged.internals::gedcom_value(r$ged, "HD", "DATA", 2))
      shiny::updateTextInput(session = session, "ged_source_date", 
                             value = tidyged.internals::gedcom_value(r$ged, "HD", "DATE", 3))
      shiny::updateTextAreaInput(session = session, "ged_source_copy", 
                                 value = tidyged.internals::gedcom_value(r$ged, "HD", "COPR", 3))

    }) %>% 
      shiny::bindEvent(r$file_count)
    
    shiny::observe({
      ged_source_name <- process_input(input$ged_source_name)
      err <- tidyged.internals::chk_name_of_source_data(ged_source_name, 1)
      err1 <- input$ged_source_name == "" & input$ged_source_date != ""
      err2 <- input$ged_source_name == "" & input$ged_source_copy != ""
      shinyFeedback::feedbackDanger("ged_source_name", !is.null(err), err)
      shinyFeedback::feedbackDanger("ged_source_date", err1, "Source name is required for this input")
      shinyFeedback::feedbackDanger("ged_source_copy", err2, "Source name is required for this input")
      req(is.null(err), !err1, !err2, cancelOutput = TRUE)
      update_ged_value(r, "head_file_sour_rows", "HD", 2, "DATA", ged_source_name, .pkgenv$tags_file_sour)
    }) %>% 
      shiny::bindEvent(input$ged_source_name, ignoreNULL = FALSE, ignoreInit = TRUE)
      
    shiny::observe({
      ged_source_date <- process_input(input$ged_source_date)
      err <- tidyged.internals::chk_date_exact(ged_source_date, 1)
      shinyFeedback::feedbackDanger("ged_source_date", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "head_file_sour_rows", "HD", 3, "DATE", ged_source_date, .pkgenv$tags_file_sour)
    }) %>% 
      shiny::bindEvent(input$ged_source_date, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    shiny::observe({
      ged_source_copy <- process_input(input$ged_source_copy)
      err <- tidyged.internals::chk_copyright_source_data(ged_source_copy, 1)
      shinyFeedback::feedbackDanger("ged_source_copy", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "head_file_sour_rows", "HD", 3, "COPR", ged_source_copy, .pkgenv$tags_file_sour)
    }) %>% 
      shiny::bindEvent(input$ged_source_copy, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    shiny::observe({
      shinyjs::toggleState("ged_source_date", input$ged_source_name != "")
      shinyjs::toggleState("ged_source_copy", input$ged_source_name != "")
    }) %>% 
      shiny::bindEvent(input$ged_source_name)
    
  })
}

