




file_details_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::textInput(ns("receiving_sys"), "Receiving system"),
    shiny::textAreaInput(ns("ged_desc"), "Description", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 85%;'),
    shiny::textAreaInput(ns("ged_copy"), "Copyright statement", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 85%;'),
    shiny::selectInput(ns("language"), "Language", 
                       choices = tidyged.internals::val_languages(), selected = character())
  )
}

file_details_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    

    # Load in data from gedcom object -----------------------------------------
    shiny::observe({
      req(r$ged)
      shiny::updateTextInput(inputId = "receiving_sys", 
                             value = tidyged.internals::gedcom_value(r$ged, "HD", "DEST", 1))
      shiny::updateSelectInput(inputId = "language", 
                             selected = tidyged.internals::gedcom_value(r$ged, "HD", "LANG", 1))
      shiny::updateTextAreaInput(inputId = "ged_desc", 
                               value = tidyged.internals::gedcom_value(r$ged, "HD", "NOTE", 1))
      shiny::updateTextAreaInput(inputId = "ged_copy", 
                                 value = tidyged.internals::gedcom_value(r$ged, "HD", "COPR", 1))
    }) %>% 
      shiny::bindEvent(r$file_count)
    
    # Edit receiving system --------------------------------------------------
    shiny::observe({
      receiving_sys <- process_input(input$receiving_sys)
      err <- tidyged.internals::chk_receiving_system_name(receiving_sys, 1)
      shinyFeedback::feedbackDanger("receiving_sys", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "head_rows", "HD", 1, "DEST", receiving_sys)
    }) %>% 
      shiny::bindEvent(input$receiving_sys, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit language ----------------------------------------------------------
    shiny::observe({
      language <- process_input(input$language)
      err <- tidyged.internals::chk_language_of_text(language, 1)
      shinyFeedback::feedbackDanger("language", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "head_rows", "HD", 1, "LANG", language)
    }) %>% 
      shiny::bindEvent(input$language, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit description --------------------------------------------------------
    shiny::observe({
      ged_desc <- process_input(input$ged_desc)
      err <- tidyged.internals::chk_gedcom_content_description(ged_desc, 1)
      shinyFeedback::feedbackDanger("ged_desc", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "head_rows", "HD", 1, "NOTE", ged_desc)
    }) %>% 
      shiny::bindEvent(input$ged_desc, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit copyright -----------------------------------------------------------
    shiny::observe({
      ged_copy <- process_input(input$ged_copy)
      err <- tidyged.internals::chk_copyright_gedcom_file(ged_copy, 1)
      shinyFeedback::feedbackDanger("ged_copy", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "head_rows", "HD", 1, "COPR", ged_copy)
    }) %>% 
      shiny::bindEvent(input$ged_copy, ignoreNULL = FALSE, ignoreInit = TRUE)
    
  })
}

