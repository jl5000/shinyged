




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

file_details_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(ged(), {
      shiny::updateTextInput(session = session, "receiving_sys", 
                             value = tidyged.internals::gedcom_value(ged(), "HD", "DEST", 1))
      shiny::updateSelectInput(session = session, "language", 
                             selected = tidyged.internals::gedcom_value(ged(), "HD", "LANG", 1))
      shiny::updateTextAreaInput(session = session, "ged_desc", 
                               value = tidyged.internals::gedcom_value(ged(), "HD", "NOTE", 1))
      shiny::updateTextAreaInput(session = session, "ged_copy", 
                                 value = tidyged.internals::gedcom_value(ged(), "HD", "COPR", 1))
    })
    
    observeEvent(input$receiving_sys, {
      shiny::isolate(
        ged <- dplyr::mutate(ged(), value = ifelse(tag == "DEST", "New", value))
      )
    })
    
    # observeEvent(input$language, {
    #   shiny::isolate(
    #     # ged <- tidyged.internals::update(ged())
    #   )
    # })
    # 
    # observeEvent(input$ged_desc, {
    #   shiny::isolate(
    #     # ged <- tidyged.internals::update(ged())
    #   )
    # })
    # 
    # observeEvent(input$ged_copy, {
    #   shiny::isolate(
    #     # ged <- tidyged.internals::update(ged())
    #   )
    # })
    
  })
}

file_details_app <- function(ged = NULL) {
  ui <- fluidPage(
    file_details_ui("file_details")
  )
  server <- function(input, output, session) {
    file_details_server("file_details", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}


