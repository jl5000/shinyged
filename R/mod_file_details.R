




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
    
    observeEvent(input$receiving_sys, {
      req(r$ged)
      shiny::isolate(
        r$ged <- dplyr::mutate(r$ged, value = ifelse(tag == "DEST", "New", value))
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
  r <- shiny::reactiveValues(ged = ged)
  ui <- shiny::fluidPage(
    file_details_ui("file_details")
  )
  server <- function(input, output, session) {
    file_details_server("file_details", r)
  }
  shiny::shinyApp(ui, server)  
}


