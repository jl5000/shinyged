




file_detailsUI <- function(id) {
  shiny::tagList(
    shiny::tags$br(),
    shiny::splitLayout(
      shiny::textInput(shiny::NS(id, "receiving_sys"), "Receiving system"),
      shiny::selectInput(shiny::NS(id, "language"), "Language", 
                         choices = tidyged.internals::val_languages())
    ),
    shiny::textAreaInput(shiny::NS(id, "ged_desc"), "Description", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;'),
    shiny::textAreaInput(shiny::NS(id, "ged_copy"), "Copyright statement", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
    
  )
}

file_detailsServer <- function(id, ged = NULL) {
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
    
  })
}

file_detailsApp <- function(ged = NULL) {
  ui <- fluidPage(
    file_detailsUI("file_details")
  )
  server <- function(input, output, session) {
    file_detailsServer("file_details", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}


