





file_data_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$br(),
    shiny::splitLayout(
      shiny::textInput(ns("ged_source_name"), "Source name"),
      shiny::textInput(ns("ged_source_date"), "Publication date (e.g. 6 APR 1983)")
    ),
    shiny::textAreaInput(ns("ged_source_copy"), "Copyright", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
    
  )
}

file_data_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(ged(), {
      shiny::updateTextInput(session = session, "ged_source_name", 
                             value = tidyged.internals::gedcom_value(ged(), "HD", "DATA", 2))
      shiny::updateTextInput(session = session, "ged_source_date", 
                             value = tidyged.internals::gedcom_value(ged(), "HD", "DATE", 3))
      shiny::updateTextAreaInput(session = session, "ged_source_copy", 
                                 value = tidyged.internals::gedcom_value(ged(), "HD", "COPR", 3))

    })
    
    observeEvent(input$ged_source_name, {
      shinyjs::toggleState("ged_source_date", input$ged_source_name != "" | is.null(input$ged_source_name))
      shinyjs::toggleState("ged_source_copy", input$ged_source_name != "" | is.null(input$ged_source_name))
    })
    
  })
}

file_data_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    file_data_ui("file_data")
  )
  server <- function(input, output, session) {
    file_data_server("file_data", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}


