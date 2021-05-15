




submitter_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::helpText("The submitter is the individual who has created and owns the file."),
    shiny::textInput(ns("subm_name"), "Name"),
    shiny::tabsetPanel(
      shiny::tabPanel("Contact details", address_ui(ns("subm_address"))),
      shiny::tabPanel("Notes", notes_ui(ns("subm_notes"))),
      shiny::tabPanel("Media", media_links_ui(ns("subm_media")))
    )
    
  )
}

submitter_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    subm <- reactive({
      req(ged)
      tidyged::xrefs_subm(ged())
    })
    
    addr <- reactive({
      req(ged, subm)
      dplyr::filter(ged(), record == subm(),
                    tag %in% c(paste0("ADR",1:3),"CITY","STAE","POST","CTRY","EMAIL","WWW","FAX","PHON"))
    })

    notes <- reactive({
      req(ged, subm)
      dplyr::filter(ged(), record == subm(), tag == "NOTE")$value
    })

    media_links <- reactive({
      req(ged, subm)
      dplyr::filter(ged(), record == subm(), tag == "OBJE")$value
    })
    
    observeEvent(ged(), {
      
      shiny::updateTextInput(session = session, "subm_name", 
                             value = dplyr::filter(ged(), record == subm(), tag == "NAME")$value)
    })
    
    address_server("subm_address", addr) #Problem
    notes_server("subm_notes", notes)
    media_links_server("subm_media", media_links)
    
    
  })
}


submitter_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    submitter_ui("submitter")
  )
  server <- function(input, output, session) {
    submitter_server("submitter", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}


