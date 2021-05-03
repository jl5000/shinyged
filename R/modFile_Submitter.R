




file_submitterUI <- function(id) {
  shiny::tagList(
    shiny::tags$br(),
    shiny::textInput(shiny::NS(id, "subm_name"), "Name"),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::actionButton(shiny::NS(id, "edit_subm_address"), "Edit contact details"),
                    shiny::actionButton(shiny::NS(id, "rm_subm_address"), "Remove contact details")
      ),
      shiny::column(6,
                    form_address("edit_subm_address", id),
                    shiny::textOutput(shiny::NS(id, "subm_addr_out"))
      )
      
      
      
    )
    
    #subm notes, media links
    
  )
}

file_submitterServer <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    subm <- reactive(tidyged::xrefs_subm(ged()))
    
    subm_rec <- reactive(dplyr::filter(ged(), record == subm()))
    
    observeEvent(ged(), {
      
      shiny::updateTextInput(session = session, "subm_name", 
                             value = tidyged.internals::gedcom_value(subm_rec(), subm(), "NAME", 1))
      
      shiny::updateTextInput(session = session, "adr1", 
                             value = tidyged.internals::gedcom_value(subm_rec(), subm(), "ADR1", 2))
      shiny::updateTextInput(session = session, "adr2", 
                             value = tidyged.internals::gedcom_value(subm_rec(), subm(), "ADR2", 2))
      shiny::updateTextInput(session = session, "adr3", 
                             value = tidyged.internals::gedcom_value(subm_rec(), subm(), "ADR3", 2))
      shiny::updateTextInput(session = session, "city", 
                             value = tidyged.internals::gedcom_value(subm_rec(), subm(), "CITY", 2))
      shiny::updateTextInput(session = session, "state", 
                             value = tidyged.internals::gedcom_value(subm_rec(), subm(), "STAE", 2))
      shiny::updateTextInput(session = session, "postcode", 
                             value = tidyged.internals::gedcom_value(subm_rec(), subm(), "POST", 2))
      shiny::updateTextInput(session = session, "country", 
                             value = tidyged.internals::gedcom_value(subm_rec(), subm(), "CTRY", 2))
      shiny::updateTextAreaInput(session = session, "emails", 
                             value = dplyr::filter(subm_rec(), tag == "EMAIL")$value %>% 
                               paste(collapse = "\n"))
      shiny::updateTextAreaInput(session = session, "phones", 
                                 value = dplyr::filter(subm_rec(), tag == "PHON")$value %>% 
                                   paste(collapse = "\n"))
      shiny::updateTextAreaInput(session = session, "faxes", 
                                 value = dplyr::filter(subm_rec(), tag == "FAX")$value %>% 
                                   paste(collapse = "\n"))
      shiny::updateTextAreaInput(session = session, "websites", 
                                 value = dplyr::filter(subm_rec(), tag == "WWW")$value %>% 
                                   paste(collapse = "\n"))
    })
    
    
    
  })
}

file_submitterApp <- function(ged = NULL) {
  ui <- fluidPage(
    file_submitterUI("file_submitter")
  )
  server <- function(input, output, session) {
    file_submitterServer("file_submitter", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}


