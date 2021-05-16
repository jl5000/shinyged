
address_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::fluidRow(
      shiny::column(6,
                    shiny::textAreaInput(ns("adr"), "Initial address lines (max 3 lines, no separator)", height = "75px"),
                    shiny::textInput(ns("city"), "City"),
      ),
      shiny::column(6,
                    shiny::textInput(ns("state"), "State"),
                    shiny::textInput(ns("postcode"), "Postal code"),
                    shiny::textInput(ns("country"), "Country")
      )
      
    ),
    
    shiny::helpText("Up to 3 separate phone numbers, FAX numbers, email addresses, and websites can be defined. ",
                    "These must be defined on separate lines."),
    
    shiny::fluidRow(
      shiny::column(6, #paste(phones, collapse = "\n")
                    shiny::textAreaInput(ns("phones"), "Phone numbers", height = "75px"),
                    shiny::textAreaInput(ns("emails"), "Email addresses", height = "75px")
      ),
      shiny::column(6,
                    shiny::textAreaInput(ns("faxes"), "Fax numbers", height = "75px"),
                    shiny::textAreaInput(ns("websites"), "Websites", height = "75px")
      )
    )
  )
  
  
}


address_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    addr <- shiny::reactive({
      req(r$ged, r$section_rows)
      dplyr::slice(r$ged, r$section_rows) %>%
        dplyr::filter(tag %in% c("ADDR",paste0("ADR",1:3),"CITY","STAE","POST",
                                 "CTRY","EMAIL","PHON","FAX","WWW"))
    })
    
    observeEvent(addr(), {
      shiny::updateTextAreaInput(session = session, "adr", 
                                 value = dplyr::filter(addr(), tag %in% paste0("ADR",1:3))$value %>% 
                                   sort() %>% 
                                   paste(collapse = "\n"))
      shiny::updateTextInput(session = session, "city", 
                             value = dplyr::filter(addr(), tag == "CITY")$value)
      shiny::updateTextInput(session = session, "state", 
                             value = dplyr::filter(addr(), tag == "STAE")$value)
      shiny::updateTextInput(session = session, "postcode", 
                             value = dplyr::filter(addr(), tag == "POST")$value)
      shiny::updateTextInput(session = session, "country", 
                             value = dplyr::filter(addr(), tag == "CTRY")$value)
      shiny::updateTextAreaInput(session = session, "emails", 
                                 value = dplyr::filter(addr(), tag == "EMAIL")$value %>% 
                                   paste(collapse = "\n"))
      shiny::updateTextAreaInput(session = session, "phones", 
                                 value = dplyr::filter(addr(), tag == "PHON")$value %>% 
                                   paste(collapse = "\n"))
      shiny::updateTextAreaInput(session = session, "faxes", 
                                 value = dplyr::filter(addr(), tag == "FAX")$value %>% 
                                   paste(collapse = "\n"))
      shiny::updateTextAreaInput(session = session, "websites", 
                                 value = dplyr::filter(addr(), tag == "WWW")$value %>% 
                                   paste(collapse = "\n"))
    })

    
  })
}


address_app <- function(addr = NULL) {
  r <- shiny::reactiveValues(ged = ged, section_rows = section_rows)
  ui <- shiny::fluidPage(
    address_ui("address")
  )
  server <- function(input, output, session) {
    address_server("address", r)
  }
  shiny::shinyApp(ui, server)  
}
