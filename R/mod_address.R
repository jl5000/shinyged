
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


address_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    
    tag_order <- c("ADDR",paste0("ADR",1:3),"CITY","STAE","POST",
                   "CTRY","EMAIL","PHON","FAX","WWW")
    
    section <- shiny::reactiveValues(rows = section_rows, auto_update = TRUE)
    
    addr <- shiny::reactive({
      req(r$ged, section$rows)
      r$ged[section$rows,]
    })
    
    base_level <- shiny::reactive({
      req(addr)
      addr()$level[1]
    })
    
    observeEvent(addr(), {
      print(addr())
      # Only update on initilisation
      if(section$auto_update) {
        shiny::updateTextAreaInput(session = session, "adr",
                                   value = dplyr::filter(addr(), tag %in% paste0("ADR",1:3))$value %>%
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
      }
      section$auto_update = FALSE
    })

    
    shiny::observeEvent(input$adr, ignoreNULL = FALSE, ignoreInit = TRUE, {
      adr <- split_multiline(input$adr, 3)
      dummy <- update_ged_value(r, section, tag_order, base_level() + 1, paste0("ADR", 1:3), adr)
    })
    shiny::observeEvent(input$city, ignoreNULL = FALSE, ignoreInit = TRUE, {
      dummy <- update_ged_value(r, section, tag_order, base_level() + 1, "CITY", input$city)
    })
    shiny::observeEvent(input$state, ignoreNULL = FALSE, ignoreInit = TRUE, {
      dummy <- update_ged_value(r, section, tag_order, base_level() + 1, "STAE", input$state)
    })
    shiny::observeEvent(input$postcode, ignoreNULL = FALSE, ignoreInit = TRUE, {
      dummy <- update_ged_value(r, section, tag_order, base_level() + 1, "POST", input$postcode)
    })
    shiny::observeEvent(input$country, ignoreNULL = FALSE, ignoreInit = TRUE, {
      dummy <- update_ged_value(r, section, tag_order, base_level() + 1, "CTRY", input$country)
    })
    shiny::observeEvent(input$emails, ignoreNULL = FALSE, ignoreInit = TRUE, {
      emails <- split_multiline(input$emails, 3)
      dummy <- update_ged_value(r, section, tag_order, base_level(), "EMAIL", emails)
    })
    shiny::observeEvent(input$phones, ignoreNULL = FALSE, ignoreInit = TRUE, {
      phones <- split_multiline(input$phones, 3)
      dummy <- update_ged_value(r, section, tag_order, base_level(), "PHON", phones)
    })
    shiny::observeEvent(input$faxes, ignoreNULL = FALSE, ignoreInit = TRUE, {
      faxes <- split_multiline(input$faxes, 3)
      dummy <- update_ged_value(r, section, tag_order, base_level(), "FAX", faxes)
    })
    shiny::observeEvent(input$websites, ignoreNULL = FALSE, ignoreInit = TRUE, {
      websites <- split_multiline(input$websites, 3)
      dummy <- update_ged_value(r, section, tag_order, base_level(), "WWW", websites)
    })
    
  })
}


address_app <- function(ged = NULL, section_rows = NULL) {
  r <- shiny::reactiveValues(ged = ged)
  ui <- shiny::fluidPage(
    address_ui("address")
  )
  server <- function(input, output, session) {
    address_server("address", r, section_rows)
  }
  shiny::shinyApp(ui, server)  
}
