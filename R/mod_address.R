
address_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::fluidRow(
      shiny::column(6,
                    shiny::textAreaInput(ns("adr"), "Initial address lines (max 3 lines, no separator)", rows = 3),
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
      shiny::column(6,
                    shiny::textAreaInput(ns("phones"), "Phone numbers", rows = 3),
                    shiny::textAreaInput(ns("emails"), "Email addresses", rows = 3)
      ),
      shiny::column(6,
                    shiny::textAreaInput(ns("faxes"), "Fax numbers", rows = 3),
                    shiny::textAreaInput(ns("websites"), "Websites", rows = 3)
      )
    )
  )
  
  
}


address_server <- function(id, r, addr_rows) {
  moduleServer(id, function(input, output, session) {

    addr <- shiny::reactive({
      req(r$ged, r[[addr_rows]])
      r$ged[r[[addr_rows]],]
    })
    
    shiny::observeEvent(addr(), {
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
      
    })

    
    shiny::observeEvent(input$adr, ignoreNULL = FALSE, ignoreInit = TRUE, {
      adr <- process_input(input$adr)
      err <- tidyged.internals::chk_address_lines(adr, 3)
      shinyFeedback::feedbackDanger("adr", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, addr_rows, addr()$level[1] + 1, paste0("ADR", 1:3), adr, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$city, ignoreNULL = FALSE, ignoreInit = TRUE, {
      city <- process_input(input$city)
      err <- tidyged.internals::chk_address_city(city, 1)
      shinyFeedback::feedbackDanger("city", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, addr_rows, addr()$level[1] + 1, "CITY", city, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$state, ignoreNULL = FALSE, ignoreInit = TRUE, {
      state <- process_input(input$state)
      err <- tidyged.internals::chk_address_state(state, 1)
      shinyFeedback::feedbackDanger("state", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, addr_rows, addr()$level[1] + 1, "STAE", state, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$postcode, ignoreNULL = FALSE, ignoreInit = TRUE, {
      postcode <- process_input(input$postcode)
      err <- tidyged.internals::chk_address_postal_code(postcode, 1)
      shinyFeedback::feedbackDanger("postcode", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, addr_rows, addr()$level[1] + 1, "POST", postcode, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$country, ignoreNULL = FALSE, ignoreInit = TRUE, {
      country <- process_input(input$country)
      err <- tidyged.internals::chk_address_country(country, 1)
      shinyFeedback::feedbackDanger("country", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, addr_rows, addr()$level[1] + 1, "CTRY", country, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$emails, ignoreNULL = FALSE, ignoreInit = TRUE, {
      emails <- process_input(input$emails)
      err <- tidyged.internals::chk_address_email(emails, 3)
      shinyFeedback::feedbackDanger("emails", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, addr_rows, addr()$level[1], "EMAIL", emails, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$phones, ignoreNULL = FALSE, ignoreInit = TRUE, {
      phones <- process_input(input$phones)
      err <- tidyged.internals::chk_phone_number(phones, 3)
      shinyFeedback::feedbackDanger("phones", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, addr_rows, addr()$level[1], "PHON", phones, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$faxes, ignoreNULL = FALSE, ignoreInit = TRUE, {
      faxes <- process_input(input$faxes)
      err <- tidyged.internals::chk_address_fax(faxes, 3)
      shinyFeedback::feedbackDanger("faxes", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, addr_rows, addr()$level[1], "FAX", faxes, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$websites, ignoreNULL = FALSE, ignoreInit = TRUE, {
      websites <- process_input(input$websites)
      err <- tidyged.internals::chk_address_web_page(websites, 3)
      shinyFeedback::feedbackDanger("websites", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, addr_rows, addr()$level[1], "WWW", websites, .pkgenv$tags_addr)
    })
    
  })
}


