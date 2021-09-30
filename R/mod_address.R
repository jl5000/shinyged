
address_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::fluidRow(
      shiny::column(5,
                    shiny::htmlOutput(ns("address"), 
                                      style = "border-style:solid;border-width:thin;border-color:lightgrey;padding:12px")
                    ),
      shiny::column(2,
                    shiny::actionButton(ns("edit_address"), "Edit address")
                    )
    )
  )
  
}


address_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    addr <- shiny::reactive({
      req(r$ged, r[[section_rows]])
      r$addr_rows <- intersect(r[[section_rows]],
                             which(r$ged$tag %in% .pkgenv$tags_addr))
      r$ged[r$addr_rows,]
    })

    output$address <- shiny::renderUI({
      req(addr)
      if(nrow(addr()) > 1) {
        address <- dplyr::filter(addr(), tag %in% c("ADR1","ADR2","ADR3","CITY",
                                                    "STAE","POST","CTRY"))$value
        phones <- dplyr::filter(addr(), tag == "PHON")$value
        faxes <- dplyr::filter(addr(), tag == "FAX")$value
        emails <- dplyr::filter(addr(), tag == "EMAIL")$value
        websites <- dplyr::filter(addr(), tag == "WWW")$value
        
        details <- ""
        if(length(address) > 0) details <- paste0("<b>Address:</b><br>",paste(address,collapse="<br>"),"<br>")
        if(length(phones) > 0) details <- paste0(details,"<b>Phone numbers: </b>",paste(phones,collapse=", "),"<br>")
        if(length(faxes) > 0) details <- paste0(details,"<b>Fax numbers: </b>",paste(faxes,collapse=", "),"<br>")
        if(length(emails) > 0) details <- paste0(details,"<b>Email addresses: </b>",paste(emails,collapse=", "),"<br>")
        if(length(websites) > 0) details <- paste0(details,"<b>Websites: </b>",paste(websites,collapse=", "),"<br>")
        
      } else {
        details <- "None defined"
      }
      shiny::HTML(details)
    }) %>% shiny::bindEvent({input$apply
      r$file_count})

    shiny::observeEvent(input$edit_address, {
      req(r$ged)
      shiny::showModal(
        shiny::modalDialog(title = "Edit address",

                           shiny::fluidRow(
                             shiny::column(6,
                                           shiny::textAreaInput(ns("adr"), "Initial address lines (max 3 lines, no separator)",
                                                                dplyr::filter(addr(), tag %in% paste0("ADR",1:3))$value %>%
                                                                  paste(collapse = "\n"), rows = 3),
                                           shiny::textInput(ns("city"), "City",
                                                            dplyr::filter(addr(), tag == "CITY")$value),
                             ),
                             shiny::column(6,
                                           shiny::textInput(ns("state"), "State",
                                                            dplyr::filter(addr(), tag == "STAE")$value),
                                           shiny::textInput(ns("postcode"), "Postal code",
                                                            dplyr::filter(addr(), tag == "POST")$value),
                                           shiny::textInput(ns("country"), "Country",
                                                            dplyr::filter(addr(), tag == "CTRY")$value)
                             )

                           ),

                           shiny::helpText("Up to 3 separate phone numbers, FAX numbers, email addresses, and websites can be defined. ",
                                           "These must be defined on separate lines."),

                           shiny::fluidRow(
                             shiny::column(6,
                                           shiny::textAreaInput(ns("phones"), "Phone numbers", 
                                                                dplyr::filter(addr(), tag == "PHON")$value %>%
                                                                  paste(collapse = "\n"), rows = 3),
                                           shiny::textAreaInput(ns("emails"), "Email addresses", 
                                                                dplyr::filter(addr(), tag == "EMAIL")$value %>%
                                                                  paste(collapse = "\n"), rows = 3)
                             ),
                             shiny::column(6,
                                           shiny::textAreaInput(ns("faxes"), "Fax numbers", 
                                                                dplyr::filter(addr(), tag == "FAX")$value %>%
                                                                  paste(collapse = "\n"), rows = 3),
                                           shiny::textAreaInput(ns("websites"), "Websites", 
                                                                dplyr::filter(addr(), tag == "WWW")$value %>%
                                                                  paste(collapse = "\n"), rows = 3)
                             )
                           ),

                           footer = shiny::tagList(
                             shiny::actionButton(ns("apply"), "Apply")
                           )
        )
      )
    })
    
    shiny::observeEvent(input$apply, shiny::removeModal())

    shiny::observeEvent(input$adr, ignoreNULL = FALSE, ignoreInit = TRUE, {
      adr <- process_input(input$adr)
      err <- tidyged.internals::chk_address_lines(adr, 3)
      shinyFeedback::feedbackDanger("adr", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "addr_rows", addr()$level[1] + 1, paste0("ADR", 1:3), adr, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$city, ignoreNULL = FALSE, ignoreInit = TRUE, {
      city <- process_input(input$city)
      err <- tidyged.internals::chk_address_city(city, 1)
      shinyFeedback::feedbackDanger("city", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "addr_rows", addr()$level[1] + 1, "CITY", city, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$state, ignoreNULL = FALSE, ignoreInit = TRUE, {
      state <- process_input(input$state)
      err <- tidyged.internals::chk_address_state(state, 1)
      shinyFeedback::feedbackDanger("state", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "addr_rows", addr()$level[1] + 1, "STAE", state, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$postcode, ignoreNULL = FALSE, ignoreInit = TRUE, {
      postcode <- process_input(input$postcode)
      err <- tidyged.internals::chk_address_postal_code(postcode, 1)
      shinyFeedback::feedbackDanger("postcode", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "addr_rows", addr()$level[1] + 1, "POST", postcode, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$country, ignoreNULL = FALSE, ignoreInit = TRUE, {
      country <- process_input(input$country)
      err <- tidyged.internals::chk_address_country(country, 1)
      shinyFeedback::feedbackDanger("country", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "addr_rows", addr()$level[1] + 1, "CTRY", country, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$emails, ignoreNULL = FALSE, ignoreInit = TRUE, {
      emails <- process_input(input$emails)
      err <- tidyged.internals::chk_address_email(emails, 3)
      shinyFeedback::feedbackDanger("emails", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "addr_rows", addr()$level[1], "EMAIL", emails, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$phones, ignoreNULL = FALSE, ignoreInit = TRUE, {
      phones <- process_input(input$phones)
      err <- tidyged.internals::chk_phone_number(phones, 3)
      shinyFeedback::feedbackDanger("phones", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "addr_rows", addr()$level[1], "PHON", phones, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$faxes, ignoreNULL = FALSE, ignoreInit = TRUE, {
      faxes <- process_input(input$faxes)
      err <- tidyged.internals::chk_address_fax(faxes, 3)
      shinyFeedback::feedbackDanger("faxes", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "addr_rows", addr()$level[1], "FAX", faxes, .pkgenv$tags_addr)
    })
    shiny::observeEvent(input$websites, ignoreNULL = FALSE, ignoreInit = TRUE, {
      websites <- process_input(input$websites)
      err <- tidyged.internals::chk_address_web_page(websites, 3)
      shinyFeedback::feedbackDanger("websites", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "addr_rows", addr()$level[1], "WWW", websites, .pkgenv$tags_addr)
    })
    
  })
}


