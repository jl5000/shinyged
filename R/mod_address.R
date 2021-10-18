
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
    

    # The address dataframe ---------------------------------------------------   
    addr <- shiny::reactive({
      req(r$ged, r[[section_rows]])

      r$addr_rows <- intersect(r[[section_rows]],
                             which(r$ged$tag %in% .pkgenv$tags_addr))
      r$ged[r$addr_rows,]
    })


    # Address output ----------------------------------------------------------
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
    }) %>% shiny::bindEvent(input$apply, r$file_count)


    # Open modal to edit address ----------------------------------------------
    shiny::observe({
      req(r$ged)
      
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
                         
      ) %>% shiny::showModal()
    
    }) %>% 
      shiny::bindEvent(input$edit_address)
    

    # Edit address lines ------------------------------------------------------
    shiny::observe({
      adr <- process_input(input$adr)
      err <- tidyged.internals::chk_address_lines(adr, 3)
      shinyFeedback::feedbackDanger("adr", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, section_rows, r$ged$record[r[[section_rows]][1]],
                       r$ged$level[r[[section_rows]][1]] + 2, paste0("ADR", 1:3), adr)
    }) %>% 
      shiny::bindEvent(input$adr, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit city ----------------------------------------------------------------
    shiny::observe({
      city <- process_input(input$city)
      err <- tidyged.internals::chk_address_city(city, 1)
      shinyFeedback::feedbackDanger("city", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, section_rows, r$ged$record[r[[section_rows]][1]],
                       r$ged$level[r[[section_rows]][1]] + 2, "CITY", city)
    }) %>% 
      shiny::bindEvent(input$city, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit state --------------------------------------------------------------
    shiny::observe({
      state <- process_input(input$state)
      err <- tidyged.internals::chk_address_state(state, 1)
      shinyFeedback::feedbackDanger("state", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, section_rows, r$ged$record[r[[section_rows]][1]],
                       r$ged$level[r[[section_rows]][1]] + 2, "STAE", state)
    }) %>% 
      shiny::bindEvent(input$state, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit postcode -----------------------------------------------------------
    shiny::observe({
      postcode <- process_input(input$postcode)
      err <- tidyged.internals::chk_address_postal_code(postcode, 1)
      shinyFeedback::feedbackDanger("postcode", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, section_rows, r$ged$record[r[[section_rows]][1]],
                       r$ged$level[r[[section_rows]][1]] + 2, "POST", postcode)
    }) %>% 
      shiny::bindEvent(input$postcode, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit country -----------------------------------------------------------
    shiny::observe({
      country <- process_input(input$country)
      err <- tidyged.internals::chk_address_country(country, 1)
      shinyFeedback::feedbackDanger("country", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, section_rows, r$ged$record[r[[section_rows]][1]],
                       r$ged$level[r[[section_rows]][1]] + 2, "CTRY", country)
    }) %>% 
      shiny::bindEvent(input$country, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit emails ------------------------------------------------------------
    shiny::observe({
      emails <- process_input(input$emails)
      err <- tidyged.internals::chk_address_email(emails, 3)
      shinyFeedback::feedbackDanger("emails", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, section_rows, r$ged$record[r[[section_rows]][1]],
                       r$ged$level[r[[section_rows]][1]] + 1, "EMAIL", emails)
    }) %>% 
      shiny::bindEvent(input$emails, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit phone numbers ------------------------------------------------------
    shiny::observe({
      phones <- process_input(input$phones)
      err <- tidyged.internals::chk_phone_number(phones, 3)
      shinyFeedback::feedbackDanger("phones", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, section_rows, r$ged$record[r[[section_rows]][1]],
                       r$ged$level[r[[section_rows]][1]] + 1, "PHON", phones)
    }) %>% 
      shiny::bindEvent(input$phones, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit faxes --------------------------------------------------------------
    shiny::observe({
      faxes <- process_input(input$faxes)
      err <- tidyged.internals::chk_address_fax(faxes, 3)
      shinyFeedback::feedbackDanger("faxes", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, section_rows, r$ged$record[r[[section_rows]][1]],
                       r$ged$level[r[[section_rows]][1]] + 1, "FAX", faxes)
    }) %>% 
      shiny::bindEvent(input$faxes, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Edit websites -----------------------------------------------------------
    shiny::observe({
      websites <- process_input(input$websites)
      err <- tidyged.internals::chk_address_web_page(websites, 3)
      shinyFeedback::feedbackDanger("websites", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, section_rows, r$ged$record[r[[section_rows]][1]],
                       r$ged$level[r[[section_rows]][1]] + 1, "WWW", websites)
    }) %>% 
      shiny::bindEvent(input$websites, ignoreNULL = FALSE, ignoreInit = TRUE)
    
  })
}

