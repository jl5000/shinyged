


citation_details_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::textInput(ns("page"), "Where within source?")
                    ),
      shiny::column(6,
                    shiny::selectizeInput(ns("event_type"), label = "Event type", 
                                          choices = unique_facts(),
                                          multiple = TRUE, options = list(maxItems = 1))
      )
    ),
    shiny::fluidRow(
      shiny::column(6, 
                    shiny::selectizeInput(ns("role"), "Role in event",
                                          choices = c(tidyged.internals::val_roles(), Other = "Other"),
                                          multiple = TRUE, options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::textInput(ns("custom_role"), "Custom role")
                    )
    ),
    shiny::fluidRow(
      shiny::column(12, 
                    shiny::textAreaInput(ns("source_text"), "Source text", resize = "vertical") %>%
                      shiny::tagAppendAttributes(style = 'width: 85%;')
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::textInput(ns("certainty"), "Certainty assessment")
      ),
      shiny::column(6,
                    shiny::textInput(ns("entry_date"), "Entry date (e.g. 6 APR 1983)")
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::helpText("What is the credibility of this source?",
                                    shiny::br(),
                                    "0 = unreliable/estimated data",
                                    shiny::br(),
                                    "1 = Questionable reliability of evidence",
                                    shiny::br(),
                                    "2 = Secondary evidence, officially recorded sometime after event",
                                    shiny::br(),
                                    "3 = Direct and primary evidence used / dominance of evidence")
      )
    )
  )
  
  
}


citation_details_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(r$ged, priority = 2, {
      req(r$ged, r$citation_rows)
      r$citation_rows <- tidyged.internals::identify_section(r$ged,
                                                             r$ged$level[r$citation_rows[1]],
                                                             "SOUR",
                                                             r$ged$value[r$citation_rows[1]],
                                                             r$ged$record[r$citation_rows[1]],
                                                             first_only = TRUE)
    })
    
    sour_xref <- shiny::reactive({
      req(r$ged, r$citation_rows)
      r$ged$value[r$citation_rows[1]]
    })
    
    shiny::observeEvent(sour_xref(), once = TRUE, {
      
      shiny::updateTextInput(session = session, "page", 
                             value = tidyged.internals::gedcom_value(r$ged[r$citation_rows,], 
                                                                     r$ged$record[r$citation_rows[1]], "PAGE", 
                                                                     r$ged$level[r$citation_rows[1]] + 1))
      shiny::updateTextInput(session = session, "entry_date",
                             value = tidyged.internals::gedcom_value(r$ged[r$citation_rows,], 
                                                                     r$ged$record[r$citation_rows[1]], "DATE", 
                                                                     r$ged$level[r$citation_rows[1]] + 2))
      shiny::updateSelectizeInput(session = session, "event_type",
                                  selected = tidyged.internals::gedcom_value(r$ged[r$citation_rows,], 
                                                                             r$ged$record[r$citation_rows[1]], "EVEN", 
                                                                             r$ged$level[r$citation_rows[1]] + 1))
      
      role <- tidyged.internals::gedcom_value(r$ged[r$citation_rows,], 
                                              r$ged$record[r$citation_rows[1]], "ROLE", 
                                              r$ged$level[r$citation_rows[1]] + 2)

      if(role == "" | role %in% tidyged.internals::val_roles()) {
        shiny::updateSelectizeInput(session = session, "role",  selected = role)
        shiny::updateTextInput(session = session, "custom_role",  value = "")
      } else { #custom role
        shiny::updateSelectizeInput(session = session, "role",  selected = "Other")
        shiny::updateTextInput(session = session, "custom_role",  value = stringr::str_sub(role, 2, -2))
      }
      
      shiny::updateTextAreaInput(session = session, "source_text",
                                 value = tidyged.internals::gedcom_value(r$ged[r$citation_rows,], 
                                                                         r$ged$record[r$citation_rows[1]], "TEXT", 
                                                                         r$ged$level[r$citation_rows[1]] + 2))
      shiny::updateTextInput(session = session, "certainty",
                                value = tidyged.internals::gedcom_value(r$ged[r$citation_rows,], 
                                                                        r$ged$record[r$citation_rows[1]], "QUAY", 
                                                                        r$ged$level[r$citation_rows[1]] + 1))
    })
    
    shiny::observeEvent(input$event_type, ignoreNULL = FALSE, {
      shinyjs::toggleState("role", !is.null(input$event_type))
      shinyjs::toggleState("custom_role", !is.null(input$role) && input$role == "Custom")
    })
    
    shiny::observeEvent(input$page, ignoreNULL = FALSE, ignoreInit = TRUE, {
      page <- process_input(input$page)
      err <- tidyged.internals::chk_where_within_source(page, 1)
      shinyFeedback::feedbackDanger("page", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "citation_rows", r$ged$level[r$citation_rows[1]] + 1, 
                                "PAGE", page, .pkgenv$tags_sour_cit)
    })
    
    shiny::observeEvent(input$entry_date, ignoreNULL = FALSE, ignoreInit = TRUE, {
      entry_date <- process_input(input$entry_date)
      err <- tidyged.internals::chk_date_value(entry_date, 1)
      shinyFeedback::feedbackDanger("entry_date", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "citation_rows", r$ged$level[r$citation_rows[1]] + 2, 
                                "DATE", entry_date, .pkgenv$tags_sour_cit)
    })
    
    shiny::observeEvent(input$event_type, ignoreNULL = FALSE, ignoreInit = TRUE, {
      event_type <- process_input(input$event_type) %>% stringr::str_trim()
      err <- tidyged.internals::chk_event_type_cited_from(event_type, 1)
      err1 <- is.null(input$event_type) & !is.null(input$role)
      shinyFeedback::feedbackDanger("event_type", !is.null(err), err)
      shinyFeedback::feedbackDanger("role", err1, "Event type is required for this input")
      req(is.null(err), !err1, cancelOutput = TRUE)
      update_ged_value(r, "citation_rows", r$ged$level[r$citation_rows[1]] + 1, 
                       "EVEN", event_type, .pkgenv$tags_sour_cit)
    })
    
    shiny::observeEvent(input$role, ignoreNULL = FALSE, ignoreInit = TRUE, { # finish this
      role <- process_input(input$role)
      shinyjs::toggleState("custom_role", !is.null(input$role) && input$role == "Custom")
      if(input$role == "Other") {
        role <- paste0("(", input$custom_role, ")")
      }
      err <- tidyged.internals::chk_role_in_event(role, 1)
      if(input$role == "Other") {
        shinyFeedback::feedbackWarning("custom_role", !is.null(err), "Enter a custom role")
      } else {
        shinyFeedback::feedbackDanger("role", !is.null(err), err)
      }
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "citation_rows", r$ged$level[r$citation_rows[1]] + 2, 
                       "ROLE", role, .pkgenv$tags_sour_cit)
    })
    
    shiny::observeEvent(input$custom_role, ignoreNULL = FALSE, ignoreInit = TRUE, { # finish this
      custom_role <- process_input(input$custom_role)
      custom_role <- paste0("(", custom_role, ")")
      err <- tidyged.internals::chk_role_in_event(custom_role, 1)
      shinyFeedback::feedbackWarning("custom_role", !is.null(err), "Enter a custom role")
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "citation_rows", r$ged$level[r$citation_rows[1]] + 2, 
                       "ROLE", custom_role, .pkgenv$tags_sour_cit)
    })
    
    shiny::observeEvent(input$source_text, ignoreNULL = FALSE, ignoreInit = TRUE, {
      source_text <- process_input(input$source_text)
      err <- tidyged.internals::chk_text_from_source(source_text, 1)
      shinyFeedback::feedbackDanger("source_text", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "citation_rows", r$ged$level[r$citation_rows[1]] + 2, 
                       "TEXT", source_text, .pkgenv$tags_sour_cit)
    })
    
    shiny::observeEvent(input$certainty, ignoreNULL = FALSE, ignoreInit = TRUE, {
      certainty <- process_input(input$certainty)
      err <- tidyged.internals::chk_certainty_assessment(certainty, 1)
      shinyFeedback::feedbackDanger("certainty", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "citation_rows", r$ged$level[r$citation_rows[1]] + 1, 
                                "QUAY", certainty, .pkgenv$tags_sour_cit)
    })
    
    
    
  })
}

