


citations_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::br(),
    shiny::helpText("Here you can manage citations associated with an item.",
                    "Citations are links to sources that provide evidence for the item."),
    shiny::tags$hr(),
    DT::DTOutput(ns("table")),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(8,
                    shiny::actionButton(ns("add_citation"), "Add citation"),
                    shiny::actionButton(ns("remove_citation"), "Delete citation"),
                    shiny::actionButton(ns("update_citation"), "Update citation")
      ),
      shiny::column(1),
      shiny::column(3,
                    notes_ui(ns("citation_notes")),
                    media_links_ui(ns("citation_media")),
      )
    ),
    
    shiny::tags$hr(),
    
    shiny::fluidRow(id = ns("citation_section"),
                    shiny::column(12,
                                  shiny::fluidRow(
                                    shiny::column(6,
                                                  shiny::textInput(ns("page"), "Where within source?")
                                    ),
                                    shiny::column(6,
                                                  shiny::selectizeInput(ns("event_type"), 
                                                                        label = "Event type", 
                                                                        choices = unique_facts(),
                                                                        multiple = TRUE, options = list(maxItems = 1))
                                    )
                                  ),
                                  shiny::fluidRow(
                                    shiny::column(6, 
                                                  shiny::selectizeInput(ns("role"), 
                                                                        label = "Role in event",
                                                                        choices = c(tidyged.internals::val_roles(), Other = "Other"),
                                                                        multiple = TRUE, 
                                                                        options = list(maxItems = 1))
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
    ) %>% shinyjs::hidden()
    
    
    
  )
}


citations_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    notes_server("citation_notes", r, "citation_rows")
    media_links_server("citation_media", r, "citation_rows")
    valid_cit <- shiny::reactiveValues(input = list())

    # The list of rows in the tidyged object for each citation ----------------
    citations_rows <- shiny::reactive({
      req(r$ged, r[[section_rows]])
      
      rows_vect <- tidyged.internals::identify_section(r$ged,
                                                       containing_level = r$ged$level[r[[section_rows]]][1] + 1,
                                                       containing_tags = "SOUR",
                                                       xrefs = r$ged$record[r[[section_rows]]][1])
      
      split(rows_vect, cumsum(r$ged$tag[rows_vect] == "SOUR"))
      
    })
    
    # The table of citations -------------------------------------------------
    cit_table <- shiny::reactive({
      req(r$ged, citations_rows)
      
      if(length(citations_rows()) == 0) return(NULL)
      
      rows <- as.integer(unlist(citations_rows()))
      
      titles <- sapply(citations_rows(), `[`, 1) %>% 
        dplyr::slice(r$ged, .) %>% 
        dplyr::pull(value) %>% 
        sapply(tidyged.internals::gedcom_value, gedcom = r$ged, tag = "TITL", level = 1)
      
      cit_df <- r$ged %>%
        dplyr::slice(rows) %>% 
        dplyr::select(tag, value) %>% 
        dplyr::filter(tag %in% c("SOUR","PAGE","QUAY")) %>% 
        dplyr::mutate(id1 = cumsum(tag == "SOUR")) %>% 
        as.data.frame() %>% 
        reshape(direction = "wide", idvar = "id1", v.names = "value", timevar = "tag") %>% 
        dplyr::select(-id1) %>% 
        dplyr::mutate(titl = titles, .after = 1)
      
      if(!"value.PAGE" %in% names(cit_df)) cit_df <- dplyr::mutate(cit_df, C = "", .after = 2)
      if(!"value.QUAY" %in% names(cit_df)) cit_df <- dplyr::mutate(cit_df, D = "", .after = 3)
      names(cit_df) <- LETTERS[1:4]
      cit_df[is.na(cit_df)] <- ""
      
      cit_df
    })
    
    
    # Show the dataframe of citations ------------------------------
    output$table <- DT::renderDataTable({
      req(cit_table)
      
      DT::datatable(cit_table(), rownames = FALSE, selection = list(mode = "single", selected = r$cit_to_select),
                    filter = "none", colnames = c("Source", "Title", "Where in source?", "Certainty"),
                    options = list(searching = FALSE, paging = FALSE))
    })
    
  
    # Show dialog to choose a source record ---------------------------------
    shiny::observe({
      req(r$ged)
      
      shiny::modalDialog(
        shiny::selectizeInput(ns("source_select"), label = "Choose a source...",
                              choices = tidyged::describe_records(r$ged, tidyged::xrefs_sour(r$ged), short_desc = TRUE),
                              multiple = TRUE, options = list(maxItems = 1), width = "500px"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("add_sour_citation"), "Add source citation")
        )
      ) %>% shiny::showModal()
      
    }) %>%
      shiny::bindEvent(input$add_citation)
    
    # Disable add_sour_citation button if nothing selected --------------------
    shiny::observe({
      shinyjs::toggleState("add_sour_citation", !is.null(input$source_select))
    }) %>%
      shiny::bindEvent(input$source_select, ignoreNULL = FALSE)
    
    # Add source citation ------------------------------------------------------
    shiny::observe({
      sour_xref <- stringr::str_extract(input$source_select, tidyged.internals::reg_xref(FALSE))
      
      r$ged <- tibble::add_row(r$ged,
                               tibble::tibble(record = r$ged$record[r[[section_rows]][1]],
                                              level = r$ged$level[r[[section_rows]][1]] + 1,
                                              tag = "SOUR",
                                              value = sour_xref),
                               # Need to insert new citations after final citation so it
                               # doesn't shift existing row numbers
                               .after = max(r[[section_rows]]))
      
      r$cit_to_select <- nrow(cit_table())
      shiny::removeModal()
    }) %>%
      shiny::bindEvent(input$add_sour_citation)
    
    # Select a citation ---------------------------------------------
    shiny::observe({
      shinyjs::toggle("citation_section", condition = !is.null(input$table_rows_selected))
      shinyjs::toggleState("remove_citation", !is.null(input$table_rows_selected))
      r$cit_to_select <- input$table_rows_selected
      
      #req(citations_rows, input$table_rows_selected)
      if(is.null(input$table_rows_selected)){
        r$citation_rows <- NULL
      } else {
        r$citation_rows <- citations_rows()[[input$table_rows_selected]]
        citation <- r$ged[r$citation_rows,]
        
        shiny::updateTextInput(inputId = "page", 
                               value = tidyged.internals::gedcom_value(citation, 
                                                                       citation$record[1], "PAGE", 
                                                                       citation$level[1] + 1))
        shiny::updateTextInput(inputId = "entry_date",
                               value = tidyged.internals::gedcom_value(citation, 
                                                                       citation$record[1], "DATE", 
                                                                       citation$level[1] + 2))
        shiny::updateSelectizeInput(inputId = "event_type",
                                    selected = tidyged.internals::gedcom_value(citation, 
                                                                               citation$record[1], "EVEN", 
                                                                               citation$level[1] + 1))
        
        role <- tidyged.internals::gedcom_value(citation, 
                                                citation$record[1], "ROLE", 
                                                citation$level[1] + 2)
        
        if(role == "" | role %in% tidyged.internals::val_roles()) {
          shiny::updateSelectizeInput(inputId = "role",  selected = role)
          shiny::updateTextInput(inputId = "custom_role",  value = "")
        } else { #custom role
          shiny::updateSelectizeInput(inputId = "role",  selected = "Other")
          shiny::updateTextInput(inputId = "custom_role",  value = stringr::str_sub(role, 2, -2))
        }
        
        shiny::updateTextAreaInput(inputId = "source_text",
                                   value = tidyged.internals::gedcom_value(citation, 
                                                                           citation$record[1], "TEXT", 
                                                                           citation$level[1] + 2))
        shiny::updateTextInput(inputId = "certainty",
                               value = tidyged.internals::gedcom_value(citation, 
                                                                       citation$record[1], "QUAY", 
                                                                       citation$level[1] + 1))
        
      }
      
    }) %>%
      shiny::bindEvent(input$table_rows_selected, ignoreNULL = FALSE)
    
    # Enable/disable role inputs ------------------------------------------------
    shiny::observe({
      shinyjs::toggleState("role", !is.null(input$event_type))
      shinyjs::toggleState("custom_role", !is.null(input$role) && input$role == "Other")
      shinyjs::toggleState("update_citation", !is.null(input$table_rows_selected) &&
                             all(unlist(valid_cit$input)))
    })
    
    # Validate page ----------------------------------------------------
    shiny::observe({
      page <- process_input(input$page)
      err <- tidyged.internals::chk_where_within_source(page, 1)
      shinyFeedback::feedbackDanger("page", !is.null(err), err)
      valid_cit$input$page <- is.null(err)
    }) %>% 
      shiny::bindEvent(input$page, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Validate entry date ----------------------------------------------------
    shiny::observe({
      entry_date <- process_input(input$entry_date)
      err <- tidyged.internals::chk_date_value(entry_date, 1)
      shinyFeedback::feedbackDanger("entry_date", !is.null(err), err)
      valid_cit$input$date <- is.null(err)
    }) %>% 
      shiny::bindEvent(input$entry_date, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Validate event type / role ---------------------------------------------
    shiny::observe({
      # some events have had a space added to make them unique
      event_type <- process_input(input$event_type) %>% stringr::str_trim()
      err <- tidyged.internals::chk_event_type_cited_from(event_type, 1)
      err1 <- is.null(input$event_type) & !is.null(input$role)
      shinyFeedback::feedbackDanger("event_type", !is.null(err), err)
      shinyFeedback::feedbackDanger("role", err1, "Event type is required for this input")
      valid_cit$input$event <- is.null(err)
      valid_cit$input$role <- is.null(err1)
    }) %>% 
      shiny::bindEvent(input$event_type, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Validate role -----------------------------------------------------------
    shiny::observe({
      role <- process_input(input$role)
      if(!is.null(input$role) && input$role == "Other") {
        role <- paste0("(", input$custom_role, ")")
      }
      err <- tidyged.internals::chk_role_in_event(role, 1)
      if(!is.null(input$role) && input$role == "Other") {
        shinyFeedback::feedbackWarning("custom_role", !is.null(err), "Enter a custom role")
        valid_cit$input$custom_role <- is.null(err)
      } else {
        shinyFeedback::feedbackDanger("role", !is.null(err), err)
        valid_cit$input$role <- is.null(err)
      }
    }) %>% 
      shiny::bindEvent(input$role, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Validate custom role ----------------------------------------------------
    shiny::observe({
      custom_role <- process_input(input$custom_role)
      if(!is.null(input$role) && input$role == "Other") {
        role <- paste0("(", input$custom_role, ")")
      }
      err <- tidyged.internals::chk_role_in_event(custom_role, 1)
      shinyFeedback::feedbackWarning("custom_role", !is.null(err), "Enter a custom role")
      valid_cit$input$custom_role <- is.null(err)
    }) %>% 
      shiny::bindEvent(input$custom_role, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Validate source text -----------------------------------------------------
    shiny::observe({
      source_text <- process_input(input$source_text)
      err <- tidyged.internals::chk_text_from_source(source_text, 1)
      shinyFeedback::feedbackDanger("source_text", !is.null(err), err)
      valid_cit$input$text <- is.null(err)
    }) %>% 
      shiny::bindEvent(input$source_text, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Validate certainty --------------------------------------------------------
    shiny::observe({
      certainty <- process_input(input$certainty)
      err <- tidyged.internals::chk_certainty_assessment(certainty, 1)
      shinyFeedback::feedbackDanger("certainty", !is.null(err), err)
      valid_cit$input$certainty <- is.null(err)
    }) %>% 
      shiny::bindEvent(input$certainty, ignoreNULL = FALSE, ignoreInit = TRUE)

    
    # Remove citation -------------------------------------------------------------
    shiny::observe({
      r$ged <- dplyr::slice(r$ged, -r$citation_rows)
      r$cit_to_select <- NULL
    }) %>%
      shiny::bindEvent(input$remove_citation)
    
    # Update citation -------------------------------------------------------------
    shiny::observe({
      cit <- r$ged[r$citation_rows,]
      
      #event <- process_input(input$event_type) %>% stringr::str_trim()
      
      cert <- process_input(input$certainty)
      cert_val <- switch(cert, 
                         "0" = "unreliable",
                         "1" = "subjective",
                         "2" = "secondary",
                         "3" = "primary")
      
      notes <- dplyr::filter(cit, tag == "NOTE")$value
      media <- dplyr::filter(cit, tag == "OBJE")$value
      
      cit_structure <- tidyged::source_citation(r$ged, cit$value[1], 
                                                process_input(input$page),
                                                #event, role,#TODO
                                                process_input(input$entry_date), 
                                                process_input(input$source_text),
                                                cert_val, notes, media) %>% 
        dplyr::mutate(record = cit$record[1], level = level + cit$level[1])
      
      r$ged <- r$ged %>% 
        dplyr::slice(-r$citation_rows) %>% 
        dplyr::add_row(cit_structure, .before = r$citation_rows[1])
      
    }) %>%
      shiny::bindEvent(input$update_citation)
    
  })
}
