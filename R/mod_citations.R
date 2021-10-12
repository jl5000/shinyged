


citations_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::actionButton(ns("citations"), label = NULL)
  )
}


citations_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Disable citations button if no source records to point to
    shiny::observeEvent(r$ged, {
      shinyjs::toggleState("citations", tidyged::num_sour(r$ged) > 0)
    })
    
    # Show modal
    shiny::observeEvent(input$citations, {
      show_citations_modal(ns, r)
    })
    
    
    # The list of rows in the tidyged object for each citation
    citations_rows <- shiny::reactive({
      req(r$ged, r[[section_rows]])

      rows_vect <- tidyged.internals::identify_section(r$ged,
                                                       containing_level = r$ged$level[r[[section_rows]]][1] + 1,
                                                       containing_tags = "SOUR",
                                                       xrefs = r$ged$record[r[[section_rows]]][1])

      split(rows_vect, cumsum(r$ged$tag[rows_vect] == "SOUR"))

    })
    
    # Update button label with number of citations
    shiny::observeEvent(citations_rows(), {
      req(citations_rows)

      lbl <- paste0(length(citations_rows()), " citations")
      if(length(citations_rows()) == 1) lbl <- substr(lbl, 1, nchar(lbl) - 1)
      shiny::updateActionButton(inputId = "citations", label = lbl)
    })

    # The vector of citation descriptions
    citations <- shiny::reactive({
      req(r$ged, citations_rows)
      
      if(length(citations_rows()) == 0) return(NULL)
      
      pages <- vapply(citations_rows(), function(rows) {ifelse("PAGE" %in% r$ged$tag[rows],
                                                               dplyr::slice(r$ged, rows) %>%
                                                                 dplyr::filter(tag == "PAGE") %>%
                                                                 dplyr::pull(value),
                                                               "")},
                      character(1),
                      USE.NAMES = FALSE)
      
      
      citations_rows() %>%
        sapply(`[[`, 1) %>%
        dplyr::slice(r$ged, .) %>%
        dplyr::pull(value) %>%
        sapply(tidyged::describe_records, gedcom = r$ged, 
               short_desc = TRUE, USE.NAMES = FALSE) %>% #describe_records removes duplicates!!
        # ifelse(pages == "", ., paste0(., " [", pages, "]")) %>%
        paste0(seq_along(.), ". ", .) # 1. 2. 3. etc.

    })
    #TODO: Try to get same citation selected when modal is returned to
    citation_to_select <- shiny::reactive({
      if(is.null(r$cit_to_select)) {
        input$citation
      } else {
        r$cit_to_select
      }
    })
    
    # Update choices with list of individuals and select one
    observe({
      r$cits <- citations()
      if(!is.null(citations())) {
        shiny::updateSelectizeInput(session = session, inputId = "citation", choices = citations(), 
                                    selected = citation_to_select())
      } else {
        shiny::updateSelectizeInput(session = session, inputId = "citation", choices = character(), 
                                    selected = character())
      }
      r$cit_to_select <- input$citation
    })

    # Update citation_rows with rows of selected citation
    shiny::observeEvent(input$citation, {
      req(input$citation, citations_rows, citations)
      
      r$citation_rows <- citations_rows()[[which(citations() == input$citation)]]
    })


    # Show/hide tabs and toggle delete button if no citation
    shiny::observeEvent(input$citation, ignoreNULL = FALSE, {
      shinyjs::toggle("citation_section", condition = !is.null(input$citation))
      shinyjs::toggleState("remove_citation", !is.null(input$citation))
    })
    
    # Show dialog to choose a source record
    shiny::observeEvent(input$add_citation, {
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
      
    })

    # Disable add_sour_citation button if no source records
    shiny::observeEvent(input$source_select, ignoreNULL = FALSE, {
      shinyjs::toggleState("add_sour_citation", !is.null(input$source_select))
    })

    # Add source citation
    shiny::observeEvent(input$add_sour_citation, {
      sour_xref <- stringr::str_extract(input$source_select, tidyged.internals::reg_xref(FALSE))

      r$ged <- tibble::add_row(r$ged,
                               tibble::tibble(record = r$ged$record[r[[section_rows]][1]],
                                              level = r$ged$level[r[[section_rows]][1]] + 1,
                                              tag = "SOUR",
                                              value = sour_xref),
                               # Need to insert new citations after final citation so it
                               # doesn't shift existing row numbers
                               .after = max(r[[section_rows]]))
      
      r$cit_to_select <- citations()[length(citations())]
      shiny::removeModal()
    })

    # Remove citation
    shiny::observeEvent(input$remove_citation, {
      r$ged <- dplyr::slice(r$ged, -r$citation_rows)
      r$cit_to_select <- NULL
    })
    
    shiny::observeEvent(r$cit_to_select, ignoreNULL = FALSE, {
      print(r$cit_to_select)
    })

    citation_details_server("citation_details", r)
    notes_server("citation_notes", r, "citation_rows", show_citations_modal)
    media_links_server("citation_media", r, "citation_rows", show_citations_modal)


  })
}

show_citations_modal <- function(ns, r){
  
  shiny::modalDialog(
    title = "Edit source citations",
    
    shiny::helpText("Here you can manage citations associated with an item.",
                    "Citations are links to sources that provide evidence for the item."),
    shiny::tags$hr(),
    shiny::selectizeInput(ns("citation"), label = NULL, choices = r$cits, selected = r$cit_to_select,
                          multiple = TRUE, width = "750px", options = list(maxItems = 1)),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(ns("add_citation"), "Add citation"),
                    shiny::actionButton(ns("remove_citation"), "Delete citation")
      )
    ),
    
    shiny::tags$hr(),
    
    shiny::fluidRow(id = ns("citation_section"),
                    shiny::column(12,
                                  notes_ui(ns("citation_notes")),
                                  media_links_ui(ns("citation_media")),
                                  
                    ),
                    shiny::column(12,
                                  citation_details_ui(ns("citation_details"))
                    )
    ) %>% shinyjs::hidden()
    
    
  ) %>% shiny::showModal()
  
}