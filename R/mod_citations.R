


citations_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$br(),
    shiny::helpText("Here you can manage citations associated with an item.",
                    "Citations are links to sources that provide evidence for the item."),
    shiny::tags$hr(),
    shiny::selectizeInput(ns("citation"), label = NULL, choices = NULL,
                          multiple = TRUE, width = "750px", options = list(maxItems = 1)),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(ns("add_citation"), "Add citation"),
                    shiny::actionButton(ns("remove_citation"), "Delete citation")
      )
    ),
    notes_ui(ns("citation_notes")),
    media_links_ui(ns("citation_media")),
    shiny::tags$br(),
    
    shiny::fluidRow(id = ns("citation_tabs"),
                    shiny::column(12,
                                  shiny::tabsetPanel(id = ns("tabset"),
                                                     shiny::tabPanel("Details", citation_details_ui(ns("citation_details")))
                                  )
                    )
    ) %>% shinyjs::hidden()
    
  )


}


citations_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The list of rows in the tidyged object for each citation
    citations_rows <- shiny::reactive({
      req(r$ged, r[[section_rows]])

      rows_vect <- tidyged.internals::identify_section(r$ged,
                                                       containing_level = r$ged$level[r[[section_rows]]][1] + 1,
                                                       containing_tags = "SOUR",
                                                       xrefs = r$ged$record[r[[section_rows]]][1])

      split(rows_vect, cumsum(r$ged$tag[rows_vect] == "SOUR"))

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

    citation_to_select <- shiny::reactive({
      if(is.null(r$cit_to_select)) {
        input$citation
      } else {
        r$cit_to_select
      }
    })
    
    # Update choices with list of individuals and select one
    observeEvent(citations(), ignoreNULL = FALSE, {
      if(!is.null(citations())) {
        shiny::updateSelectizeInput(session = session, inputId = "citation", choices = citations(), 
                                    selected = citation_to_select())
      } else {
        shiny::updateSelectizeInput(session = session, inputId = "citation", choices = character(), 
                                    selected = character())
      }
      r$cit_to_select <- NULL
    })

    # Update citation_rows with rows of selected citation
    shiny::observeEvent(input$citation, {
      req(input$citation, citations_rows, citations)
      
      r$citation_rows <- citations_rows()[[which(citations() == input$citation)]]
    })


    # Show/hide tabs and toggle delete button if no citation
    shiny::observeEvent(input$citation, ignoreNULL = FALSE, {
      shinyjs::toggle("citation_tabs", condition = !is.null(input$citation))
      shinyjs::toggleState("remove_citation", !is.null(input$citation))
    })
    
    # Disable add_citation button if no source records to point to
    shiny::observeEvent(r$ged, {
      shinyjs::toggleState("add_citation", tidyged::num_sour(r$ged) > 0)
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

    citation_details_server("citation_details", r)

    notes_server("citation_notes", r, "citation_rows")


    media_links_server("citation_media", r, "citation_rows")


  })
}

