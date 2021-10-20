


citations_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::br(),
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
    
    
    
  )
}


citations_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    notes_server("citation_notes", r, "citation_rows")
    media_links_server("citation_media", r, "citation_rows")
    
    shiny::observe(citation_details_server("citation_details", r)) %>% 
      shiny::bindEvent(input$citation)
    

    # The list of rows in the tidyged object for each citation ----------------
    citations_rows <- shiny::reactive({
      req(r$ged, r[[section_rows]])

      rows_vect <- tidyged.internals::identify_section(r$ged,
                                                       containing_level = r$ged$level[r[[section_rows]]][1] + 1,
                                                       containing_tags = "SOUR",
                                                       xrefs = r$ged$record[r[[section_rows]]][1])

      split(rows_vect, cumsum(r$ged$tag[rows_vect] == "SOUR"))

    })
    

    # The vector of citation descriptions --------------------------------------
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
    
    # The citation to select after changes -------------------------------------
    citation_to_select <- shiny::reactive({
      if(is.null(r$cit_to_select)) {
        input$citation
      } else {
        r$cit_to_select
      }
    })
    
    # Update choices with list of citations and select one -------------------
    shiny::observe({
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

    # Update citation_rows with rows of selected citation --------------------
    shiny::observe({
      req(input$citation, citations_rows, citations)

      r$citation_rows <- citations_rows()[[which(citations() == input$citation)]]
    }) %>% 
      shiny::bindEvent(input$citation)


    # Show/hide tabs and toggle delete button if no citation -----------------
    shiny::observe({
      shinyjs::toggle("citation_section", condition = !is.null(input$citation))
      shinyjs::toggleState("remove_citation", !is.null(input$citation))
    }) %>% 
      shiny::bindEvent(input$citation, ignoreNULL = FALSE)
    
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

    # Disable add_sour_citation button if no source records --------------------
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
      
      r$cit_to_select <- citations()[length(citations())]
      shiny::removeModal()
    }) %>% 
      shiny::bindEvent(input$add_sour_citation)

    # Remove citation -------------------------------------------------------------
    shiny::observe({
      r$ged <- dplyr::slice(r$ged, -r$citation_rows)
      r$cit_to_select <- NULL
    }) %>% 
      shiny::bindEvent(input$remove_citation)
    
   
  })
}
