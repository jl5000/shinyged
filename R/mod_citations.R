


citations_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::helpText("Here you can manage citations associated with an item.", 
                    "Use the buttons to add and remove citations by selecting items in the list."),
    shiny::tags$hr(),
    DT::DTOutput(ns("citations_list")),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(ns("add_citation"), "Add citation"),
                    shinyjs::disabled(
                      shiny::actionButton(ns("remove_citation"), "Remove citation")
                    )
      )
    ),
    shiny::tags$br(),
    #shinyjs::hidden(
      shiny::fluidRow(id = ns("citation_tabs"),
                      shiny::column(12,
                                    shiny::tabsetPanel(
                                      shiny::tabPanel("Details", citation_details_ui(ns("citation_details"))),
                                      shiny::tabPanel("Notes", notes_ui(ns("citation_notes"))),
                                      shiny::tabPanel("Media", media_links_ui(ns("citation_media")))
                                    )
                      )
     # )
    )
  )
  
  
}


citations_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # The vector of source xrefs being cited
    sources_raw <- shiny::reactive({
      req(r$ged, r[[section_rows]])
      
      dplyr::slice(r$ged, r[[section_rows]]) %>%
        dplyr::filter(level == .$level[1] + 1, tag == "SOUR") %>% 
        dplyr::pull(value)
    })
    
    # The list of rows in the tidyged object for each citation
    citations_rows <- shiny::reactive({
      req(r$ged, r[[section_rows]], sources_raw)

      purrr::map(sources_raw(), tidyged.internals::identify_section, 
                 gedcom = r$ged, containing_tag = "SOUR",
                 containing_level = r$ged$level[r[[section_rows]]][1] + 1,
                 xrefs = r$ged$record[r[[section_rows]]][1])
    })
    
    # df of citations source title, page, certainty, firstrow, lastrow
    citations_summary <- shiny::reactive({
      req(r$ged, r[[section_rows]], sources_raw, citations_rows)

      titles <- purrr::map_chr(sources_raw(), tidyged.internals::gedcom_value, 
                               gedcom = r$ged, tag = "TITL", level = 1)
     
      pages <- purrr::map_chr(citations_rows(), 
                              ~ dplyr::slice(r$ged, .x) %>% 
                                dplyr::filter(tag == "PAGE") %>% 
                                dplyr::pull(value) %>% 
                                ifelse(length(.) == 0, "", .))
    
      certainty <- purrr::map_chr(citations_rows(), 
                              ~ dplyr::slice(r$ged, .x) %>% 
                                dplyr::filter(tag == "QUAY") %>% 
                                dplyr::pull(value) %>% 
                                ifelse(length(.) == 0, "", .))

      first_rows <- purrr::map_int(citations_rows(), dplyr::first)
      last_rows <- purrr::map_int(citations_rows(), dplyr::last)
     
      tibble::tibble(`Source Ref` = sources_raw(),
                     Title = titles,
                     Where = pages,
                     Credibility = certainty,
                     first_rows = first_rows, last_rows=last_rows)

    })
    
    # The rows of the tidyged object corresponding to the selected citation
    shiny::observeEvent(input$citations_list_rows_selected, {

      r$citation_rows <- citations_rows()[[input$citations_list_rows_selected]]
        
    })
    
    # Update table with citations
    output$citations_list <- DT::renderDataTable({
      DT::datatable(dplyr::select(citations_summary(), -first_rows, -last_rows), 
                    rownames = FALSE, selection = "single")
    })
    
    # Disable add_citation button if no source records to point to
    shiny::observeEvent(r$ged, {
      shinyjs::toggleState("add_citation", tidyged::num_sour(r$ged) > 0)
    })
    
    # Disable remove button if nothing selected
    shiny::observeEvent(input$citations_list_rows_selected, ignoreNULL = FALSE, {
      shinyjs::toggleState("remove_citation", !is.null(input$citations_list_rows_selected))
    })
    
    # Show dialog to choose a source record
    shiny::observeEvent(input$add_citation, {
      req(r$ged)
      shiny::showModal(
        shiny::modalDialog(
          shiny::selectizeInput(ns("source_select"), label = "Choose a source...",
                                choices = tidyged::describe_records(r$ged, tidyged::xrefs_sour(r$ged), short_desc = TRUE),
                                multiple = TRUE, options = list(maxItems = 1), width = "500px"),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("add_sour_citation"), "Add source citation")
          )
        )
      )
    })
    
    # Disable add_sour_citation button if no source records
    shiny::observeEvent(input$source_select, ignoreNULL = FALSE, {
      shinyjs::toggleState("add_sour_citation", !is.null(input$source_select))
    })
    
    # Add source citation
    shiny::observeEvent(input$add_sour_citation, {
      sour_xref <- stringr::str_extract(input$source_select, "@[a-zA-Z0-9]{1,20}@")
      
      r$ged <- tibble::add_row(r$ged,
                               tibble::tibble(record = r$ged$record[r[[section_rows]][1]],
                                              level = r$ged$level[r[[section_rows]][1]] + 1,
                                              tag = "SOUR",
                                              value = sour_xref),
                               # Need to insert new notes after final note so it
                               # doesn't shift existing row numbers
                               .after = max(r[[section_rows]]))
      
      shiny::removeModal()
    })
    
    # Remove citation
    shiny::observeEvent(input$remove_citation, {
      r$ged <- dplyr::slice(r$ged, -r$citation_rows)
    })
    

    # citation_details_server("citation_details", r, "citation_rows")
    notes_server("citation_notes", r, "citation_rows")
    media_links_server("citation_media", r, "citation_rows")
    
  })
}

