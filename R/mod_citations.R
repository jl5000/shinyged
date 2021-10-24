


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
    citation_details_server("citation_details", r)
 
    
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
    
    
    # Update citation_rows with rows of selected citation --------------------
    # shiny::observe({
    #   req(citations_rows)
    # 
    #   r$citation_rows <- citations_rows()[[input$table_rows_selected]]
    # }) %>%
    #   shiny::bindEvent(input$table_rows_selected)


    # Show/hide tabs and toggle delete button if no citation -----------------
    shiny::observe({
      shinyjs::toggle("citation_section", condition = !is.null(input$table_rows_selected))
      shinyjs::toggleState("remove_citation", !is.null(input$table_rows_selected))
      shinyjs::toggleState("update_citation", !is.null(input$table_rows_selected))
      r$cit_to_select <- input$table_rows_selected
      
      #req(citations_rows, input$table_rows_selected)
      if(is.null(input$table_rows_selected)){
        r$citation_rows <- NULL
      } else {
        r$citation_rows <- citations_rows()[[input$table_rows_selected]]
      }
      
    }) %>%
      shiny::bindEvent(input$table_rows_selected, ignoreNULL = FALSE)
     
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

      r$cit_to_select <- nrow(cit_table())
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
