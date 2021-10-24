

media_links_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::actionButton(ns("media_links"), label = "Media")
  )
  }

media_links_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    # Update media links button ----------------------------------------------------
    shiny::observe({
      shinyjs::toggleState("media_links", tidyged::num_media(r$ged) > 0 &&
                             !is.null(r[[section_rows]]))
      
      if(is.null(r[[section_rows]])){
        shiny::updateActionButton(inputId = "media_links", label = "Media")
      } else {
        lbl <- paste0(length(media_xrefs()), " media")
        shiny::updateActionButton(inputId = "media_links", label = lbl)
      }
    })
    

    # Click the button to show popup ------------------------------------------
    shiny::observe({
   
      shiny::modalDialog(
        title = "Edit multimedia links",
        
        shiny::helpText("Here you can manage links to multimedia associated with an item.", 
                        "Use the buttons to add and remove links by selecting items in the list."),
        shiny::tags$hr(),
        DT::DTOutput(ns("media_list")),
        shiny::tags$br(),
        
        shiny::fluidRow(
          shiny::column(12,
                        shiny::selectizeInput(ns("media_choice"), label = "Add reference to existing multimedia record...",
                                              choices = tidyged::describe_records(r$ged, tidyged::xrefs_media(r$ged)),
                                              multiple = TRUE, options = list(maxItems = 1), width = "500px"),
          )
        ),
        shiny::fluidRow(
          shiny::column(12,
                        shiny::actionButton(ns("add_link"), "Add multimedia link"),
                        shiny::actionButton(ns("remove_link"), "Remove multimedia link") %>% 
                          shinyjs::disabled()
                        
          )
        ),
      ) %>% shiny::showModal()
    }) %>% 
      shiny::bindEvent(input$media_links)
    


    # The vector of media xrefs -----------------------------------------------
    media_xrefs <- shiny::reactive({
      req(r$ged, r[[section_rows]])
      
      dplyr::slice(r$ged, r[[section_rows]]) %>%
        dplyr::filter(level == .$level[1] + 1, tag == "OBJE") %>% 
        dplyr::pull(value)
    })
    

    # The vector of media descriptions ----------------------------------------
    media_desc <- shiny::reactive({
      req(r$ged, media_xrefs)
      tidyged::describe_records(r$ged, media_xrefs())
    })


    # The row of the tidyged object corresponding to the selected media --------
    selected_ged_row <- shiny::reactive({
      req(r$ged, r[[section_rows]], input$media_list_rows_selected)
      
      dplyr::mutate(r$ged, row = dplyr::row_number()) %>% 
        dplyr::slice(r[[section_rows]]) %>%
        dplyr::filter(level == .$level[1] + 1, tag == "OBJE") %>% 
        dplyr::slice(input$media_list_rows_selected) %>% 
        dplyr::pull(row)
    }) %>% 
      shiny::bindEvent(r$ged, r[[section_rows]], input$media_list_rows_selected)
    
    # Update table with media links -------------------------------------------
    output$media_list <- DT::renderDataTable({
      DT::datatable(data.frame(Multimedia = media_desc()), rownames = FALSE, selection = "single")
    })
    
    
    # Disable remove_link button if nothing selected --------------------------
    shiny::observe({
      shinyjs::toggleState("remove_link", !is.null(input$media_list_rows_selected))
    }) %>% 
      shiny::bindEvent(input$media_list_rows_selected, ignoreNULL = FALSE)
    

    # Disable add_link button if no media records or it's already been added -----
    shiny::observe({
      shinyjs::toggleState("add_link", !is.null(input$media_choice) && 
                             !input$media_choice %in% media_desc())
    })
    
    # Add media reference ------------------------------------------------------
    shiny::observe({
      media_xref <- stringr::str_extract(input$media_choice, tidyged.internals::reg_xref(FALSE))
      
      r$ged <- tibble::add_row(r$ged,
                               tibble::tibble(record = r$ged$record[r[[section_rows]][1]],
                                              level = r$ged$level[r[[section_rows]][1]] + 1,
                                              tag = "OBJE",
                                              value = media_xref),
                               # Need to insert new links after final link so it
                               # doesn't shift existing row numbers
                               .after = max(r[[section_rows]]))
      
    }) %>% 
      shiny::bindEvent(input$add_link)
    
    # Remove media link ----------------------------------------------------------
    shiny::observe({
      r$ged <- dplyr::slice(r$ged, -selected_ged_row())
    }) %>% 
      shiny::bindEvent(input$remove_link)
    

  })
}

