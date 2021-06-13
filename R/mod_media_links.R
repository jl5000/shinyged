

media_links_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::helpText("Here you can manage links to multimedia associated with an item.", 
                    "Use the buttons to add and remove links by selecting items in the list."),
    shiny::tags$hr(),
    DT::DTOutput(ns("media_list")),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(ns("add_link"), "Add multimedia link"),
                    shinyjs::disabled(
                      shiny::actionButton(ns("remove_link"), "Remove multimedia link")
                    )
      )
    )
    
  )
  
}

media_links_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # The vector of media xrefs
    media_xrefs <- shiny::reactive({
      req(r$ged, r[[section_rows]])
      
      dplyr::slice(r$ged, r[[section_rows]]) %>%
        dplyr::filter(level == .$level[1] + 1, tag == "OBJE") %>% 
        dplyr::pull(value)
    })
    
    media_desc <- shiny::reactive({
      req(media_xrefs)
      tidyged::describe_records(r$ged, media_xrefs())
    })

    # The row of the tidyged object corresponding to the selected media link
    selected_ged_row <- shiny::eventReactive({
      r$ged
      r[[section_rows]]
      input$media_list_rows_selected
    },{
      req(r$ged, r[[section_rows]], input$media_list_rows_selected)
      
      dplyr::mutate(r$ged, row = dplyr::row_number()) %>% 
        dplyr::slice(r[[section_rows]]) %>%
        dplyr::filter(level == .$level[1] + 1, tag == "OBJE") %>% 
        dplyr::slice(input$media_list_rows_selected) %>% 
        dplyr::pull(row)
    })
    
    # Update table with media links
    output$media_list <- DT::renderDataTable({
      DT::datatable(data.frame(Multimedia = media_desc()), rownames = FALSE, selection = "single")
    })
    
    # Disable add_link button if no media records to point to
    shiny::observeEvent(r$ged, {
      shinyjs::toggleState("add_link", tidyged::num_media(r$ged) > 0)
    })
    
    # Disable remove_link button if nothing selected
    shiny::observeEvent(input$media_list_rows_selected, ignoreNULL = FALSE, {
      shinyjs::toggleState("remove_link", !is.null(input$media_list_rows_selected))
    })
    
    # Show dialog to choose a media record
    shiny::observeEvent(input$add_link, {
      req(r$ged)
      shiny::showModal(
        shiny::modalDialog(
          shiny::selectizeInput(ns("media_choice"), label = "Add reference to existing multimedia record...",
                                choices = tidyged::describe_records(r$ged, tidyged::xrefs_media(r$ged)),
                                multiple = TRUE, options = list(maxItems = 1), width = "500px"),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("add_media_ref"), "Add reference")
          )
        )
      )
    })

    # Disable add_media_ref button if no media records or it's already been added
    shiny::observeEvent(input$media_choice, ignoreNULL = FALSE, {
      shinyjs::toggleState("add_media_ref", !is.null(input$media_choice) && 
                             !input$media_choice %in% media_desc())
    })
    
    # Add media reference
    shiny::observeEvent(input$add_media_ref, {
      media_xref <- stringr::str_extract(input$media_choice, "@[a-zA-Z0-9]{1,20}@")
      
      r$ged <- tibble::add_row(r$ged,
                               tibble::tibble(record = r$ged$record[r[[section_rows]][1]],
                                              level = r$ged$level[r[[section_rows]][1]] + 1,
                                              tag = "OBJE",
                                              value = media_xref),
                               # Need to insert new links after final link so it
                               # doesn't shift existing row numbers
                               .after = max(r[[section_rows]]))
      
      shiny::removeModal()
    })
    
    # Remove media link
    shiny::observeEvent(input$remove_link, {
      r$ged <- dplyr::slice(r$ged, -selected_ged_row())
    })
    

  })
}


