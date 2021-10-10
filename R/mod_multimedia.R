
multimedia_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("record"), label = NULL, choices = NULL,
                                          multiple = TRUE, width = "500px", options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add multimedia"),
                    shiny::actionButton(ns("delete"), "Delete multimedia")
      )
      
    ),
    
    shiny::fluidRow(id = ns("media_data"),
                    shiny::column(width = 6,
                                  ref_numbers_ui(ns("media_ref_numbers"))
                    ),
                    shiny::column(width = 6,
                                  notes_ui(ns("media_notes")),
                                  citations_ui(ns("media_citations"))
                    )
    ) %>% shinyjs::hidden(),
    
    shiny::br(),
    
    shinyjs::hidden(
      shiny::fluidRow(id = ns("media_tabs"),
                      shiny::column(12,
                                    shiny::tabsetPanel(id = ns("tabset"),
                                      shiny::tabPanel("Summary", multimedia_summary_ui(ns("media_summary"))),
                                      shiny::tabPanel("Description", multimedia_description_ui(ns("media_description"))),
                                      shiny::tabPanel("Raw data", record_ui(ns("media_raw")))
                                    )
                      )
      )
    )
  )
}

multimedia_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update list of media
    records <- shiny::reactive({
      req(r$ged)
      
      tidyged::xrefs_media(r$ged) %>% 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
    })
    
    # Update choices with list of media and select one
    observeEvent(records(), {
      if(!is.null(records())) {
        
        if(is.null(r$media_to_select)) {
          current_selection <- input$record
        } else {
          current_selection <- r$media_to_select
        }
        
        shiny::updateSelectizeInput(session = session, inputId = "record", choices = records(), selected = current_selection)
      } else {
        shiny::updateSelectizeInput(session = session, inputId = "record", choices = character(), selected = character())
      }
      r$media_to_select <- NULL
    })
    
    # Update media_rows
    shiny::observeEvent(priority = 2, {
      input$record
      r$ged
    }, {
      req(input$record)
      media_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$media_rows <- which(r$ged$record == media_xref)
    })
    
    # Show/hide tabs and toggle delete button
    shiny::observeEvent(input$record, ignoreNULL = FALSE, {
      shinyjs::toggle("media_tabs", condition = !is.null(input$record))
      shinyjs::toggleState("delete", !is.null(input$record))
    })
    
    # Add media and set a flag to ensure it is selected
    observeEvent(input$add, {
      r$ged <- tidyged::add_media(r$ged) #TODO: Can't add empty media - need modal to choose file ref and format
      media_xrefs <- tidyged::xrefs_media(r$ged)
      last_media <- tail(media_xrefs, 1)
      r$media_to_select <- tidyged::describe_records(r$ged, last_media, short_desc = TRUE)
    })
    
    # Remove media and set a flag to ensure no media is selected
    observeEvent(input$delete, {
      media_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$ged <- tidyged::remove_media(r$ged, media_xref)
      shiny::showNotification("Multimedia deleted")
      r$media_to_select <- NULL
    })
    
    multimedia_summary_server("media_summary", r)
    record_server("media_raw", r, "media_rows")
    ref_numbers_server("media_ref_numbers", r, "media_rows")
    
    shiny::observeEvent({input$tabset == "Description"},once=TRUE,ignoreInit = TRUE, {
      multimedia_description_server("media_description", r)
    })

    notes_server("media_notes", r, "media_rows")

    citations_server("media_citations", r, "media_rows")
    
  })
}


