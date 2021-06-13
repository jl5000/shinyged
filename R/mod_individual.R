
individual_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("record"), label = NULL, choices = NULL, 
                                       multiple = TRUE, width = "500px", options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add individual"),
                    shiny::actionButton(ns("delete"), "Delete individual")
      )
      
    ),
    
    shinyjs::hidden(
      shiny::fluidRow(id = ns("indi_tabs"),
                      shiny::column(12,
                                    shiny::tabsetPanel(
                                      shiny::tabPanel("Summary", individual_summary_ui(ns("indi_summary"))),
                                      shiny::tabPanel("Names", individual_names_ui(ns("indi_names"))),
                                      shiny::tabPanel("Facts", individual_facts_ui(ns("indi_facts"))),
                                      shiny::tabPanel("Links", individual_links_ui(ns("indi_links"))),
                                      shiny::tabPanel("Notes", notes_ui(ns("indi_notes"))),
                                      shiny::tabPanel("Citations", citations_ui(ns("indi_citations"))),
                                      shiny::tabPanel("Media", media_links_ui(ns("indi_media")))
                                    )
                      )
      )
    )
  )
}

individual_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    # Update list of individuals
    records <- shiny::reactive({
      req(r$ged)
      tidyged::xrefs_indi(r$ged) %>% 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
    })
    
    # Update choices with list of individuals and select one
    observeEvent(records(), {
      if(!is.null(records())) {
        
        if(is.null(r$indi_to_select)) {
          current_selection <- input$record
        } else {
          current_selection <- r$indi_to_select
        }
        
        shiny::updateSelectizeInput(session = session, inputId = "record", choices = records(), selected = current_selection)
      } else {
        shiny::updateSelectizeInput(session = session, inputId = "record", choices = character(), selected = character())
      }
      r$indi_to_select <- NULL
    })
    
    # Update indi_rows
    shiny::observeEvent(input$record, {
      req(input$record)
      indi_xref <- stringr::str_extract(input$record, "@[a-zA-Z0-9]{1,20}@")
      r$indi_rows <- which(r$ged$record == indi_xref)
    })
    
    # Show/hide tabs and toggle delete button
    shiny::observeEvent(input$record, ignoreNULL = FALSE, {
      shinyjs::toggle("indi_tabs", condition = !is.null(input$record))
      shinyjs::toggleState("delete", !is.null(input$record))
    })
    
    # Add individual and set a flag to ensure it is selected
    observeEvent(input$add, {
      r$ged <- tidyged::add_indi(r$ged)
      indi_xrefs <- tidyged::xrefs_indi(r$ged)
      last_indi <- tail(indi_xrefs, 1)
      r$indi_to_select <- tidyged::describe_records(r$ged, last_indi, short_desc = TRUE)
    })
    
    # Remove individual and set a flag to ensure the previous individual is selected
    observeEvent(input$delete, {
      indi_xref <- stringr::str_extract(input$record, "@[a-zA-Z0-9]{1,20}@")
      r$ged <- tidyged::remove_indi(r$ged, indi_xref)
      indi_xrefs <- tidyged::xrefs_indi(r$ged)
      last_indi <- tail(indi_xrefs, 1)
      r$indi_to_select <- tidyged::describe_records(r$ged, last_indi, short_desc = TRUE)
    })
    
    # individual_summary_server("indi_summary", r, "indi_rows")
    # individual_names_server("indi_names", r, "indi_name_rows")
    # individual_facts_server("indi_facts", r, "indi_fact_rows")
    # individual_links_server("indi_links", r, "indi_links_rows")
    notes_server("indi_notes", r, "indi_rows")
    
    media_links_server("indi_media", r, "indi_rows")
  })
}

