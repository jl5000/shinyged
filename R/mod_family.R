

family_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("record"), label = NULL, choices = NULL, 
                                          multiple = TRUE, width = "500px", options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add family group"),
                    shiny::actionButton(ns("delete"), "Delete family group"),
                    shiny::checkboxInput(ns("and_members"), "Delete family members also?")
      )
      
    ),
    
    shinyjs::hidden(
      shiny::fluidRow(id = ns("famg_tabs"),
                      shiny::column(12,
                                    shiny::tabsetPanel(
                                      shiny::tabPanel("Summary", family_summary_ui(ns("family_summary"))),
                                      shiny::tabPanel("Members", family_members_ui(ns("family_members"))),
                                      shiny::tabPanel("Events", family_events_ui(ns("family_events"))),
                                      shiny::tabPanel("Notes", notes_ui(ns("famg_notes"))),
                                      shiny::tabPanel("Citations", citations_ui(ns("famg_citations"))),
                                      shiny::tabPanel("Media", media_links_ui(ns("famg_media")))
                                    )
                      )
      )
    )
  )
}



family_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    # Update list of families
    records <- shiny::reactive({
      req(r$ged)
      tidyged::xrefs_famg(r$ged) %>% 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
    })
    
    # Update choices with list of families and select one
    observeEvent(records(), {
      if(!is.null(records())) {
        
        if(is.null(r$famg_to_select)) {
          current_selection <- input$record
        } else {
          current_selection <- r$famg_to_select
        }
        
        shiny::updateSelectizeInput(session = session, inputId = "record", choices = records(), selected = current_selection)
      } else {
        shiny::updateSelectizeInput(session = session, inputId = "record", choices = character(), selected = character())
      }
      r$famg_to_select <- NULL
    })
    
    # Update famg_rows
    shiny::observeEvent(priority = 2, {
      r$ged
      input$record
    }, {
      req(input$record)
      famg_xref <- stringr::str_extract(input$record, "@[a-zA-Z0-9]{1,20}@")
      r$famg_rows <- which(r$ged$record == famg_xref)
    })
    
    # Show/hide tabs and toggle delete button
    shiny::observeEvent(input$record, ignoreNULL = FALSE, {
      shinyjs::toggle("famg_tabs", condition = !is.null(input$record))
      shinyjs::toggleState("delete", !is.null(input$record))
      shinyjs::toggleState("and_members", !is.null(input$record))
    })
    
    # Add family and set a flag to ensure it is selected
    observeEvent(input$add, {
      r$ged <- tidyged::add_famg(r$ged)
      famg_xrefs <- tidyged::xrefs_famg(r$ged)
      last_famg <- tail(famg_xrefs, 1)
      r$famg_to_select <- tidyged::describe_records(r$ged, last_famg, short_desc = TRUE)
    })
    
    # Remove family and set a flag to ensure the previous family is selected
    observeEvent(input$delete, {
      famg_xref <- stringr::str_extract(input$record, "@[a-zA-Z0-9]{1,20}@")
      r$ged <- tidyged::remove_famg(r$ged, famg_xref, remove_individuals = input$and_members)
      shiny::showNotification("Family deleted")
      if(input$and_members) shiny::showNotification("Family members deleted")
      famg_xrefs <- tidyged::xrefs_famg(r$ged)
      last_famg <- tail(famg_xrefs, 1)
      r$famg_to_select <- tidyged::describe_records(r$ged, last_famg, short_desc = TRUE)
    })
    
    family_summary_server("family_summary", r)
    # family_members_server("family_members", r)
    # family_events_server("family_events", r)
    notes_server("famg_notes", r, "famg_rows")
    
    media_links_server("famg_media", r, "famg_rows")
  })
}


