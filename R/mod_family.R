

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

    shiny::fluidRow(id = ns("famg_data"),
                    shiny::column(width = 12,
                                  ref_numbers_ui(ns("famg_ref_numbers")),
                                  notes_ui(ns("famg_notes")),
                                  media_links_ui(ns("famg_media")),
                    )
    ) %>% shinyjs::hidden(),
    
    shiny::br(),
    
    shiny::fluidRow(id = ns("famg_tabs"),
                    shiny::column(width = 12,
                                  shiny::tabsetPanel(id = ns("tabset"),
                                                     shiny::tabPanel("Summary", family_summary_ui(ns("family_summary"))),
                                                     shiny::tabPanel("Members", family_members_ui(ns("family_members"))),
                                                     shiny::tabPanel("Events", family_events_ui(ns("family_events"))),
                                                     shiny::tabPanel("Timeline", timeline_ui(ns("famg_timeline"))),
                                                     shiny::tabPanel("Citations", citations_ui(ns("famg_citations"))),
                                                     shiny::tabPanel("Raw data", record_ui(ns("famg_raw"))))
                    )
    ) %>% shinyjs::hidden(),
    
    
  )
}



family_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    ref_numbers_server("famg_ref_numbers", r, "famg_rows")
    notes_server("famg_notes", r, "famg_rows")
    media_links_server("famg_media", r, "famg_rows")
    family_summary_server("family_summary", r)
    
    shiny::observe({
      if(input$tabset == "Timeline") timeline_server("famg_timeline", r, "famg_rows")
      else if(input$tabset == "Members") family_members_server("family_members", r)
      else if(input$tabset == "Events") family_events_server("family_events", r)
      else if(input$tabset == "Citations") citations_server("famg_citations", r, "famg_rows")
      else if(input$tabset == "Raw data") record_server("famg_raw", r, "famg_rows")
    }) %>% 
      shiny::bindEvent(input$tabset, ignoreInit = TRUE)
  
    
    # Update list of families -------------------------------------------------------
    records <- shiny::reactive({
      req(r$ged)
      tidyged::xrefs_famg(r$ged) %>% 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
    })
    
    # Update choices with list of families and select one --------------------------
    shiny::observe({
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
    }) %>% 
      shiny::bindEvent(records())
    
    # Update famg_rows --------------------------------------------------------------
    shiny::observe(priority = 2, {
      req(input$record)
      famg_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$famg_rows <- which(r$ged$record == famg_xref)
    }) %>% 
      shiny::bindEvent(r$ged, input$record)
    
    # Show/hide tabs and toggle delete button ---------------------------------------
    shiny::observe({
      shinyjs::toggle("famg_tabs", condition = !is.null(input$record))
      shinyjs::toggle("famg_data", condition = !is.null(input$record))
      shinyjs::toggleState("delete", !is.null(input$record))
      shinyjs::toggleState("and_members", !is.null(input$record))
    }) %>% 
      shiny::bindEvent(input$record, ignoreNULL = FALSE)
    
    # Add family and set a flag to ensure it is selected ----------------------------
    shiny::observe({
      r$ged <- tidyged::add_famg(r$ged)
      famg_xrefs <- tidyged::xrefs_famg(r$ged)
      last_famg <- tail(famg_xrefs, 1)
      r$famg_to_select <- tidyged::describe_records(r$ged, last_famg, short_desc = TRUE)
    }) %>% 
      shiny::bindEvent(input$add)
    
    # Remove family and set a flag to ensure no family is selected ------------------
    shiny::observe({
      famg_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$ged <- tidyged::remove_famg(r$ged, famg_xref, remove_individuals = input$and_members)
      shiny::showNotification("Family deleted")
      if(input$and_members) shiny::showNotification("Family members deleted")
      r$famg_to_select <- NULL
    }) %>% 
      shiny::bindEvent(input$delete)
    
  })
}


