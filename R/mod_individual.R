
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
    
    shiny::fluidRow(id = ns("indi_data"),
                    shiny::column(3,
                                  shiny::selectizeInput(ns("sex"), label = "Sex", choices = tidyged.internals::val_sexes(), 
                                                        multiple = TRUE, width = "100%", options = list(maxItems = 1)),
                    ),
                    shiny::column(9, style = 'margin-top:25px',
                                  ref_numbers_ui(ns("indi_ref_numbers")),
                                  notes_ui(ns("indi_notes")),
                                  media_links_ui(ns("indi_media")),
                    ),

    ) |> shinyjs::hidden(),
    
    shiny::br(),
    
    shiny::fluidRow(id = ns("indi_tabs"),
                    shiny::column(width = 12,
                                  shiny::tabsetPanel(id = ns("tabset"),
                                                     shiny::tabPanel("Summary", individual_summary_ui(ns("indi_summary"))),
                                                     shiny::tabPanel("Names", individual_names_ui(ns("indi_names"))),
                                                     shiny::tabPanel("Facts", individual_facts_ui(ns("indi_facts"))),
                                                     shiny::tabPanel("Timeline", timeline_ui(ns("indi_timeline"))),
                                                     shiny::tabPanel("Links", individual_links_ui(ns("indi_links"))),
                                                     shiny::tabPanel("Citations", citations_ui(ns("indi_citations"))),
                                                     shiny::tabPanel("Raw data", record_ui(ns("indi_raw"))))
                    )
    ) |> shinyjs::hidden()
    
  )
}

individual_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    
    individual_summary_server("indi_summary", r)
    ref_numbers_server("indi_ref_numbers", r, "indi_rows")
    notes_server("indi_notes", r, "indi_rows")
    media_links_server("indi_media", r, "indi_rows")
    
    shiny::observe(timeline_server("indi_timeline", r, "indi_rows")) |>
      shiny::bindEvent(input$tabset == "Timeline", once = TRUE, ignoreInit = TRUE)
    
    shiny::observe(individual_names_server("indi_names", r)) |>
      shiny::bindEvent(input$tabset == "Names", once = TRUE, ignoreInit = TRUE)
    
    shiny::observe(individual_facts_server("indi_facts", r)) |>
      shiny::bindEvent(input$tabset == "Facts", once = TRUE, ignoreInit = TRUE)
    
    shiny::observe(individual_links_server("indi_links", r)) |>
      shiny::bindEvent(input$tabset == "Links", once = TRUE, ignoreInit = TRUE)
    
    shiny::observe(record_server("indi_raw", r, "indi_rows")) |>
      shiny::bindEvent(input$tabset == "Raw data", once = TRUE, ignoreInit = TRUE)
    
    shiny::observe(citations_server("indi_citations", r, "indi_rows")) |>
      shiny::bindEvent(input$tabset == "Citations", once = TRUE, ignoreInit = TRUE)
    

    # Update list of individuals ----------------------------------------------
    records <- shiny::reactive({
      req(r$ged)

      tidyged::xrefs_indi(r$ged) |> 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
    })
    
    # Update choices with list of individuals and select one -------------------
    shiny::observe({
      if(!is.null(records())) {
        
        if(is.null(r$indi_to_select)) {
          current_selection <- input$record
        } else {
          current_selection <- r$indi_to_select
        }
        
        shiny::updateSelectizeInput(inputId = "record", choices = records(), selected = current_selection)
      } else {
        shiny::updateSelectizeInput(inputId = "record", choices = character(), selected = character())
      }
      r$indi_to_select <- NULL
    }) |> 
      shiny::bindEvent(records())
    
    # Update indi_rows ----------------------------------------------------------
    shiny::observe(priority = 2, {
      req(input$record)
      indi_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$indi_rows <- which(r$ged$record == indi_xref)
    }) |> 
      shiny::bindEvent(input$record, r$ged)
    
    # Show/hide tabs and toggle delete button ----------------------------------
    shiny::observe({
      shinyjs::toggle("indi_tabs", condition = !is.null(input$record))
      shinyjs::toggle("indi_data", condition = !is.null(input$record))
      shinyjs::toggleState("delete", !is.null(input$record))
      if(!is.null(input$record)){
        sex <- tidyged.internals::gedcom_value(r$ged, r$ged$record[r$indi_rows[1]], "SEX", 1)
        shiny::updateSelectizeInput(inputId = "sex", selected = sex)
      }
    }) |> 
      shiny::bindEvent(input$record, ignoreNULL = FALSE)
    
    # Add individual and set a flag to ensure it is selected -------------------
    shiny::observe({
      r$ged <- tidyged::add_indi(r$ged)
      indi_xrefs <- tidyged::xrefs_indi(r$ged)
      last_indi <- tail(indi_xrefs, 1)
      r$indi_to_select <- tidyged::describe_records(r$ged, last_indi, short_desc = TRUE)
    }) |> 
      shiny::bindEvent(input$add)
    
    # Remove individual and set a flag to ensure no individual is selected --------
    shiny::observe({
      indi_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$ged <- tidyged::remove_indi(r$ged, indi_xref)
      shiny::showNotification("Individual deleted")
      r$indi_to_select <- NULL
    }) |> 
      shiny::bindEvent(input$delete)
    
    # Update sex value ------------------------------------------------------------
    shiny::observe({
      sex <- process_input(input$sex)
      err <- tidyged.internals::chk_sex_value(sex, 1)
      shinyFeedback::feedbackDanger("sex", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "indi_rows", r$ged$record[r$indi_rows[1]], 1, "SEX", sex)
    }) |> 
      shiny::bindEvent(input$sex, ignoreNULL = FALSE, ignoreInit = TRUE)
    

  })
}

