
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
                    shiny::column(width = 12,
                                  ref_numbers_ui(ns("media_ref_numbers")),
                                  notes_ui(ns("media_notes")),
                    )
    ) |> shinyjs::hidden(),
    
    shiny::br(),
    
    shinyjs::hidden(
      shiny::fluidRow(id = ns("media_tabs"),
                      shiny::column(12,
                                    shiny::tabsetPanel(id = ns("tabset"),
                                      shiny::tabPanel("Summary", multimedia_summary_ui(ns("media_summary"))),
                                      shiny::tabPanel("Description", multimedia_description_ui(ns("media_description"))),
                                      shiny::tabPanel("Citations", citations_ui(ns("media_citations"))),
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
    
    multimedia_summary_server("media_summary", r)
    ref_numbers_server("media_ref_numbers", r, "media_rows")
    notes_server("media_notes", r, "media_rows")

    shiny::observe(multimedia_description_server("media_description", r)) |>
      shiny::bindEvent(input$tabset == "Description", once = TRUE, ignoreInit = TRUE)
    
    shiny::observe(record_server("media_raw", r, "media_rows")) |>
      shiny::bindEvent(input$tabset == "Raw data", once = TRUE, ignoreInit = TRUE)
    
    shiny::observe(citations_server("media_citations", r, "media_rows")) |>
      shiny::bindEvent(input$tabset == "Citations", once = TRUE, ignoreInit = TRUE)
    
    # Update list of media
    records <- shiny::reactive({
      req(r$ged)
      
      tidyged::describe_records(r$ged, tidyged::xrefs_media(r$ged), short_desc = TRUE)
    })
    
    # Update choices with list of media and select one
    shiny::observe({
      if(!is.null(records())) {
        
        if(is.null(r$media_to_select)) {
          current_selection <- input$record
        } else {
          current_selection <- r$media_to_select
        }
        
        shiny::updateSelectizeInput(inputId = "record", choices = records(), selected = current_selection)
      } else {
        shiny::updateSelectizeInput(inputId = "record", choices = character(), selected = character())
      }
      r$media_to_select <- NULL
    }) |> 
      shiny::bindEvent(records())
    
    # Update media_rows
    shiny::observe(priority = 2, {
      req(input$record)
      media_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$media_rows <- which(r$ged$record == media_xref)
    }) |> 
      shiny::bindEvent(input$record, r$ged)
    
    # Show/hide tabs and toggle delete button
    shiny::observe({
      shinyjs::toggle("media_tabs", condition = !is.null(input$record))
      shinyjs::toggle("media_data", condition = !is.null(input$record))
      shinyjs::toggleState("delete", !is.null(input$record))
    }) |> 
      shiny::bindEvent(input$record, ignoreNULL = FALSE)
    
    # Add media and set a flag to ensure it is selected
    shiny::observe({
      r$ged <- tidyged::add_media(r$ged) #TODO: Can't add empty media - need modal to choose file ref and format
      media_xrefs <- tidyged::xrefs_media(r$ged)
      last_media <- tail(media_xrefs, 1)
      r$media_to_select <- tidyged::describe_records(r$ged, last_media, short_desc = TRUE)
    }) |> 
      shiny::bindEvent(input$add)
    
    # Remove media and set a flag to ensure no media is selected
    shiny::observe({
      media_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$ged <- tidyged::remove_media(r$ged, media_xref)
      shiny::showNotification("Multimedia deleted")
      r$media_to_select <- NULL
    }) |> 
      shiny::bindEvent(input$delete)
    
 
  })
}


