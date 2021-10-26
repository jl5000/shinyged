




submitter_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::helpText("The submitter is the individual who has created and owns the file."),
    shiny::fluidRow(
      shiny::column(4,
                    shiny::textInput(ns("subm_name"), "Name"),
                    ),
      shiny::column(8, style = 'margin-top:25px',
                    notes_ui(ns("subm_notes")),
                    media_links_ui(ns("subm_media"))
      )
    ),
    address_ui(ns("subm_address")),
    
  )
}

submitter_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    address_server("subm_address", r, "subm_rows")
    notes_server("subm_notes", r, "subm_rows")
    media_links_server("subm_media", r, "subm_rows")
    
    shiny::observe(priority = 2, { # want this to fire first
      req(r$ged)
      subm_xref <- tidyged::xrefs_subm(r$ged)
      r$subm_rows <- which(r$ged$record == subm_xref)
    }) %>% 
      shiny::bindEvent(r$ged)

    subm <- shiny::reactive({
      req(r$ged, r$subm_rows)
      r$ged[r$subm_rows,]
    })
    
    shiny::observe({
      req(subm)
      shiny::updateTextInput(inputId = "subm_name",
                             value = dplyr::filter(subm(), tag == "NAME")$value)
    }) %>% 
      shiny::bindEvent(r$file_count)
    
    shiny::observe({
      subm_name <- process_input(input$subm_name, input_required = TRUE)
      err <- tidyged.internals::chk_submitter_name(subm_name, 1)
      shinyFeedback::feedbackDanger("subm_name", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "subm_rows", subm()$record[1], 1, "NAME", subm_name)
    }) %>% 
      shiny::bindEvent(input$subm_name, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
  })
}

