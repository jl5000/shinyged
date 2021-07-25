




submitter_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::helpText("The submitter is the individual who has created and owns the file."),
    shiny::textInput(ns("subm_name"), "Name"),
    shiny::tabsetPanel(
      shiny::tabPanel("Contact details", address_ui(ns("subm_address"))),
      shiny::tabPanel("Notes", notes_ui(ns("subm_notes"))),
      shiny::tabPanel("Media", media_links_ui(ns("subm_media")))
    )
    
  )
}

submitter_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(r$ged, priority = 2, { # want this to fire first
      req(r$ged)
      subm_xref <- tidyged::xrefs_subm(r$ged)
      r$subm_rows <- which(r$ged$record == subm_xref)
      r$subm_addr_rows <- which(r$ged$record == subm_xref &
                                  r$ged$tag %in% .pkgenv$tags_addr)
    })

    subm <- shiny::reactive({
      req(r$ged, r$subm_rows)
      r$ged[r$subm_rows,]
    })
    
    shiny::observeEvent(subm(), once = TRUE, {
      shiny::updateTextInput(session = session, "subm_name",
                             value = dplyr::filter(subm(), tag == "NAME")$value)
    })
    
    shiny::observeEvent(input$subm_name, ignoreNULL = FALSE, ignoreInit = TRUE, {
      subm_name <- process_input(input$subm_name)
      err <- tidyged.internals::chk_submitter_name(subm_name, 1)
      shinyFeedback::feedbackDanger("subm_name", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "subm_rows", 1, "NAME", subm_name)
    })
    
    address_server("subm_address", r, "subm_addr_rows")
    notes_server("subm_notes", r, "subm_rows")
    media_links_server("subm_media", r, "subm_rows")
    
  })
}

