


file_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary", file_summary_ui(ns("file_summary"))),
      shiny::tabPanel("File details", file_details_ui(ns("file_details"))),
      shiny::tabPanel("Source data details", file_data_ui(ns("file_data")))
    )
  )
}

file_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(r$ged, priority = 2, { # want this to fire first
      req(r$ged)
      r$head_rows <- which(r$ged$record == "HD")
      r$head_file_sour_rows <- tidyged.internals::identify_section(r$ged, 1, "SOUR", "tidyged", xrefs = "HD")
    })
    
    file_summary_server("file_summary", r)
    file_details_server("file_details", r)
    file_data_server("file_data", r)
  })
}


