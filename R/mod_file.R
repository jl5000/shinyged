


file_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::tabsetPanel(id = ns("tabset"),
      shiny::tabPanel("Summary", file_summary_ui(ns("file_summary"))),
      shiny::tabPanel("File details", file_details_ui(ns("file_details"))),
      shiny::tabPanel("Source data details", file_data_ui(ns("file_data"))),
      shiny::tabPanel("Raw data", record_ui(ns("file_raw")))
    )
  )
}

file_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    file_summary_server("file_summary", r)
    
    shiny::observe({
      if(input$tabset == "File details") file_details_server("file_details", r)
      else if(input$tabset == "Source data details") file_data_server("file_data", r)
      else if(input$tabset == "Raw data") record_server("file_raw", r, "head_rows")
    }) %>% 
      shiny::bindEvent(input$tabset, ignoreInit = TRUE)
    

    shiny::observe(priority = 2, { # want this to fire first
      req(r$ged)
      r$head_rows <- which(r$ged$record == "HD")
      r$head_file_sour_rows <- tidyged.internals::identify_section(r$ged, 1, "SOUR", 
                                                                   xrefs = "HD", first_only = TRUE)
    }) %>% 
      shiny::bindEvent(r$ged)
    

  })
}


