





individual_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::textOutput(ns("temp_info"))
  )
}

individual_summary_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$temp_info <- shiny::renderText({
      req(r$ged, r$indi_rows)
      xref <- r$ged$record[r$indi_rows][1]
      tidyged::describe_indi(r$ged, xref)
      
    })
    
  })
}


