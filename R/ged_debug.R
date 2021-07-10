

ged_debug_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::helpText("indi_rows"),
    shiny::textOutput(ns("indi_rows")),
    shiny::helpText("famg_rows"),
    shiny::textOutput(ns("famg_rows")),
    shiny::helpText("citation_rows"),
    shiny::textOutput(ns("citation_rows")),
    
    DT::DTOutput(ns("gedfile"))
  )
  
}


ged_debug_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$citation_rows <- shiny::renderText({
      req(r$citation_rows)
      r$citation_rows
    })
    
    output$indi_rows <- shiny::renderText({
      req(r$indi_rows)
      r$indi_rows
    })
    
    output$famg_rows <- shiny::renderText({
      req(r$famg_rows)
      r$famg_rows
    })
    
    output$gedfile <- DT::renderDataTable({
      req(r$ged)
      DT::datatable(r$ged, rownames = TRUE, selection = "single")
    })
    
  })
}
