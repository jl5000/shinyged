





individual_facts_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    timevis::timevisOutput(ns("timeline"), width = "1500px")
    
  )
}

individual_facts_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$timeline <- timevis::renderTimevis({
      req(r$ged, r$indi_rows)
      
      xref <- r$ged$record[r$indi_rows[1]]
      visged::timevis_indi(r$ged, xref)
    })
    
  })
}



