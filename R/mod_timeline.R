





timeline_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    timevis::timevisOutput(ns("timeline"), width = "1500px")
  )
}

timeline_server <- function(id, r, record_rows) {
  moduleServer(id, function(input, output, session) {
    
    output$timeline <- timevis::renderTimevis({
      req(r$ged, r[[record_rows]])
      
      xref <- r$ged$record[r[[record_rows]][1]]
      
      if(tidyged::is_indi(r$ged, xref)){
        visged::timevis_indi(r$ged, xref)
      } else {
        visged::timevis_famg(r$ged, xref)
      }
      
    })
    
  })
}



