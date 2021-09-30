


record_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    DT::DTOutput(ns("record"))
  )
  
}


record_server <- function(id, r, record_rows) {
  moduleServer(id, function(input, output, session) {
    
    output$record <- DT::renderDataTable({
      req(r$ged, r[[record_rows]])
      DT::datatable(r$ged[r[[record_rows]],], rownames = TRUE, selection = "single")
    })
    
  })
}



