



file_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::verbatimTextOutput(ns("str")),
    shiny::tags$br(),
    shiny::verbatimTextOutput(ns("summary"))
    
  )
}

file_summary_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$str <- shiny::renderPrint({
      req(r$ged)
      str(r$ged)
    })
    
    output$summary <- shiny::renderPrint({
      req(r$ged)
      summary(r$ged)
    })
    
  })
}


