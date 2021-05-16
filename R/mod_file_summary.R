



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

file_summary_app <- function(ged = NULL) {
  r <- shiny::reactiveValues(ged = ged)
  ui <- shiny::fluidPage(
    file_summary_ui("file_summary")
  )
  server <- function(input, output, session) {
    file_summary_server("file_summary", r)
  }
  shiny::shinyApp(ui, server)  
}


