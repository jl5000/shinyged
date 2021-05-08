



file_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::verbatimTextOutput(ns("str")),
    shiny::tags$br(),
    shiny::verbatimTextOutput(ns("summary"))
    
  )
}

file_summary_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    output$str <- shiny::renderPrint({
      req(ged)
      str(ged())
    })
    
    output$summary <- shiny::renderPrint({
      req(ged)
      summary(ged())
    })
    
  })
}

file_summary_app <- function(ged = NULL) {
  ui <- fluidPage(
    file_summary_ui("file_summary")
  )
  server <- function(input, output, session) {
    file_summary_server("file_summary", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}


