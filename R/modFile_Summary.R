



file_summaryUI <- function(id) {
  shiny::tagList(
    shiny::tags$br(),
    shiny::verbatimTextOutput(shiny::NS(id, "str")),
    shiny::tags$br(),
    shiny::verbatimTextOutput(shiny::NS(id, "summary"))
    
  )
}

file_summaryServer <- function(id, ged = NULL) {
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

file_summaryApp <- function(ged = NULL) {
  ui <- fluidPage(
    file_summaryUI("file_summary")
  )
  server <- function(input, output, session) {
    file_summaryServer("file_summary", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}


