


file_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary", file_summary_ui(ns("file_summary"))),
      shiny::tabPanel("File details", file_details_ui(ns("file_details"))),
      shiny::tabPanel("Source data details", file_data_ui(ns("file_data")))
    )
  )
}

file_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    file_summary_server("file_summary", ged)
    file_details_server("file_details", ged)
    file_data_server("file_data", ged)
  })
}

file_app <- function(ged = NULL) {
  ui <- fluidPage(
    file_ui("file")
  )
  server <- function(input, output, session) {
    file_server("file", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}


