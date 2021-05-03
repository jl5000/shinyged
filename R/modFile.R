


fileUI <- function(id) {
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary", file_summaryUI(shiny::NS(id, "file_summary"))),
      
      shiny::tabPanel("File details", file_detailsUI(shiny::NS(id, "file_details"))),
                      

      shiny::tabPanel("Submitter details", file_submitterUI(shiny::NS(id, "file_submitter"))),
                      
   
      shiny::tabPanel("Source data details", file_dataUI(shiny::NS(id, "file_data")))
                      

    ) 
    

  )
}

fileServer <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    file_summaryServer("file_summary", ged)
    file_detailsServer("file_details", ged)
    file_submitterServer("file_submitter", ged)
    file_dataServer("file_data", ged)
  })
}

fileApp <- function(ged = NULL) {
  ui <- fluidPage(
    fileUI("file")
  )
  server <- function(input, output, session) {
    fileServer("file", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}


