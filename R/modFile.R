


fileUI <- function(id) {
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary", file_summaryUI(shiny::NS(id, "file_summary"))),
      
      shiny::tabPanel("File details", file_detailsUI(shiny::NS(id, "file_details"))),
                      

      shiny::tabPanel("Submitter details",
                      shiny::tags$br(),
                      shiny::textInput(shiny::NS(id, "subm_name"), "Name"),
                      shiny::fluidRow(
                        shiny::column(6,
                                      shiny::actionButton(shiny::NS(id, "edit_subm_address"), "Edit address"),
                                      shiny::actionButton(shiny::NS(id, "rm_subm_address"), "Remove address")
                        ),
                        shiny::column(6,
                                      form_address(shiny::NS(id, "edit_subm_address")),
                                      shiny::textOutput(shiny::NS(id, "subm_addr_out"))
                        )
                        
                        
                        
                      )
                      
                      #subm notes, media links
      ),
      shiny::tabPanel("Source data details",
                      shiny::tags$br(),
                      shiny::splitLayout(
                        shiny::textInput(shiny::NS(id, "ged_source_name"), "Source name"),
                        shiny::textInput(shiny::NS(id, "ged_source_date"), "Publication date (e.g. 6 APR 1983)")
                      ),
                      shiny::textAreaInput(shiny::NS(id, "ged_source_copy"), "Copyright", resize = "vertical") %>%
                        shiny::tagAppendAttributes(style = 'width: 100%;')
      )
    ) 
    

  )
}

fileServer <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    file_summaryServer("file_summary", ged)
    file_detailsServer("file_details", ged)
    
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


