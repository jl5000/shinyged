

file_records_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::br(),
    
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectInput(ns("record_type"), label = "Choose a record type",
                                       choices = c("Individuals","Family Groups","Multimedia","Sources","Repositories","Notes"))
      )
    ),
    
    shiny::fluidRow(
      shiny::column(12,
                    DT::DTOutput(ns("table"))
      )
    ),
    
    
    
  )
  
}


file_records_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$table <- DT::renderDataTable({
      req(r$ged, input$record_type)
      
      if(input$record_type == "Individuals"){
        tbl <- tidyged::df_indi(r$ged)
      } else if(input$record_type == "Family Groups"){
        tbl <- tidyged::df_famg(r$ged)
      } else if(input$record_type == "Multimedia"){
        tbl <- tidyged::df_media(r$ged)
      } else if(input$record_type == "Sources"){
        tbl <- tidyged::df_sour(r$ged)
      } else if(input$record_type == "Repositories"){
        tbl <- tidyged::df_repo(r$ged)
      } else if(input$record_type == "Notes"){
        tbl <- tidyged::df_note(r$ged)
      }
        
      DT::datatable(tbl, rownames = FALSE)
    })
    
  })
}
