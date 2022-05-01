

file_records_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::br(),
    
    shiny::fluidRow(
      shiny::column(3,
                    shiny::selectInput(ns("record_type"), label = "Choose a record type",
                                       choices = c("Individuals","Family Groups","Multimedia","Sources","Repositories","Notes")),
                    
                    
      ),
      shiny::column(2, style = 'margin-top:25px',
        shiny::actionButton(ns("refresh"), "Display"),
      ),
      shiny::column(7, style = 'margin-top:25px',
        shiny::helpText("Note: Summary tables can take several seconds to generate (depending on size)")
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
    }) |> 
      shiny::bindEvent(input$refresh)
    
  })
}
