

tab_sour <- function() {
  
  shiny::tabPanel("Sources",
                  
                  shiny::tags$br(),
                  shiny::fluidRow(
                    shiny::column(6,
                                  shiny::selectInput("sour_list", label = NULL, choices = NULL, width = "500px")
                    ),
                    shiny::column(6,
                                  shiny::actionButton("add_sour", "Add source"),
                                  shiny::actionButton("delete_sour", "Delete source")
                    )
                    
                  ),
                  
            shiny::tabsetPanel(
              shiny::tabPanel("Summary")
              
            )
                  
                  
                  )
  
  
}