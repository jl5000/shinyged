

tab_indi <- function() {
  
  shiny::tabPanel("Individuals",
                  
                  shiny::tags$br(),
                  shiny::fluidRow(
                    shiny::column(6,
                                  shiny::selectInput("indi_list", label = NULL, choices = NULL, width = "500px")
                    ),
                    shiny::column(6,
                                  shiny::actionButton("add_indi", "Add individual"),
                                  shiny::actionButton("delete_indi", "Delete individual")
                    )
                    
                  ),
                  
                  shiny::tabsetPanel(
                    shiny::tabPanel("Summary"),
                    shiny::tabPanel("Names"),
                    shiny::tabPanel("Facts"),
                    shiny::tabPanel("Links"),
                    shiny::tabPanel("Notes"),
                    shiny::tabPanel("Citations"),
                    shiny::tabPanel("Media")
              
            )
                  
                  
                  )
  
  
}