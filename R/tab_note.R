

tab_note <- function() {
  
  shiny::tabPanel("Notes",
                  
                  shiny::tags$br(),
                  shiny::fluidRow(
                    shiny::column(6,
                                  shiny::selectInput("note_list", label = NULL, choices = NULL, width = "500px")
                    ),
                    shiny::column(6,
                                  shiny::actionButton("add_note", "Add note"),
                                  shiny::actionButton("delete_note", "Delete note")
                    )
                    
                  ),
                  
            shiny::tabsetPanel(
              shiny::tabPanel("Summary")
              
            )
                  
                  
                  )
  
  
}