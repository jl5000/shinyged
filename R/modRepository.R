

tab_repo <- function() {
  
  shiny::tabPanel("Repositories",
                  
                  shiny::tags$br(),
                  shiny::fluidRow(
                    shiny::column(6,
                                  shiny::selectInput("repo_list", label = NULL, choices = NULL, width = "500px")
                    ),
                    shiny::column(6,
                                  shiny::actionButton("add_repo", "Add repository"),
                                  shiny::actionButton("delete_repo", "Delete repository")
                    )
                    
                  ),
                  
            shiny::tabsetPanel(
              shiny::tabPanel("Summary")
              
            )
                  
                  
                  )
  
  
}