

tab_famg <- function() {
  
  shiny::tabPanel("Family groups",
                  
                  shiny::tags$br(),
                  shiny::fluidRow(
                    shiny::column(6,
                                  shiny::selectInput("famg_list", label = NULL, choices = NULL, width = "500px")
                    ),
                    shiny::column(6,
                                  shiny::actionButton("add_famg", "Add family group"),
                                  shiny::actionButton("delete_famg", "Delete family group")
                    )
                    
                  )
                  
                  
                  
                  
                  )
  
  
  
}
