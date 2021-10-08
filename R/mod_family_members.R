


family_members_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::selectizeInput(ns("husband"), label = NULL, choices = NULL, 
                          multiple = TRUE, width = "500px", options = list(maxItems = 1)),
    
    shiny::selectizeInput(ns("husband"), label = NULL, choices = NULL, 
                          multiple = TRUE, width = "500px", options = list(maxItems = 1))
  )
  
}


family_members_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}



