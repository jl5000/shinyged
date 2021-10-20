


source_repo_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::actionButton(ns("repos"), label = "Repositories")
  )
  
}


source_repo_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}


