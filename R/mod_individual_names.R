






individual_names_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br()
    # table of names/types
    # click on a name, updates a table of variations/types
    # too much nesting
  )
}

individual_names_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}



