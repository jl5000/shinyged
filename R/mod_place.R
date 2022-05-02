


place_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::fluidRow(
      shiny::column(5,
                    shiny::textOutput(ns("place_name"))
      ),
      shiny::column(2,
                    shiny::actionButton(ns("edit_place"), "Edit address")
      )
    ),
    
    
  )
  
}


place_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}




