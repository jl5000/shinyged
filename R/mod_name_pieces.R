

name_pieces_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::fluidRow(
      shiny::column(6,
             shiny::textInput(ns("prefix"), "Name prefix"),
             shiny::textInput(ns("given"), "Given name(s)"),
             shiny::textInput(ns("nick"), "Nickname")
      ),
      shiny::column(6,
             shiny::textInput(ns("surn_prefix"), "Surname prefix"),
             shiny::textInput(ns("surname"), "Surname"),
             shiny::textInput(ns("suffix"), "Suffix (e.g. Jr.)")
      )
      
    )
 
  )
  
}


name_pieces_server <- function(id, name = NULL) {
  moduleServer(id, function(input, output, session) {
    

    
  })
}

