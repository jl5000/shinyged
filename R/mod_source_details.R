


source_details_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::fluidRow(
      shiny::column(6,
                    source_repo_ui(ns("repos"))
      ),
      shiny::column(6,
                    shiny::textInput(ns("originator"), "Originator")
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::textInput(ns("title"), "Title")
      ),
      shiny::column(6,
                    shiny::textInput(ns("abbr_title"), "Short title")
      )
    ),
    
    shiny::textAreaInput(ns("pub_details"), "Publication details", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 85%;'),
    
    shiny::textAreaInput(ns("text"), "Source text", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 85%;'),
   
  )
  
}


source_details_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    source_repo_server("repos", r)
    
  })
}


