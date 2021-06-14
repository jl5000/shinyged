
note_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("records"), label = NULL, choices = NULL, 
                                          multiple = TRUE, width = "500px", options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add note"),
                    shiny::actionButton(ns("update"), "Update note"),
                    shiny::actionButton(ns("delete"), "Delete note")
      )
      
    ),
    shiny::textAreaInput(ns("note_text"), "Edit note...", height = "150px") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
  )
}

note_server <- function(id, notes = NULL) {
  moduleServer(id, function(input, output, session) {
    
   
    
  })
}

