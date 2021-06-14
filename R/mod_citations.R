


citations_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::helpText("Here you can manage citations associated with an item.", 
                    "Use the buttons to add and remove citations by selecting items in the list."),
    shiny::tags$hr(),
    DT::DTOutput(ns("citations_list")),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(ns("add_citation"), "Add citation"),
                    shinyjs::disabled(
                      shiny::actionButton(ns("remove_citation"), "Remove citation")
                    )
      )
    ),
    shiny::tags$br(),
    shinyjs::hidden(
      shiny::fluidRow(id = ns("citation_tabs"),
                      shiny::column(12,
                                    shiny::tabsetPanel(
                                      shiny::tabPanel("Details", citation_details_ui(ns("citation_details"))),
                                      shiny::tabPanel("Notes", notes_ui(ns("citation_notes"))),
                                      shiny::tabPanel("Media", media_links_ui(ns("citation_media")))
                                    )
                      )
      )
    )
  )
  
  
}


citations_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    

    
    
    
  })
}

