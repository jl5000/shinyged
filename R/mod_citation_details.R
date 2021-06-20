


citation_details_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::selectizeInput(ns("source"), label = "Source", choices = NULL, 
                          multiple = TRUE, width = "500px", options = list(maxItems = 1)),
    shiny::splitLayout(
      shiny::textInput(ns("page"), "Where within source?"),
      shiny::textInput(ns("entry_date"), "Entry date (e.g. 6 APR 1983)")
    ),
    shiny::splitLayout(
      shiny::textInput(ns("event_type"), "Event type"),
      shiny::textInput(ns("role"), "Role in event")
    ),
    shiny::textAreaInput(ns("source_text"), "Source text", resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 85%;'),
    shiny::splitLayout(
      shiny::numericInput(ns("certainty"), "Certainty assessment", value = NULL, 
                          min = 0, max = 3, step = 1),
      shiny::helpText("What is the credibility of this source?",
                      shiny::br(),
                      "0 = unreliable/estimated data",
                      shiny::br(),
                      "1 = Questionable reliability of evidence",
                      shiny::br(),
                      "2 = Secondary evidence, officially recorded sometime after event",
                      shiny::br(),
                      "3 = Direct and primary evidence used / dominance of evidence")
      
    )
  )
  
  
}


citation_details_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    
    
    
    
  })
}

