
individual_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("records"), label = NULL, choices = NULL, 
                                       multiple = TRUE, width = "500px", options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add individual"),
                    shiny::actionButton(ns("delete"), "Delete individual")
      )
      
    ),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary", individual_summary_ui(ns("individual_summary"))),
      shiny::tabPanel("Names", individual_names_ui(ns("individual_names"))),
      shiny::tabPanel("Facts", individual_facts_ui(ns("individual_facts"))),
      shiny::tabPanel("Links", individual_links_ui(ns("individual_links"))),
      shiny::tabPanel("Notes"),
      shiny::tabPanel("Citations"),
      shiny::tabPanel("Media")
      
    )
  )
}

individual_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    records <- shiny::reactive({
      req(ged)
      tidyged::xrefs_indi(ged()) %>% 
        tidyged::describe_records(ged(), ., short_desc = TRUE)
    })
    
    observeEvent(ged(), {
      if(!is.null(records())) {
        shiny::updateSelectizeInput(session = session, inputId = "records", choices = records())
      } else {
        shiny::updateSelectizeInput(session = session, inputId = "records", choices = character(), selected = character())
      }
    })
    
    observeEvent(input$add, {
      ged <- tidyged::add_indi(ged())
    })
    
    individual_summary_server("individual_summary", ged)
    individual_names_server("individual_names", ged)
    individual_facts_server("individual_facts", ged)
    individual_links_server("individual_links", ged)
    
  })
}

individualApp <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    individual_ui("indi")
  )
  server <- function(input, output, session) {
    individual_server("indi", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}
