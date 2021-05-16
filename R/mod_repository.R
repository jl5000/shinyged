
repository_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("records"), label = NULL, choices = NULL,
                                          multiple = TRUE, width = "500px", options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add repository"),
                    shiny::actionButton(ns("delete"), "Delete repository")
      )
      
    ),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary", repository_summary_ui(ns("repo_summary"))),
      shiny::tabPanel("Details", repository_details_ui(ns("repo_details"))),
      shiny::tabPanel("Notes", notes_ui(ns("repo_notes")))
    )
  )
}

repository_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    records <- shiny::reactive({
      req(r$ged)
      tidyged::xrefs_repo(r$ged) %>% 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
    })
    
    observeEvent(ged(), {
      if(!is.null(records())) {
        shiny::updateSelectizeInput(session = session, inputId = "records", choices = records())
      } else {
        shiny::updateSelectizeInput(session = session, inputId = "records", choices = character(), selected = character())
      }
    })
    
  })
}

repository_app <- function(ged = NULL) {
  r <- shiny::reactiveValues(ged = ged)
  ui <- shiny::fluidPage(
    repository_ui("repo")
  )
  server <- function(input, output, session) {
    repository_server("repo", r)
  }
  shiny::shinyApp(ui, server)  
}
