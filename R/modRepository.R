
repositoryUI <- function(id) {
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectInput(shiny::NS(id, "records"), label = NULL, choices = NULL, width = "500px")
      ),
      shiny::column(6,
                    shiny::actionButton(shiny::NS(id, "add"), "Add repository"),
                    shiny::actionButton(shiny::NS(id, "delete"), "Delete repository")
      )
      
    ),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary")
      
    )
  )
}

repositoryServer <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    records <- shiny::reactive({
      req(ged)
      tidyged::xrefs_repo(ged()) %>% 
        tidyged::describe_records(ged(), ., short_desc = TRUE)
    })
    
    observeEvent(ged(), {
      if(!is.null(records())) {
        shiny::updateSelectInput(session = session, inputId = "records", choices = records())
      } else {
        shiny::updateSelectInput(session = session, inputId = "records", choices = character(), selected = character())
      }
    })
    
  })
}

repositoryApp <- function(ged = NULL) {
  ui <- fluidPage(
    repositoryUI("repo")
  )
  server <- function(input, output, session) {
    repositoryServer("repo", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}
