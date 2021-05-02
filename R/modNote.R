
noteUI <- function(id) {
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectInput(shiny::NS(id, "records"), label = NULL, choices = NULL, width = "500px")
      ),
      shiny::column(6,
                    shiny::actionButton(shiny::NS(id, "add"), "Add note"),
                    shiny::actionButton(shiny::NS(id, "delete"), "Delete note")
      )
      
    ),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary")
      
    )
  )
}

noteServer <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    records <- shiny::reactive({
      req(ged)
      tidyged::xrefs_note(ged()) %>% 
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

noteApp <- function(ged = NULL) {
  ui <- fluidPage(
    noteUI("note")
  )
  server <- function(input, output, session) {
    noteServer("note", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}
