
individualUI <- function(id) {
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectInput(shiny::NS(id, "records"), label = NULL, choices = NULL, width = "500px")
      ),
      shiny::column(6,
                    shiny::actionButton(shiny::NS(id, "add"), "Add individual"),
                    shiny::actionButton(shiny::NS(id, "delete"), "Delete individual")
      )
      
    ),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary"),
      shiny::tabPanel("Names"),
      shiny::tabPanel("Facts"),
      shiny::tabPanel("Links"),
      shiny::tabPanel("Notes"),
      shiny::tabPanel("Citations"),
      shiny::tabPanel("Media")
      
    )
  )
}

individualServer <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    records <- shiny::reactive({
      req(ged)
      tidyged::xrefs_indi(ged()) %>% 
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

individualApp <- function(ged = NULL) {
  ui <- fluidPage(
    individualUI("indi")
  )
  server <- function(input, output, session) {
    individualServer("indi", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}
