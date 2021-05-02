

familyUI <- function(id) {
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectInput(shiny::NS(id, "records"), label = NULL, choices = NULL, width = "500px")
      ),
      shiny::column(6,
                    shiny::actionButton(shiny::NS(id, "add"), "Add family group"),
                    shiny::actionButton(shiny::NS(id, "delete"), "Delete family group")
      )
      
    ),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary"),
      shiny::tabPanel("Members"),
      shiny::tabPanel("Events"),
      shiny::tabPanel("Notes"),
      shiny::tabPanel("Citations"),
      shiny::tabPanel("Media")

      
    )
  )
}

familyServer <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    records <- shiny::reactive({
      req(ged)
      tidyged::xrefs_famg(ged()) %>% 
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

familyApp <- function(ged = NULL) {
  ui <- fluidPage(
    familyUI("famg")
  )
  server <- function(input, output, session) {
    familyServer("famg", shiny::reactive(ged))
  }
  shinyApp(ui, server)  
}


