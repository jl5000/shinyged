

family_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("records"), label = NULL, choices = NULL,
                                          multiple = TRUE, width = "500px", options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add family group"),
                    shiny::actionButton(ns("delete"), "Delete family group")
      )
      
    ),
    
    shiny::tabsetPanel(
      shiny::tabPanel("Summary", family_summary_ui(ns("family_summary"))),
      shiny::tabPanel("Members", family_members_ui(ns("family_members"))),
      shiny::tabPanel("Events", family_events_ui(ns("family_events"))),
      shiny::tabPanel("Notes", notes_ui(ns("family_notes"))),
      shiny::tabPanel("Citations", citations_ui(ns("family_citations"))),
      shiny::tabPanel("Media", media_links_ui(ns("family_media")))

      
    )
  )
}

family_server <- function(id, ged = NULL) {
  moduleServer(id, function(input, output, session) {
    
    records <- shiny::reactive({
      req(ged)
      tidyged::xrefs_famg(ged()) %>% 
        tidyged::describe_records(ged(), ., short_desc = TRUE)
    })
    
    observeEvent(ged(), {
      if(!is.null(records())) {
        shiny::updateSelectizeInput(session = session, inputId = "records", choices = records())
      } else {
        shiny::updateSelectizeInput(session = session, inputId = "records", choices = character(), selected = character())
      }
    })
    
    family_summary_server("family_summary", ged)
    family_members_server("family_members", ged)
    family_events_server("family_events", ged)
    
  })
}

family_app <- function(ged = NULL) {
  ui <- shiny::fluidPage(
    family_ui("famg")
  )
  server <- function(input, output, session) {
    family_server("famg", shiny::reactive(ged))
  }
  shiny::shinyApp(ui, server)  
}


