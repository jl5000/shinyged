





individual_facts_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # table of facts, notes + media link buttons
    # details, location, and citations tabs
    # details - age, description, type, date agency, relig, cause
    shiny::br(),
    shiny::helpText("Here you can manage facts associated with an individual."),
    shiny::tags$hr(),
    DT::DTOutput(ns("table")),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(8,
                    shiny::actionButton(ns("add_fact"), "Add fact"),
                    shiny::actionButton(ns("remove_fact"), "Delete fact"),
                    shiny::actionButton(ns("update_fact"), "Update fact")
      ),
      shiny::column(1),
      shiny::column(3,
                    notes_ui(ns("fact_notes")),
                    media_links_ui(ns("fact_media")),
      )
    ),
    
    shiny::tags$hr(),
    
    shiny::fluidRow(id = ns("fact_tabs"),
                    shiny::column(width = 12,
                                  shiny::tabsetPanel(id = ns("tabset"),
                                                     shiny::tabPanel("Details", individual_fact_details_ui(ns("indi_fact_details"))),
                                                     shiny::tabPanel("Locations", fact_locations_ui(ns("indi_fact_locations"))),
                                                     shiny::tabPanel("Citations", citations_ui(ns("indi_fact_citations")))
                                  )
                    ) |> shinyjs::hidden()
                    
                    
    ) 
  )
}

individual_facts_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    individual_fact_details_server("indi_fact_details", r)
    fact_locations_server("indi_fact_locations", r)
    citations_server("indi_fact_citations", r, "fact_rows")
    
  })
}



