


shiny::shinyUI(shiny::fluidPage(
    shinyjs::useShinyjs(),
    # Application title
    shiny::titlePanel("shinyged"),
    
    
    shiny::fluidRow(
        shiny::column(4,
                      shiny::fileInput("read_file", "Import GEDCOM file",
                                       multiple = FALSE,
                                       accept = ".ged")
        ),
        shiny::column(4, shiny::tags$br(),
                      shiny::actionButton("create_gedcom", icon = shiny::icon("plus-square"), "Create GEDCOM file")
        ),
        shiny::column(4, shiny::tags$br(),
                      shinyjs::disabled(
                          shiny::downloadButton("export_gedcom", "Export to GEDCOM file")
                      )
        )
    ),
    
    shinyjs::hidden(
        shiny::fluidRow(id = "tabs",
            shiny::column(12,
                          shiny::tabsetPanel(
                              tab_overview(),
                              tab_indi(),
                              tab_famg(),
                              tab_sour(),
                              tab_repo(),
                              tab_media(),
                              tab_note()
                              
                          )
            )
            
            
            
        )
    )
)
)
