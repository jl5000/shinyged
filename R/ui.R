


shiny::shinyUI(shiny::fluidPage(
    
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
                      shiny::downloadButton("export_gedcom", "Export to GEDCOM file")
        )
    ),
    
    shiny::fluidRow(
        shiny::column(12,
                      shiny::tabsetPanel(
                          tab_overview(),
                          tab_indi(),
                          tab_famg()
                          
                      )
        )
        
        
        
    )
)
)
