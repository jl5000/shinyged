


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
                              shiny::tabPanel("GEDCOM File", fileUI("file")),
                              shiny::tabPanel("Individuals", individualUI("indi")),
                              shiny::tabPanel("Families", familyUI("famg")),
                              shiny::tabPanel("Sources", sourceUI("sour")),
                              shiny::tabPanel("Repositories", repositoryUI("repo")),
                              shiny::tabPanel("Multimedia", multimediaUI("media")),
                              shiny::tabPanel("Notes", noteUI("note"))
                              
                          )
            )
            
            
            
        )
    )
)
)
