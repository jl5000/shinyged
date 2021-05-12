



shiny::shinyUI(shiny::fluidPage(
  shinyjs::useShinyjs(),
  # Application title
  #tags$img(src = "logo.png"),
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
                                    shiny::tabPanel("GEDCOM File", file_ui("file")),
                                    shiny::tabPanel("Submitter", submitter_ui("subm")),
                                    shiny::tabPanel("Individuals", individual_ui("indi")),
                                    shiny::tabPanel("Families", family_ui("famg")),
                                    shiny::tabPanel("Notes", note_ui("note")),
                                    shiny::tabPanel("Multimedia", multimedia_ui("media")),
                                    shiny::tabPanel("Sources", source_ui("sour")),
                                    shiny::tabPanel("Repositories", repository_ui("repo"))
                                    
                                    
                                    
                                  )
                    )
                    
                    
                    
    )
  )
)
)
