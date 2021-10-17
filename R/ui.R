



shiny::fluidPage(
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  shinybusy::add_busy_spinner(spin = "fading-circle"),
  # Application title
  #tags$img(src = "logo.png"),
  shiny::titlePanel("shinyged"),
  shiny::helpText(shiny::h4("This app provides an interactive interface to the packages of the gedcompendium.",
                            "This allows you to record genealogical data in the Genealogical Data Communication (GEDCOM) standard.")),
  shiny::helpText(shiny::h4("Use the controls below to create a new GEDCOM file or import an existing file.",
                            "None of your changes will be saved unless you export to a GEDCOM file.")),
  shiny::helpText(shiny::h4("The app is only compatible with GEDCOM v5.5.5 files.")),
  shiny::tags$hr(),
  shiny::fluidRow(
    
    shiny::column(3,         
                  shiny::actionButton("create_gedcom", icon = shiny::icon("plus-square"), "Create GEDCOM file")
    ),
    
    shiny::column(6,
                  shiny::fileInput("read_file", NULL, buttonLabel = "Import GEDCOM file",
                                   multiple = FALSE, width = "100%",
                                   accept = ".ged")
    ),
    
    shiny::column(3,
                  shiny::downloadButton("export_gedcom", "Export to GEDCOM file") %>% 
                    shinyjs::disabled()
    )
    
  ),
  
  shiny::fluidRow(id = "tabs",
                  shiny::column(width = 12,
                                shiny::tabsetPanel(id = "tabset",
                                                   shiny::tabPanel("GEDCOM File", file_ui("file")),
                                                   shiny::tabPanel("Individuals", individual_ui("indi")),
                                                   shiny::tabPanel("Families", family_ui("famg")),
                                                   shiny::tabPanel("Notes", note_ui("note")),
                                                   shiny::tabPanel("Multimedia", multimedia_ui("media")),
                                                   shiny::tabPanel("Sources", source_ui("sour")),
                                                   shiny::tabPanel("Repositories", repository_ui("repo")),
                                                   shiny::tabPanel("Tools", tools_ui("tools")),
                                                   shiny::tabPanel("GEDCOM", ged_debug_ui("debug")))
                  ) 
  ) %>% shinyjs::hidden()
  
)
