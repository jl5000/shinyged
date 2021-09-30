



shiny::shinyUI(shiny::fluidPage(
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
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
                                                   shiny::tabPanel("GEDCOM File", value = "file_tab", file_ui("file")),
                                                   shiny::tabPanel("Submitter", value = "subm_tab", submitter_ui("subm")),
                                                   shiny::tabPanel("Individuals", value = "indi_tab", individual_ui("indi")),
                                                   shiny::tabPanel("Families", value = "famg_tab", family_ui("famg")),
                                                   shiny::tabPanel("Notes", value = "note_tab", note_ui("note")),
                                                   shiny::tabPanel("Multimedia", value = "media_tab", multimedia_ui("media")),
                                                   shiny::tabPanel("Sources", value = "sour_tab", source_ui("sour")),
                                                   shiny::tabPanel("Repositories", value = "repo_tab", repository_ui("repo")),
                                                   shiny::tabPanel("GEDCOM", value = "debug_tab", ged_debug_ui("debug")))
                  ) 
  ) %>% shinyjs::hidden()
  
)
)
