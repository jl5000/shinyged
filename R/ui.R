


shinyUI(fluidPage(

    # Application title
    titlePanel("shinyged"),


    sidebarLayout(
        sidebarPanel(
            
            fileInput("read_file", "Import GEDCOM file",
                      multiple = FALSE,
                      accept = ".ged"),
            tags$hr(),
            textInput("subm_name", "Your name"),
            textInput("ged_desc", "GEDCOM description"),
            textInput("ged_copy", "GEDCOM copyright"),
            textInput("ged_source_name", "Source data"),
            dateInput("ged_source_date", "Source data publication date"),
            textInput("ged_source_copy", "Source data copyright"),
            textInput("receiving_sys", "Receiving system", "tidyged"),
            textInput("language", "Language", "English"),
            actionButton("create_gedcom", "Create GEDCOM file"),
        ),

        mainPanel(
            DT::DTOutput("file_summary"),
            textOutput("file_str")
        )
    )
))
