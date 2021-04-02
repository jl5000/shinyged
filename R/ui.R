


library(shiny)


shinyUI(fluidPage(

    # Application title
    titlePanel("shinyged"),


    sidebarLayout(
        sidebarPanel(
            actionButton("read_file", "Import GEDCOM file"),
            
            actionButton("create_gedcom", "Create GEDCOM file")
        ),

        mainPanel(
            textOutput("file_summary"),
            textOutput("file_str")
        )
    )
))
