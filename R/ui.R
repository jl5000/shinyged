


shinyUI(fluidPage(

    # Application title
    titlePanel("shinyged"),


    sidebarLayout(
        sidebarPanel(
            selectInput("input_tab", "Choose tab", 
                        choices = c("Overview","Individuals","Family Groups","Sources",
                                    "Multimedia", "Repositories","Notes"),
                        selected = "Overview"),
            
            tags$hr(),
            
            fileInput("read_file", "Import GEDCOM file",
                      multiple = FALSE,
                      accept = ".ged"),
            tags$hr(),
            
            actionButton("create_gedcom", icon = icon("plus-square"), "Create GEDCOM file"),
            tags$hr(),
            downloadButton("export_gedcom", "Export to GEDCOM file")
        ),

        mainPanel(
            textInput("subm_name", "Submitter name", width = "90%"),
            #subm address, notes, media links
            tags$hr(),
            textAreaInput("ged_desc", "GEDCOM description", width = "90%", resize = "vertical") %>%
                shiny::tagAppendAttributes(style = 'width: 100%;'),
            textAreaInput("ged_copy", "GEDCOM copyright", width = "90%", resize = "vertical") %>%
                shiny::tagAppendAttributes(style = 'width: 100%;'),
            textAreaInput("ged_source_name", "Source data", width = "90%", resize = "vertical") %>%
                shiny::tagAppendAttributes(style = 'width: 100%;'),
            dateInput("ged_source_date", "Source data publication date"),
            textAreaInput("ged_source_copy", "Source data copyright", width = "90%", resize = "vertical") %>%
                shiny::tagAppendAttributes(style = 'width: 100%;'),
            textInput("receiving_sys", "Receiving system", "tidyged", width = "90%"),
            selectInput("language", "Language", 
                        choices = c("Afrikaans", "Albanian", "Anglo-Saxon", "Catalan", "Catalan_Spn", "Czech", 
                                    "Danish", "Dutch", "English", "Esperanto", "Estonian", "Faroese", "Finnish", 
                                    "French", "German", "Hawaiian", "Hungarian", "Icelandic", "Indonesian", 
                                    "Italian", "Latvian", "Lithuanian", "Navaho", "Norwegian", "Polish", 
                                    "Portuguese", "Romanian", "Serbo_Croa", "Slovak", "Slovene", "Spanish", 
                                    "Swedish", "Turkish", "Wendic"), selected = "English")
            
            # DT::DTOutput("file_summary"),
            # textOutput("file_str")
        )
    )
))
