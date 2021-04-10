


shiny::shinyUI(shiny::fluidPage(
    
    # Application title
    shiny::titlePanel("shinyged"),
    
    
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::selectInput("input_tab", "Choose tab", 
                               choices = c("Overview","Individuals","Family Groups","Sources",
                                           "Multimedia", "Repositories","Notes"),
                               selected = "Overview"),
            
            shiny::tags$hr(),
            
            shiny::fileInput("read_file", "Import GEDCOM file",
                             multiple = FALSE,
                             accept = ".ged"),
            shiny::tags$hr(),
            
            shiny::actionButton("create_gedcom", icon = shiny::icon("plus-square"), "Create GEDCOM file"),
            shiny::tags$hr(),
            shiny::downloadButton("export_gedcom", "Export to GEDCOM file")
        ),
        
        shiny::mainPanel(
            shinyBS::bsCollapse(open = "GEDCOM file details",
                                shinyBS::bsCollapsePanel("GEDCOM file details",
                                                         shiny::textInput("receiving_sys", "Receiving system", "tidyged"),
                                                         shiny::selectInput("language", "Language", 
                                                                            choices = tidyged.internals::languages(), selected = "English"),
                                                         shiny::textAreaInput("ged_desc", "Description", resize = "vertical") %>%
                                                             shiny::tagAppendAttributes(style = 'width: 100%;'),
                                                         shiny::textAreaInput("ged_copy", "Copyright statement", resize = "vertical") %>%
                                                             shiny::tagAppendAttributes(style = 'width: 100%;')
                                ),
                                shinyBS::bsCollapsePanel("Submitter details",
                                                         shiny::textInput("subm_name", "Name"),
                                                         # I want to modularise this into a generic address structure to be used elsewhere
                                                         shiny::splitLayout(
                                                             shiny::textInput("adr1", "Address line 1"),
                                                             shiny::textInput("state", "State")
                                                         ),
                                                         shiny::splitLayout(
                                                             shiny::textInput("adr2", "Address line 2"),
                                                             shiny::textInput("city", "City")
                                                         ),
                                                         shiny::splitLayout(
                                                             shiny::textInput("adr3", "Address line 3"),
                                                             shiny::textInput("postcode", "Postal code")
                                                         ),
                                                         shiny::textInput("country", "Country")
                                                         # add ability to define up to 3 emails, phones, urls, and faxes
                                                         #subm notes, media links
                                ),
                                shinyBS::bsCollapsePanel("Source data details",
                                                         shiny::splitLayout(
                                                             shiny::textInput("ged_source_name", "Source name"),
                                                             shiny::textInput("ged_source_date", "Publication date (e.g. 6 APR 1983)")
                                                         ),
                                                         shiny::textAreaInput("ged_source_copy", "Copyright", resize = "vertical") %>%
                                                             shiny::tagAppendAttributes(style = 'width: 100%;')
                                )
            )
        )
    )
))
