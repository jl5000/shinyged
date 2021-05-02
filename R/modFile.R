
tab_overview <- function() {
  
  shiny::tabPanel("GEDCOM file",
                  shiny::tags$br(),
                  
                  shiny::tabsetPanel(
                    shiny::tabPanel("Summary",
                                    shiny::verbatimTextOutput("file_summary")
                    ),
                    shiny::tabPanel("File details",
                                    shiny::tags$br(),
                                    shiny::splitLayout(
                                      shiny::textInput("receiving_sys", "Receiving system", "tidyged"),
                                      shiny::selectInput("language", "Language", 
                                                         choices = tidyged.internals::val_languages(), selected = "English")
                                    ),
                                    shiny::textAreaInput("ged_desc", "Description", resize = "vertical") %>%
                                      shiny::tagAppendAttributes(style = 'width: 100%;'),
                                    shiny::textAreaInput("ged_copy", "Copyright statement", resize = "vertical") %>%
                                      shiny::tagAppendAttributes(style = 'width: 100%;')
                    ),
                    shiny::tabPanel("Submitter details",
                                    shiny::tags$br(),
                                    shiny::textInput("subm_name", "Name"),
                                    shiny::fluidRow(
                                      shiny::column(6,
                                                    shiny::actionButton("edit_subm_address", "Edit address"),
                                                    shiny::actionButton("rm_subm_address", "Remove address")
                                      ),
                                      shiny::column(6,
                                                    form_address("edit_subm_address"),
                                                    shiny::textOutput("subm_addr_out")
                                      )
                                      
                                      
                                      
                                    )
                                    
                                    #subm notes, media links
                    ),
                    shiny::tabPanel("Source data details",
                                    shiny::tags$br(),
                                    shiny::splitLayout(
                                      shiny::textInput("ged_source_name", "Source name"),
                                      shiny::textInput("ged_source_date", "Publication date (e.g. 6 APR 1983)")
                                    ),
                                    shiny::textAreaInput("ged_source_copy", "Copyright", resize = "vertical") %>%
                                      shiny::tagAppendAttributes(style = 'width: 100%;')
                    )
                  )                    
                  
  )
  
  
}