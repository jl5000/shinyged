

tab_media <- function() {
  
  shiny::tabPanel("Multimedia",
                  
                  shiny::tags$br(),
                  shiny::fluidRow(
                    shiny::column(6,
                                  shiny::selectInput("media_list", label = NULL, choices = NULL, width = "500px")
                    ),
                    shiny::column(6,
                                  shiny::actionButton("add_media", "Add multimedia"),
                                  shiny::actionButton("delete_media", "Delete multimedia")
                    )
                    
                  ),
                  
                  shiny::tabsetPanel(
              shiny::tabPanel("Summary")
              
            )
                  
                  
                  )
  
  
}