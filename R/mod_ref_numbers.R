






ref_numbers_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(12,
                    shiny::helpText("Each record can have any number of user-defined reference numbers associated with it.",
                                    style = 'margin-top:0px'),
                    shiny::actionButton(ns("ref_numbers"), "Edit reference numbers", style = 'margin-top:-5px')

                    
                    )
    )
  )
}

ref_numbers_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observeEvent(input$ref_numbers, {
      req(r$ged)
      shiny::showModal(
        shiny::modalDialog(title = "Edit reference numbers",
          DT::renderDataTable(DT::datatable(data.frame(a = 1), rownames = FALSE, selection = "single",
                                            filter = "none", colnames = c("Reference number", "Type"),
                                            options = list(searching = FALSE, paging = FALSE))),
          
          shiny::textInput(ns("ref_num"), label = "Reference number"),
          shiny::textInput(ns("ref_type"), label = "Reference type (optional)"),
          shiny::actionButton(ns("add_ref_num"), "Add reference number"),
          shiny::actionButton(ns("delete_ref_num"), "Delete reference number"),
          
          footer = shiny::tagList(
            shiny::modalButton("Close")
          )
        )
      )
    })
    
  })
}



