


tools_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::actionButton(ns("remove_change_dates"), "Delete all modification dates"),
    shiny::actionButton(ns("arrange_children"), "Order children by birth")
  )
  
}


tools_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$remove_change_dates, {
      r$ged <- tidyged.utils::remove_change_dates(r$ged)
      shiny::showNotification("All change dates have been deleted")
    })
    
    shiny::observeEvent(input$arrange_children, {
      r$ged <- tidyged.utils::order_famg_children_all(r$ged)
      shiny::showNotification("All children have been ordered by birth date")
    })
    
  })
}



