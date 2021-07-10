

family_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    DiagrammeR::DiagrammeROutput(ns("tree"), width = "1500px")
  )
  
}


family_summary_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    output$tree <- DiagrammeR::renderDiagrammeR({
      req(r$ged, r$famg_rows)
      
      xref <- r$ged$record[r$famg_rows[1]]
      visged::family_group_chart(r$ged, xref)
    })
    
  })
}
