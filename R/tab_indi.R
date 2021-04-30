

tab_indi <- function() {
  
  shiny::tabPanel("Individuals",
                  
                  shiny::actionButton("edit_name", "Edit name"),
                  form_name_pieces("edit_name")
                  
                  
                  )
  
  
}