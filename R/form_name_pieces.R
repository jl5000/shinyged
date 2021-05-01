

form_name_pieces <- function(button_id,
                             prefix = "",
                             given = "",
                             nick = "",
                             surn_prefix = "",
                             surname = "",
                             suffix = "") {
  
  shinyBS::bsModal(
    "name",
    "Enter name pieces...",
    button_id,
    
    shiny::fluidRow(
      shiny::column(6,
             shiny::textInput("prefix", "Name prefix", prefix),
             shiny::textInput("given", "Given name(s)", given),
             shiny::textInput("nick", "Nickname", nick)
      ),
      shiny::column(6,
             shiny::textInput("surn_prefix", "Surname prefix", surn_prefix),
             shiny::textInput("surname", "Surname", surname),
             shiny::textInput("suffix", "Suffix (e.g. Jr.)", suffix)
      )
      
    )
 
  )
  
}