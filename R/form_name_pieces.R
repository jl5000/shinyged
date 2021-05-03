

form_name_pieces <- function(trigger_id,
                             namespace,
                             prefix = "",
                             given = "",
                             nick = "",
                             surn_prefix = "",
                             surname = "",
                             suffix = "") {
  
  shinyBS::bsModal(
    shiny::NS(namespace, "name"),
    "Enter name pieces...",
    shiny::NS(namespace, trigger_id),
    
    shiny::fluidRow(
      shiny::column(6,
             shiny::textInput(shiny::NS(namespace, "prefix"), "Name prefix", prefix),
             shiny::textInput(shiny::NS(namespace, "given"), "Given name(s)", given),
             shiny::textInput(shiny::NS(namespace, "nick"), "Nickname", nick)
      ),
      shiny::column(6,
             shiny::textInput(shiny::NS(namespace, "surn_prefix"), "Surname prefix", surn_prefix),
             shiny::textInput(shiny::NS(namespace, "surname"), "Surname", surname),
             shiny::textInput(shiny::NS(namespace, "suffix"), "Suffix (e.g. Jr.)", suffix)
      )
      
    )
 
  )
  
}