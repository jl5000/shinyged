
form_address <- function(trigger_id,
                         namespace = NULL,
                         adr1 = "",
                         adr2 = "",
                         adr3 = "",
                         state = "",
                         city = "",
                         postal_code = "",
                         country = "",
                         phones = "",
                         emails = "",
                         faxes = "",
                         websites = "") {
  
  shinyBS::bsModal(
    shiny::NS(namespace, "address"),
    "Enter contact details...",
    shiny::NS(namespace, trigger_id),
    
    shiny::fluidRow(
      shiny::column(6,
                    shiny::textInput(shiny::NS(namespace, "adr1"), "Address line 1", adr1),
                    shiny::textInput(shiny::NS(namespace, "adr2"), "Address line 2", adr2),
                    shiny::textInput(shiny::NS(namespace, "adr3"), "Address line 3", adr3)
      ),
      shiny::column(6,
                    shiny::textInput(shiny::NS(namespace, "city"), "City", city),
                    shiny::textInput(shiny::NS(namespace, "state"), "State", state),
                    shiny::textInput(shiny::NS(namespace, "postcode"), "Postal code", postal_code)
      ),
      
    ),
    shiny::textInput(shiny::NS(namespace, "country"), "Country", country),
    
    shiny::helpText("Up to 3 separate phone numbers, FAX numbers, email addresses, and websites can be defined. ",
                    "These must be defined on separate lines."),
    
    shiny::fluidRow(
      shiny::column(6,
                    shiny::textAreaInput(shiny::NS(namespace, "phones"), "Phone numbers", paste(phones, collapse = "\n"), height = "75px"),
                    shiny::textAreaInput(shiny::NS(namespace, "emails"), "Email addresses", paste(emails, collapse = "\n"), height = "75px")
      ),
      shiny::column(6,
                    shiny::textAreaInput(shiny::NS(namespace, "faxes"), "Fax numbers", paste(faxes, collapse = "\n"), height = "75px"),
                    shiny::textAreaInput(shiny::NS(namespace, "websites"), "Websites", paste(websites, collapse = "\n"), height = "75px")
      )
    )
    
  )
  
}