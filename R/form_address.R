
form_address <- function(button_id,
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
    "address",
    "Enter address details...",
    button_id,
    
    shiny::fluidRow(
      shiny::column(6,
                    shiny::textInput("adr1", "Address line 1", adr1),
                    shiny::textInput("adr2", "Address line 2", adr2),
                    shiny::textInput("adr3", "Address line 3", adr3)
      ),
      shiny::column(6,
                    shiny::textInput("city", "City", city),
                    shiny::textInput("state", "State", state),
                    shiny::textInput("postcode", "Postal code", postal_code)
      ),
      
    ),
    shiny::textInput("country", "Country", country),
    
    shiny::helpText("Up to 3 separate phone numbers, FAX numbers, email addresses, and websites can be defined. ",
                    "These must be defined on separate lines."),
    
    shiny::fluidRow(
      shiny::column(6,
                    shiny::textAreaInput("phones", "Phone numbers", paste(phones, collapse = "\n"), height = "75px"),
                    shiny::textAreaInput("emails", "Email addresses", paste(emails, collapse = "\n"), height = "75px")
      ),
      shiny::column(6,
                    shiny::textAreaInput("faxes", "Fax numbers", paste(faxes, collapse = "\n"), height = "75px"),
                    shiny::textAreaInput("websites", "Websites", paste(websites, collapse = "\n"), height = "75px")
      )
    )
    
  )
  
}