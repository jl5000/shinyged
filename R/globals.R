

.pkgenv <- new.env(parent=emptyenv())


.pkgenv$tags_addr <- c("ADDR",paste0("ADR",1:3),"CITY","STAE","POST",
                            "CTRY","PHON","FAX","EMAIL","WWW")
.pkgenv$tags_file_sour <- c("SOUR","VERS","NAME","CORP",.pkgenv$tags_addr, "DATA","DATE","COPR")
.pkgenv$tags_sour_cit <- c("SOUR","PAGE","EVEN","ROLE","DATA","DATE","TEXT","OBJE","NOTE","QUAY")


unique_facts <- function() {
  # Make values of the lookups unique for selectInput
  add_space <- function(x) {paste0(x, " ")}
  
  att <- tidyged.internals::val_attribute_types()
  att["Residence"] <- add_space(att["Residence"])
  
  indev <- tidyged.internals::val_individual_event_types()
  indev["Census"] <- add_space(indev["Census"])
  indev["Other event"] <- add_space(indev["Other event"])
  
  famev <- tidyged.internals::val_family_event_types()
  
  list("Individual attributes" = att,
       "Individual events" = indev,
       "Family events" = famev)
  
  
}