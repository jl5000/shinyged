

#' Update value(s) in a tidyged object
#'
#' @param r A reactiveValues object containing the tidyged object (r$ged) and vectors of rows of specific 
#' records and subrecords.
#' @param rows_name A string giving the name of the item in r where the rows of the relevant section are.
#' @param xref The xref of the record to be updated.
#' @param lvl The level of the value(s) to update.
#' @param tags All tag(s) that can be associated with the value(s).
#' @param new_value A character vector of new values with the appropriate tag and level. 
#' An empty character vector will remove existing values.
#' @param tag_order A character vector of tags indicating the desired order of tags in the section.
#'
#' @return Nothing. The critical side effect is the updated r$ged object,
#' which is updated by reference.
update_ged_value <- function(r, rows_name, xref, lvl, tags, new_value = character(), tag_order = NULL) {

  if(!is.null(tag_order)) for(tg in tags) if(!tg %in% tag_order) stop("Tag is not recognised")
  if(length(new_value) == 1 && new_value == "") new_value = character() # No value - remove row
  new_value <- as.character(new_value)

  section_rows <- r[[rows_name]]
  if(length(new_value) == 0 && length(section_rows) == 0) return(invisible(TRUE))
  
  sec <- r$ged[section_rows,]
  if(length(section_rows) > 0) ged <- r$ged[-section_rows,] else ged <- r$ged
  
  # Delete existing 
  new_sec <- dplyr::filter(sec, !(level == lvl & tag %in% tags))

  # Only use tags which are needed (e.g. ADR1 - ADR3) for the new value
  if(length(tags) > length(new_value)) length(tags) <- length(new_value)
  
    # Add again
  new_sec <- new_sec |> 
    tibble::add_row(tibble::tibble(record = xref, level = lvl, tag = tags, value = new_value)) |> 
    deal_with_crap()
  
  # Order by tags
  if(!is.null(tag_order)) {
    new_sec <- dplyr::mutate(new_sec, tag = factor(tag, levels = tag_order, ordered = TRUE)) |> 
      dplyr::arrange(tag) |> 
      dplyr::mutate(tag = as.character(tag))
  }
  
  r$ged <- ged |> 
    tibble::add_row(new_sec, .before = section_rows[1])
  
  invisible(TRUE)
}

deal_with_crap <- function(section){
  
  if(nrow(section) == 0) return(section)
  
  top_level <- section$level[1]
  top_tag <- section$tag[1]
  
  if(top_tag == "SOUR" && top_level > 0)
    section <- crap_function(section, c("DATA","DATE","TEXT")) 
  
  if(top_tag == "PLAC" |
     top_tag %in% c(tidyged.internals::val_attribute_types(),
                    tidyged.internals::val_individual_event_types(),
                    tidyged.internals::val_family_event_types()))
    section <- crap_function(section, c("MAP","LATI","LONG")) 
  
  if(top_tag == "REPO" | top_tag == "SUBM" |
     top_tag %in% c(tidyged.internals::val_attribute_types(),
                    tidyged.internals::val_individual_event_types(),
                    tidyged.internals::val_family_event_types()))
    section <- crap_function(section, .pkgenv$tags_addr) 
    
  section

}

crap_function <- function(section, tag_order){

  containing_tag <- tag_order[1]
  
  #extract rows
  subsection <- dplyr::filter(section, tag %in% tag_order)
  
  #if none, exit
  if(nrow(subsection) == 0) return(section)
  
  #if one and its header, delete and exit
  if(nrow(subsection) == 1 && subsection$tag[1] == containing_tag)
    return(dplyr::filter(section, tag != containing_tag))
  
  #if one or more and no header, insert header
  if(nrow(subsection) > 0 && !containing_tag %in% subsection$tag)
    subsection <- subsection |> 
      tibble::add_row(tibble::tibble(record = section$record[1],
                                     level = section$level[1] + 1,
                                     tag = containing_tag, value = ""),
                      .before = 1)
  
  #delete rows from main
  section <- dplyr::filter(section, !tag %in% tag_order)
  
  #order rows
  subsection <- dplyr::mutate(subsection, tag = factor(tag, levels = tag_order, ordered = TRUE)) |> 
    dplyr::arrange(tag) |> 
    dplyr::mutate(tag = as.character(tag))
  
  #add to main
  dplyr::bind_rows(section, subsection)
  
}


process_input <- function(input, input_required = FALSE) {
  if(is.null(input)) return(character())
  input <- as.character(input) 
  input <- unlist(stringr::str_split(input, "\n")) #base::strsplit doesn't handle ""
  if(!input_required) input <- input[input != ""]
  
  unique(input)
}


