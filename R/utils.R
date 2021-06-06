

#' Update value(s) in a tidyged object
#'
#' @param r A reactiveValues object containing the tidyged object (r$ged) and vectors of rows of specific 
#' records and subrecords.
#' @param rows_name A string giving the name of the item in r where the rows to check can be found.
#' @param lvl The level of the value(s) to update.
#' @param tags All tag(s) that can be associated with the value(s).
#' @param new_value A character vector of new values with the appropriate tag and level. 
#' @param tag_order A character vector of tags indicating the desired order of tags in the section.
#'
#' @return Nothing. The critical return value is the updated r$ged object,
#' which is updated by reference.
update_ged_value <- function(r, rows_name, lvl, tags, new_value = character(), tag_order = NULL) {

  if(!is.null(tag_order)) for(tg in tags) if(!tg %in% tag_order) stop("Tag is not recognised")
  if(length(new_value) == 1 && new_value == "") new_value = character()
  
  section_rows <- r[[rows_name]]
  sec <- r$ged[section_rows,]
  ged <- r$ged[-section_rows,]
  
  # Delete existing
  new_sec <- dplyr::filter(sec, !(level == lvl & tag %in% tags))

  # Only use tags which are needed (e.g. ADR1 - ADR3)
  if(length(tags) > length(new_value)) length(tags) <- length(new_value)
  
    # Add again
  new_sec <- new_sec %>% 
    tibble::add_row(tibble::tibble(record = sec$record[1], level = lvl, tag = tags, value = new_value))
  
  # Order by tags
  if(!is.null(tag_order)) {
    new_sec <- dplyr::mutate(new_sec, tag = factor(tag, levels = tag_order, ordered = TRUE)) %>% 
      dplyr::arrange(tag) %>% 
      dplyr::mutate(tag = as.character(tag))
  }
    
  r$ged <- ged %>% 
    tibble::add_row(new_sec, .before = section_rows[1])
  
  invisible(TRUE)
}

process_input <- function(input) {
  input <- unlist(strsplit(input, "\n"))
  input <- unique(input[input != ""])
  input
}
