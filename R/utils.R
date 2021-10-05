

#' Update value(s) in a tidyged object
#'
#' @param r A reactiveValues object containing the tidyged object (r$ged) and vectors of rows of specific 
#' records and subrecords.
#' @param rows_name A string giving the name of the item in r where the rows of the relevant section are.
#' @param lvl The level of the value(s) to update.
#' @param tags All tag(s) that can be associated with the value(s).
#' @param new_value A character vector of new values with the appropriate tag and level. An empty character vector will remove existing values.
#' @param tag_order A character vector of tags indicating the desired order of tags in the section.
#'
#' @return Nothing. The critical side effect is the updated r$ged object,
#' which is updated by reference.
update_ged_value <- function(r, rows_name, lvl, tags, new_value = character(), tag_order = NULL) {

  if(!is.null(tag_order)) for(tg in tags) if(!tg %in% tag_order) stop("Tag is not recognised")
  if(length(new_value) == 1 && new_value == "") new_value = character() # No value - remove row
  new_value <- as.character(new_value)

  section_rows <- r[[rows_name]]
  if(length(new_value) == 0 && length(section_rows) == 0) return(invisible(TRUE))
  
  sec <- r$ged[section_rows,]
  ged <- r$ged[-section_rows,]
  
  # Merge together HUSB/WIFE AGE
  husb_row <- which(rec$level == 2 & rec$tag == "HUSB")
  if(length(husb_row) > 0){
    rec$tag[husb_row] <- "HUSB_AGE"
    rec$value[husb_row] <- rec$value[husb_row + 1]
    rec <- dplyr::slice(rec, -(husb_row + 1))
  }
  wife_row <- which(rec$level == 2 & rec$tag == "WIFE")
  if(length(wife_row) > 0){
    rec$tag[wife_row] <- "WIFE_AGE"
    rec$value[wife_row] <- rec$value[wife_row + 1]
    rec <- dplyr::slice(rec, -(wife_row + 1))
  }
  
  # Delete existing 
  new_sec <- dplyr::filter(sec, !(level == lvl & tag %in% tags))

  # Only use tags which are needed (e.g. ADR1 - ADR3) for the new value
  if(length(tags) > length(new_value)) length(tags) <- length(new_value)
  
    # Add again
  new_sec <- new_sec %>% 
    tibble::add_row(tibble::tibble(record = sec$record[1], level = lvl, tag = tags, value = new_value))
  
  # Now need to manage container rows with no value
  
  
  # If source citation, potentially add a DATA row - PLAC-MAP, ADDR, HUSB/WIFE-AGE, SOUR-DATA, - GENERALISE
  if(nrow(new_sec) > 0 && new_sec$tag[1] == "SOUR" && new_sec$level[1] > 0) {
    new_sec <- manage_source_citation_data_row(new_sec, "SOUR", sec$level[1], c("DATE","TEXT"))
  }
    
  
  # Order by tags
  if(!is.null(tag_order)) {
    new_sec <- dplyr::mutate(new_sec, tag = factor(tag, levels = tag_order, ordered = TRUE)) %>% 
      dplyr::arrange(tag) %>% 
      dplyr::mutate(tag = as.character(tag))
  }
  
  # TODO: Special case - split HUSB_AGE / WIFE_AGE to separate rows
    
  r$ged <- ged %>% 
    tibble::add_row(new_sec, .before = section_rows[1])
  
  invisible(TRUE)
}

manage_source_citation_data_row <- function(section, container_tag, container_tag_level, child_tags){
  
  container_tag_exists <- length(which(section$tag == container_tag)) > 0
  child_tags_exist <- nrow(dplyr::filter(section, tag %in% child_tags)) > 0
  
  if(child_tags_exist) {
    if(!container_tag_exists){ 
      # Add it to end, because it will be reordered eventually
      section <- citation_tbl %>% 
        tibble::add_row(tibble::tibble(record = citation_tbl$record[1], 
                                       level = citation_tbl$level[1] + 1, 
                                       tag = "DATA", value = ""))
      
    }
  } else if(container_tag_exists) {
    section <- dplyr::filter(section, tag != container_tag)
  }
  
  section
}

process_input <- function(input, input_required = FALSE) {
  if(is.null(input)) return(character())
  input <- as.character(input) 
  input <- unlist(stringr::str_split(input, "\n")) #base::strsplit doesn't handle ""
  if(!input_required) input <- input[input != ""]
  
  unique(input)
}


