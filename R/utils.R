

#' Update value(s) in a tidyged object
#'
#' @param r A reactiveValues object containing the tidyged object (r$ged).
#' @param section A reactiveValues object containing the rows of the section to update (section$rows).
#' @param tag_order A character vector of tags indicating the desired order of tags in the section.
#' @param lvl The level of the value(s) to update.
#' @param tg All tag(s) that can be associated with the value(s).
#' @param new_value A character vector of new values with the appropriate tag and level. 
#'
#' @return A dummy value of TRUE. The critical return values are the updated reactiveValues objects,
#' which are updated by reference.
update_ged_value <- function(r, section, tag_order, lvl, tags, new_value = character()) {
  
  for(tg in tags) if(!tg %in% tag_order) stop("Tag is not recognised")
  if(length(new_value) == 1 && new_value == "") new_value = character()
  
  addr <- r$ged[section$rows,]
  ged <- r$ged[-section$rows,]
  
  # Delete existing
  new_addr <- dplyr::filter(addr, !(level == lvl & tag %in% tags))

  # Only use tags which are needed
  if(length(tags) > length(new_value)) length(tags) <- length(new_value)
  
    # Add again
  new_addr <- new_addr %>% 
    tibble::add_row(tibble::tibble(record = addr$record[1], level = lvl, tag = tags, value = new_value)) %>% 
    # Order by tags
    dplyr::mutate(tag = factor(tag, levels = tag_order, ordered = TRUE)) %>% 
    dplyr::arrange(tag) %>% 
    dplyr::mutate(tag = as.character(tag))
  
  shiny::isolate(
    r$ged <- ged %>% 
      tibble::add_row(new_addr, .before = section$rows[1])
  )
  
  section$rows <- seq.int(section$rows[1], length.out = nrow(new_addr))
  TRUE
}

process_input <- function(input) {
  input <- unlist(strsplit(input, "\n"))
  input <- unique(input[input != ""])
  input
}
