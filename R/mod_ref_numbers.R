






ref_numbers_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::actionButton(ns("ref_numbers"), label = NULL)
}



ref_numbers_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Click the button to show popup
    shiny::observeEvent(input$ref_numbers, {
      
      shiny::modalDialog(title = "Edit reference numbers",
                         
                         shiny::helpText("Each record can have any number of user-defined reference numbers associated with it."),
                         shiny::hr(),
                         DT::DTOutput(ns("table")),
                         
                         shiny::textInput(ns("ref_num"), label = "Reference number"),
                         shiny::textInput(ns("ref_type"), label = "Reference type (optional)") %>% shinyjs::disabled(),
                         shiny::actionButton(ns("add_ref_num"), "Add reference number") %>% shinyjs::disabled(),
                         shiny::actionButton(ns("delete_ref_num"), "Delete reference number") %>% shinyjs::disabled(),
                         shiny::actionButton(ns("update_ref_num"), "Update reference number") %>% shinyjs::disabled(),
                         
      ) %>% shiny::showModal()
      
    })
    
    # Derive a dataframe of ref numbers
    ref_number_df <- shiny::reactive({
      req(r$ged, r[[section_rows]])

      rows <- tidyged.internals::identify_section(r$ged, 1, "REFN", 
                                                  xrefs = r$ged$record[r[[section_rows]][1]],
                                                  first_only = FALSE)
      
      if(length(rows) > 0) {

      ref_num_df <- r$ged %>%
        dplyr::slice(rows) %>% 
        dplyr::select(tag, value) %>% 
        dplyr::mutate(id1 = cumsum(tag == "REFN")) %>% 
        as.data.frame() %>% 
        reshape(direction = "wide", idvar = "id1", v.names = "value", timevar = "tag") %>% 
        dplyr::select(-id1)

      if(ncol(ref_num_df) == 1) ref_num_df <- dplyr::mutate(ref_num_df, b = "")
      names(ref_num_df) <- letters[1:2]
      ref_num_df[is.na(ref_num_df)] <- ""
      
      } else {
        
        ref_num_df <- data.frame()
        
      }

      ref_num_df
    })
    
    shiny::observeEvent(ref_number_df(), {
      req(ref_number_df)
      
      lbl <- paste0(nrow(ref_number_df()), " reference numbers")
      if(nrow(ref_number_df()) == 1) lbl <- substr(lbl, 1, nchar(lbl) - 1)
      shiny::updateActionButton(inputId = "ref_numbers", label = lbl)
    })
    
    # Show the dataframe of ref numbers
    output$table <- DT::renderDataTable({
      req(ref_number_df)
      
      DT::datatable(ref_number_df(), rownames = FALSE, selection = "single",
                                      filter = "none", colnames = c("Reference number", "Type"),
                                      options = list(searching = FALSE, paging = FALSE))
    })
    
    # Validate ref num/type and enable/disable buttons
    shiny::observe({
      ref_num <- process_input(input$ref_num)
      ref_type <- process_input(input$ref_type)
      err1 <- tidyged.internals::chk_user_reference_number(ref_num, 1)
      err2 <- tidyged.internals::chk_user_reference_type(ref_type, 1)
      shinyFeedback::feedbackDanger("ref_num", !is.null(err1), err1)
      shinyFeedback::feedbackDanger("ref_type", !is.null(err2), err2)
      shinyjs::toggleState("ref_type", input$ref_num != "" && is.null(err1))
      shinyjs::toggleState("add_ref_num", input$ref_num != "" && is.null(err1) && is.null(err2))#nrow(merge(data.frame(a=ref_num,b=ref_type),ref_number_df()))==0
      shinyjs::toggleState("update_ref_num", input$ref_num != "" && is.null(err1) && is.null(err2))
    })
    
    
    shiny::observeEvent(input$table_rows_selected, ignoreNULL = FALSE, {
      if(length(input$table_rows_selected) > 0) {
        shiny::updateTextInput(inputId = "ref_num", value = ref_number_df()[input$table_rows_selected,1])
        shiny::updateTextInput(inputId = "ref_type", value = ref_number_df()[input$table_rows_selected,2])
      } else {
        shiny::updateTextInput(inputId = "ref_num", value = "")
        shiny::updateTextInput(inputId = "ref_type", value = "")
      }
      shinyjs::toggleState("delete_ref_num", !is.null(input$table_rows_selected))
      shinyjs::toggleState("update_ref_num", !is.null(input$table_rows_selected))
    })
    
    selected_ged_rows <- shiny::reactive({
      ref_num <- ref_number_df()[input$table_rows_selected,1]
      ref_type <- ref_number_df()[input$table_rows_selected,2]
      
      rows <- tidyged.internals::identify_section(r$ged, 1, "REFN", ref_num,
                                                  xrefs = r$ged$record[r[[section_rows]][1]])
      tags <- r$ged$tag[rows]
      
      if(sum(tags == "REFN") == 1) {
        rows
      } else {
        
      }
    })
    
    # Add ref number to tidyged object
    shiny::observeEvent(input$add_ref_num, {

      r$ged <- r$ged %>%
        tibble::add_row(tibble::tibble(record = r$ged$record[r[[section_rows]][1]], 
                                       level = 1, tag = "REFN", value = input$ref_num),
                        .after = max(r[[section_rows]]))
      
      if(input$ref_type != ""){
        
        r$ged <- r$ged %>%
          tibble::add_row(tibble::tibble(record = r$ged$record[r[[section_rows]][1]], 
                                         level = 2, tag = "TYPE", value = input$ref_type),
                          .after = max(r[[section_rows]]) + 1)
        
      }
      
      shiny::updateTextInput(inputId = "ref_num", value = "")
      shiny::updateTextInput(inputId = "ref_type", value = "")
    })
    
    # Update ref number in tidyged object
    shiny::observeEvent(input$update_ref_num, {
      r$ged$value[selected_ged_rows()[1]] <- input$ref_num
      
      type_exists <- length(selected_ged_rows()) == 2
      type_given <- input$ref_type != ""
      
      if(type_exists & type_given) {
        
        r$ged$value[selected_ged_rows()[2]] <- input$ref_type
        
      } else if(!type_exists & type_given) {
        
        r$ged <- r$ged %>%
          tibble::add_row(tibble::tibble(record = r$ged$record[selected_ged_rows()[1]], 
                                         level = 2, tag = "TYPE", value = input$ref_type),
                          .after = selected_ged_rows()[1])
        
      } else if(type_exists & !type_given) {
        
        r$ged <- dplyr::slice(r$ged, -selected_ged_rows()[2])
      }
      
    })
    
    # Remove ref number from tidyged object
    shiny::observeEvent(input$delete_ref_num, {
      r$ged <- dplyr::slice(r$ged, -selected_ged_rows())
      
      shiny::updateTextInput(inputId = "ref_num", value = "")
      shiny::updateTextInput(inputId = "ref_type", value = "")
    })
    
  })
}



