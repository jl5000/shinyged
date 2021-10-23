


source_repo_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::actionButton(ns("repos"), label = "Repositories")
  )
  
}


source_repo_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observe({
      
      repos <- tidyged::xrefs_repo(r$ged) %>% 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
      
      shiny::modalDialog(title = "Edit source repositories",
                         
                         shiny::tagList(
                           shiny::helpText("Each source may come from a number of repositories."),
                           shiny::hr(),
                           DT::DTOutput(ns("table")),
                           shiny::br(),
                           shiny::selectizeInput(ns("repo"), label = NULL, choices = repos,
                                                 multiple = TRUE, width = "500px", options = list(maxItems = 1)),
                           shiny::textInput(ns("call_number"), label = "Call number (optional)") %>% shinyjs::disabled(),
                           shiny::actionButton(ns("add_repo"), "Add repository") %>% shinyjs::disabled(),
                           shiny::actionButton(ns("delete_repo"), "Delete repository") %>% shinyjs::disabled(),
                           shiny::actionButton(ns("update_repo"), "Update repository") %>% shinyjs::disabled(),
                           
                         ) 
      ) %>% shiny::showModal()
    }) %>% 
      shiny::bindEvent(input$repos)
    
    # Derive a dataframe of repos
    repo_df <- shiny::reactive({
      req(r$ged)
      
      rows <- tidyged.internals::identify_section(r$ged, 1, "REPO", 
                                                  xrefs = r$ged$record[r$sour_rows[1]],
                                                  first_only = FALSE)
      
      if(length(rows) > 0) {

        repodf <- r$ged %>%
          dplyr::slice(rows) %>% 
          dplyr::select(tag, value) %>% 
          dplyr::mutate(id1 = cumsum(tag == "REPO")) %>% 
          as.data.frame() %>% 
          reshape(direction = "wide", idvar = "id1", v.names = "value", timevar = "tag") %>% 
          dplyr::select(-id1)
        
        if(ncol(repodf) == 1) repodf <- dplyr::mutate(repodf, b = "")
        names(repodf) <- letters[1:2]
        repodf[is.na(repodf)] <- ""
        xrefs <- repodf$a
        xrefs_desc <- sapply(xrefs, tidyged::describe_records, gedcom = r$ged, 
                             short_desc = TRUE, USE.NAMES = FALSE)
        repodf$a <- xrefs_desc
        
      } else {
        
        repodf <- data.frame()
        
      }
      
      repodf
    })
    
    shiny::observe({
      req(repo_df)
      
      lbl <- paste0(nrow(repo_df()), " repositories")
      if(nrow(repo_df()) == 1) lbl <- paste0(nrow(repo_df()), " repository")
      shiny::updateActionButton(inputId = "repos", label = lbl)
    }) %>% 
      shiny::bindEvent(repo_df())
    
    # Show the dataframe of repos
    output$table <- DT::renderDataTable({
      req(repo_df)
      
      DT::datatable(repo_df(), rownames = FALSE, selection = "single",
                    filter = "none", colnames = c("Repository", "Call number"),
                    options = list(searching = FALSE, paging = FALSE))
    })
    
    # Validate call num and enable/disable buttons
    shiny::observe({
      call_num <- process_input(input$call_number)
      err <- tidyged.internals::chk_source_call_number(call_num, 1)
      shinyFeedback::feedbackDanger("call_number", !is.null(err), err)
      shinyjs::toggleState("call_number", !is.null(input$repo))
      shinyjs::toggleState("add_repo", !is.null(input$repo) && is.null(err))
      shinyjs::toggleState("update_repo", !is.null(input$repo) && is.null(err))
    })
    
    
    shiny::observe({
      if(length(input$table_rows_selected) > 0) {
        shiny::updateSelectizeInput(inputId = "repo", selected = repo_df()[input$table_rows_selected,1])
        shiny::updateTextInput(inputId = "call_number", value = repo_df()[input$table_rows_selected,2])
      } else {
        shiny::updateSelectizeInput(inputId = "repo", selected = NULL)
        shiny::updateTextInput(inputId = "call_number", value = "")
      }
      shinyjs::toggleState("delete_repo", !is.null(input$table_rows_selected))
      shinyjs::toggleState("update_repo", !is.null(input$table_rows_selected))
    }) %>% 
      shiny::bindEvent(input$table_rows_selected, ignoreNULL = FALSE)
    
    selected_ged_rows <- shiny::reactive({
      sel_repo <- repo_df()[input$table_rows_selected,1]
      repo <- stringr::str_extract(sel_repo, tidyged.internals::reg_xref(FALSE))
      call_num <- repo_df()[input$table_rows_selected,2]
      
      rows <- tidyged.internals::identify_section(r$ged, 1, "REPO", repo,
                                                  xrefs = r$ged$record[r$sour_rows[1]])
      tags <- r$ged$tag[rows]
      
      if(sum(tags == "REPO") == 1) {
        rows
      } else {
        #TODO
      }
    })
    
    # Add repo to tidyged object
    shiny::observe({
      
      repo <- stringr::str_extract(input$repo, tidyged.internals::reg_xref(FALSE))
      
      r$ged <- r$ged %>%
        tibble::add_row(tibble::tibble(record = r$ged$record[r$sour_rows[1]], 
                                       level = 1, tag = "REPO", value = repo),
                        .after = max(r$sour_rows))
      
      if(input$call_number != ""){
        
        r$ged <- r$ged %>%
          tibble::add_row(tibble::tibble(record = r$ged$record[r$sour_rows[1]], 
                                         level = 2, tag = "CALN", value = input$call_number),
                          .after = max(r$sour_rows) + 1)
        
      }
      
      shiny::updateSelectizeInput(inputId = "repo", selected = NULL)
      shiny::updateTextInput(inputId = "call_number", value = "")
    }) %>% 
      shiny::bindEvent(input$add_repo)
    
    # Update repo in tidyged object
    shiny::observe({
      repo <- stringr::str_extract(input$repo, tidyged.internals::reg_xref(FALSE))
      r$ged$value[selected_ged_rows()[1]] <- repo
      
      caln_exists <- length(selected_ged_rows()) == 2
      caln_given <- input$call_number != ""
      
      if(caln_exists & caln_given) {
        
        r$ged$value[selected_ged_rows()[2]] <- input$call_number
        
      } else if(!caln_exists & caln_given) {
        
        r$ged <- r$ged %>%
          tibble::add_row(tibble::tibble(record = r$ged$record[selected_ged_rows()[1]], 
                                         level = 2, tag = "CALN", value = input$call_number),
                          .after = selected_ged_rows()[1])
        
      } else if(caln_exists & !caln_given) {
        
        r$ged <- dplyr::slice(r$ged, -selected_ged_rows()[2])
      }
      
    }) %>% 
      shiny::bindEvent(input$update_repo)
    
    # Remove repo from tidyged object
    shiny::observe({
      r$ged <- dplyr::slice(r$ged, -selected_ged_rows())
      
      shiny::updateSelectizeInput(inputId = "repo", selected = NULL)
      shiny::updateTextInput(inputId = "call_number", value = "")
    }) %>% 
      shiny::bindEvent(input$delete_repo)
    
    
  })
}


