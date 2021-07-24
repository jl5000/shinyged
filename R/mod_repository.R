
repository_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("record"), label = NULL, choices = NULL,
                                          multiple = TRUE, width = "500px", options = list(maxItems = 1))
      ),
      shiny::column(6,
                    shiny::actionButton(ns("add"), "Add repository"),
                    shiny::actionButton(ns("delete"), "Delete repository")
      )
      
    ),
    
    shinyjs::hidden(
      shiny::fluidRow(id = ns("repo_tabs"),
                      shiny::column(12,
                                    shiny::tabsetPanel(id = ns("tabset"),
                                      shiny::tabPanel("Summary", repository_summary_ui(ns("repo_summary"))),
                                      shiny::tabPanel("Details", repository_details_ui(ns("repo_details"))),
                                      shiny::tabPanel("Notes", notes_ui(ns("repo_notes")))
                                    )
                      )
      )
    )
  )
}

repository_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update list of repositories
    records <- shiny::reactive({
      req(r$ged)

      tidyged::xrefs_repo(r$ged) %>% 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
    })
    
    # Update choices with list of repositories and select one
    observeEvent(records(), {
      if(!is.null(records())) {
        
        if(is.null(r$repo_to_select)) {
          current_selection <- input$record
        } else {
          current_selection <- r$repo_to_select
        }
        
        shiny::updateSelectizeInput(session = session, inputId = "record", choices = records(), selected = current_selection)
      } else {
        shiny::updateSelectizeInput(session = session, inputId = "record", choices = character(), selected = character())
      }
      r$repo_to_select <- NULL
    })
    
    # Update repo_rows
    shiny::observeEvent(priority = 2, {
      input$record
      r$ged
    }, {
      req(input$record)
      repo_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$repo_rows <- which(r$ged$record == repo_xref)
    })
    
    # Show/hide tabs and toggle delete button
    shiny::observeEvent(input$record, ignoreNULL = FALSE, {
      shinyjs::toggle("repo_tabs", condition = !is.null(input$record))
      shinyjs::toggleState("delete", !is.null(input$record))
    })
    
    # Popup to to give repository name
    shiny::observeEvent(input$add, {
      req(r$ged)
      shiny::showModal(
        shiny::modalDialog(
          shiny::helpText("The repository must be given a name."),
          shiny::textInput(ns("repo_name"), label = "Repository name"),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shinyjs::disabled(shiny::actionButton(ns("add_repo"), "Add repository"))
          )
        )
      )
    })
    
    # Disable add_repo button if no valid name
    shiny::observeEvent(input$repo_name, ignoreNULL = FALSE, ignoreInit = TRUE, {
      repo_name <- process_input(input$repo_name)
      err <- tidyged.internals::chk_name_of_repository(repo_name, 1)
      shinyFeedback::feedbackDanger("repo_name", !is.null(err), err)
      shinyjs::toggleState("add_repo", is.null(err) & input$repo_name != "")
    })
    
    # Add repository
    observeEvent(input$add_repo, {
      r$ged <- tidyged::add_repo(r$ged, input$repo_name)
      repo_xrefs <- tidyged::xrefs_repo(r$ged)
      last_repo <- tail(repo_xrefs, 1)
      r$repo_to_select <- tidyged::describe_records(r$ged, last_repo, short_desc = TRUE)
      shiny::removeModal()
    })
    
    # Remove repository and set a flag to ensure no repository is selected
    observeEvent(input$delete, {
      repo_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$ged <- tidyged::remove_repo(r$ged, repo_xref)
      shiny::showNotification("Repository deleted")
      r$repo_to_select <- NULL
    })
    
    repository_summary_server("repo_summary", r)
    
    shiny::observeEvent({input$tabset == "Details"},once=TRUE,ignoreInit = TRUE, {
      repository_details_server("repo_details", r)
    })
    shiny::observeEvent({input$tabset == "Notes"},once=TRUE,ignoreInit = TRUE, {
      notes_server("repo_notes", r, "repo_rows")
    })
    
    
    
  })
}

