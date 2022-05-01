
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
    
    shiny::fluidRow(id = ns("repo_data"),
      shiny::column(4,
                    shiny::textInput(ns("repo_name"), "Name"),
      ),
      shiny::column(8, style = 'margin-top:25px',
                    ref_numbers_ui(ns("repo_ref_numbers")),
                    notes_ui(ns("repo_notes")),
      )
    ) |> shinyjs::hidden(),
    
    address_ui(ns("repo_address")) |> shinyjs::hidden(),
    
    
  )
}

repository_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ref_numbers_server("repo_ref_numbers", r, "repo_rows")
    notes_server("repo_notes", r, "repo_rows")
    address_server("repo_address", r, "repo_rows")
    
    
    # Update list of repositories
    records <- shiny::reactive({
      req(r$ged)

      tidyged::xrefs_repo(r$ged) |> 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
    })
    
    # Update choices with list of repositories and select one
    shiny::observe({
      if(!is.null(records())) {
        
        if(is.null(r$repo_to_select)) {
          current_selection <- input$record
        } else {
          current_selection <- r$repo_to_select
        }
        
        shiny::updateSelectizeInput(inputId = "record", choices = records(), selected = current_selection)
      } else {
        shiny::updateSelectizeInput(inputId = "record", choices = character(), selected = character())
      }
      r$repo_to_select <- NULL
    }) |> 
      shiny::bindEvent(records())
    
    # Update repo_rows
    shiny::observe(priority = 2, {
      req(input$record)
      repo_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$repo_rows <- which(r$ged$record == repo_xref)
    }) |> 
      shiny::bindEvent(input$record, r$ged)
    
    # Show/hide tabs and toggle delete button
    shiny::observe({
      shinyjs::toggle("repo_data", condition = !is.null(input$record))
      shinyjs::toggle("repo_address", condition = !is.null(input$record))
      shinyjs::toggleState("delete", !is.null(input$record))
    }) |> 
      shiny::bindEvent(input$record, ignoreNULL = FALSE)
    
    # Popup to to give repository name
    shiny::observe({
      req(r$ged)
      
      shiny::modalDialog(
        shiny::helpText("The repository must be given a name."),
        shiny::textInput(ns("repo_name"), label = "Repository name"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shinyjs::disabled(shiny::actionButton(ns("add_repo"), "Add repository"))
        )
      ) |> shiny::showModal()
      
    }) |> 
      shiny::bindEvent(input$add)
    
    # Disable add_repo button if no valid name
    shiny::observe({
      #don't need input_required=TRUE because add_repo button is disabled
      repo_name <- process_input(input$repo_name) 
      err <- tidyged.internals::chk_name_of_repository(repo_name, 1)
      shinyFeedback::feedbackDanger("repo_name", !is.null(err), err)
      shinyjs::toggleState("add_repo", is.null(err) & input$repo_name != "")
    }) |> 
      shiny::bindEvent(input$repo_name, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Add repository
    shiny::observe({
      r$ged <- tidyged::add_repo(r$ged, input$repo_name)
      repo_xrefs <- tidyged::xrefs_repo(r$ged)
      last_repo <- tail(repo_xrefs, 1)
      r$repo_to_select <- tidyged::describe_records(r$ged, last_repo, short_desc = TRUE)
      shiny::removeModal()
    }) |> 
      shiny::bindEvent(input$add_repo)
    
    # Remove repository and set a flag to ensure no repository is selected
    shiny::observe({
      repo_xref <- stringr::str_extract(input$record, tidyged.internals::reg_xref(FALSE))
      r$ged <- tidyged::remove_repo(r$ged, repo_xref)
      shiny::showNotification("Repository deleted")
      r$repo_to_select <- NULL
    }) |> 
      shiny::bindEvent(input$delete)
    
 

    

    
    
    
  })
}

