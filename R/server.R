

function(input, output, session) {
    
    # The tidyged object and rows of structures that could occur multiple times
    r <- shiny::reactiveValues(ged = NULL,
                               file_count = 0, # this increases every time r$ged changes and triggers re-population of inputs
                               addr_rows = NULL,
                               citation_rows = NULL,
                               subm_rows = NULL,
                               head_rows = NULL,
                               head_file_sour_rows = NULL,
                               indi_rows = NULL,
                               indi_name_rows = NULL,
                               indi_fact_rows = NULL,
                               indi_links_rows = NULL,
                               famg_rows = NULL,
                               famg_event_rows = NULL,
                               media_rows = NULL,
                               sour_rows = NULL,
                               repo_rows = NULL)
    
    
    file_server("file", r)
    tools_server("tools", r)
    
    # Only run modules if tab is clicked
    shiny::observe({
        if(input$tabset == "Submitter") submitter_server("subm", r)
        if(input$tabset == "Individuals") individual_server("indi", r)
        if(input$tabset == "Families") family_server("famg", r)
        if(input$tabset == "Sources") source_server("sour", r)
        if(input$tabset == "Repositories") repository_server("repo", r)
        if(input$tabset == "Notes") note_server("note", r)
        if(input$tabset == "Multimedia") multimedia_server("media", r)
        if(input$tabset == "GEDCOM") ged_debug_server("debug", r)
    }) %>% 
        shiny::bindEvent(input$tabset, ignoreInit = TRUE)
    
    # Import file
    shiny::observe({
        if(!is.null(r$ged)) {
            
            shiny::modalDialog(
                shiny::h4("If you continue, the current app data will be overwritten.",
                          shiny::br(),
                          "Would you like to Cancel and export your data to file first, 
                              or Discard existing data and import the file?"),
                title = "Continue?",
                easyClose = FALSE,
                footer = shiny::tagList(
                    shiny::modalButton("Cancel"),
                    shiny::actionButton("discard_and_read", "Discard")
                )
            ) %>% shiny::showModal()
            
        } else {
            r$ged <- tidyged.io::read_gedcom(input$read_file$datapath)
            r$file_count <- r$file_count + 1
        }
    }) %>% 
        shiny::bindEvent(input$read_file)
    
    # Discard previous and import new
    shiny::observe({
        shiny::removeModal()
        r$ged <- tidyged.io::read_gedcom(input$read_file$datapath)
        r$file_count <- r$file_count + 1
    }) %>% 
        shiny::bindEvent(input$discard_and_read)
    
    # Create new
    shiny::observe({
        if(!is.null(r$ged)) {
            
            shiny::modalDialog(
                shiny::h4("If you continue, the current app data will be overwritten.",
                          shiny::br(),
                          "Would you like to Cancel and export your data to file first, 
                              or Discard existing data and create a new file?"),
                title = "Continue?",
                easyClose = FALSE,
                footer = shiny::tagList(
                    shiny::modalButton("Cancel"),
                    shiny::actionButton("discard_and_create", "Discard")
                )
            ) %>% shiny::showModal()
            
        } else {
            r$ged <- tidyged::gedcom()
            r$file_count <- r$file_count + 1
        }
    }) %>% 
        shiny::bindEvent(input$create_gedcom)
    
    # Discard previous and create new
    shiny::observe({
        shiny::removeModal()
        r$ged <- tidyged::gedcom()
        r$file_count <- r$file_count + 1
    }) %>% 
        shiny::bindEvent(input$discard_and_create)
    
    # Show tabs
    shiny::observe({
        shinyjs::show("tabs")
        shinyjs::enable("export_gedcom")
    }) %>% 
        shiny::bindEvent(r$ged)
    
    # Export
    output$export_gedcom <- shiny::downloadHandler(
        filename = "from_shinyged.ged",
        content = function(file) {
            tidyged.io::write_gedcom(r$ged, file)
        }
    )
    
    
}

