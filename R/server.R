

shiny::shinyServer(function(input, output, session) {

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
    
    shiny::observeEvent(input$read_file, {
        if(!is.null(r$ged)) {
            shiny::showModal(
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
                )
            )
        } else {
            r$ged <- tidyged.io::read_gedcom(input$read_file$datapath)
            r$file_count <- r$file_count + 1
        }
    })
    
    shiny::observeEvent(input$discard_and_read, {
        shiny::removeModal()
        r$ged <- tidyged.io::read_gedcom(input$read_file$datapath)
        r$file_count <- r$file_count + 1
    })
    
    shiny::observeEvent(input$create_gedcom, {
        if(!is.null(r$ged)) {
            shiny::showModal(
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
                )
            )
        } else {
            r$ged <- tidyged::gedcom()
            r$file_count <- r$file_count + 1
        }
    })
    
    shiny::observeEvent(input$discard_and_create, {
        shiny::removeModal()
        r$ged <- tidyged::gedcom()
        r$file_count <- r$file_count + 1
    })
    
    shiny::observeEvent(r$ged, {
        shinyjs::show("tabs")
        shinyjs::enable("export_gedcom")
    })
    
    file_server("file", r)
    submitter_server("subm", r)

    shiny::observeEvent({input$tabset == "indi_tab"},once=TRUE,ignoreInit = TRUE, {
        individual_server("indi", r)
    })
    shiny::observeEvent({input$tabset == "famg_tab"},once=TRUE,ignoreInit = TRUE, {
        family_server("famg", r)
    })
    shiny::observeEvent({input$tabset == "sour_tab"},once=TRUE,ignoreInit = TRUE, {
        source_server("sour", r)
    })
    shiny::observeEvent({input$tabset == "repo_tab"},once=TRUE,ignoreInit = TRUE, {
        repository_server("repo", r)
    })
    shiny::observeEvent({input$tabset == "note_tab"},once=TRUE,ignoreInit = TRUE, {
        # note_server("note", r)
    })
    shiny::observeEvent({input$tabset == "media_tab"},once=TRUE,ignoreInit = TRUE, {
        multimedia_server("media", r)
    })
    shiny::observeEvent({input$tabset == "debug_tab"},once=TRUE,ignoreInit = TRUE, {
        ged_debug_server("debug", r)
    })
    
    tools_server("tools", r)
    
    
    output$export_gedcom <- shiny::downloadHandler(
        filename = "from_shinyged.ged",
        content = function(file) {
            tidyged.io::write_gedcom(r$ged, file)
        }
    )
    

})

