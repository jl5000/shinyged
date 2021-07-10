
options(shiny.error = browser)
shiny::shinyServer(function(input, output, session) {

    r <- shiny::reactiveValues(ged = NULL,
                               citation_rows = NULL,
                               subm_rows = NULL,
                               subm_addr_rows = NULL,
                               head_rows = NULL,
                               head_file_sour_rows = NULL,
                               indi_rows = NULL,
                               indi_name_rows = NULL,
                               indi_fact_rows = NULL,
                               indi_links_rows = NULL,
                               famg_rows = NULL,
                               famg_event_rows = NULL)
    
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
        }
    })
    
    shiny::observeEvent(input$discard_and_read, {
        shiny::removeModal()
        r$ged <- tidyged.io::read_gedcom(input$read_file$datapath)
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
        }
    })
    
    shiny::observeEvent(input$discard_and_create, {
        shiny::removeModal()
        r$ged <- tidyged::gedcom()
    })
    
    shiny::observeEvent(r$ged, {
        shinyjs::show("tabs")
        shinyjs::enable("export_gedcom")
    })
    
    file_server("file", r)
    submitter_server("subm", r)
    individual_server("indi", r)
    family_server("famg", r)
    # source_server("sour", r)
    # repository_server("repo", r)
    # note_server("note", r)
    # multimedia_server("media", r)
    ged_debug_server("debug", r)
    
    output$export_gedcom <- shiny::downloadHandler(
        filename = "from_shinyged.ged",
        content = function(file) {
            tidyged.io::write_gedcom(r$ged, file)
        }
    )
    

})

