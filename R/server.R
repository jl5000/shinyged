

shiny::shinyServer(function(input, output, session) {

    r <- shiny::reactiveValues(ged = NULL,
                               subm_rows = NULL,
                               subm_addr_rows = NULL,
                               head_rows = NULL,
                               head_file_sour_rows = NULL)
    
    shiny::observeEvent(input$read_file, {
        if(!is.null(r$ged)) {
            shiny::showModal(
                shiny::modalDialog(
                    title = "Export existing file?",
                    easyClose = FALSE,
                    footer = shiny::tagList(
                        shiny::modalButton("Cancel"),
                        shiny::actionButton("discard", "Discard"),
                        shiny::actionButton("export_before_new", "Export")
                    )
                )
            )
        } else {
            r$ged <- tidyged.io::read_gedcom(input$read_file$datapath)
        }
    })
    
    shiny::observeEvent(input$create_gedcom, {
        if(!is.null(r$ged)) {
            shiny::showModal(
                shiny::modalDialog(
                    title = "Export existing file?",
                    easyClose = FALSE,
                    footer = shiny::tagList(
                        shiny::modalButton("Cancel"),
                        shiny::actionButton("discard", "Discard"),
                        shiny::actionButton("export_before_new", "Export")
                    )
                )
            )
        } else {
            r$ged <- tidyged::gedcom()
        }
    })
    
    shiny::observeEvent(input$discard, {
        r$ged <- tidyged::gedcom()
        shiny::removeModal()
    })
    
    shiny::observeEvent(r$ged, {
        shinyjs::show("tabs")
        shinyjs::enable("export_gedcom")
    })
    
    file_server("file", r)
    submitter_server("subm", r)
    # individual_server("indi", r)
    # family_server("famg", r)
    # source_server("sour", r)
    # repository_server("repo", r)
    # note_server("note", r)
    # multimedia_server("media", r)
    
    output$export_gedcom <- shiny::downloadHandler(
        filename = "from_app.ged",
        content = function(file) {
            tidyged.io::write_gedcom(r$ged, file)
        }
    )
    
    shiny::observeEvent(input$export_before_new, {
        shiny::removeModal()
        output$export_gedcom #TODO: Get this working
    })
    
    
    

})

