

shiny::shinyServer(function(input, output, session) {

    r <- shiny::reactiveValues(ged = NULL)
    
    shiny::observeEvent(input$read_file, {
        r$ged <- tidyged.io::read_gedcom(input$read_file$datapath)
    })
    
    # file_server("file", r)
    # submitter_server("subm", r)
    # individual_server("indi", r)
    # family_server("famg", r)
    # source_server("sour", r)
    # repository_server("repo", r)
    # note_server("note", r)
    # multimedia_server("media", r)
    
    observeEvent(input$read_file, {
        shinyjs::show("tabs")
        shinyjs::enable("export_gedcom")
    })
    
    output$export_gedcom <- shiny::downloadHandler(
        filename = "from_app.ged",
        content = function(file) {
            tidyged.io::write_gedcom(r$ged, file)
        }
    )
    
    

})

