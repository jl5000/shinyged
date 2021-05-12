

shiny::shinyServer(function(input, output, session) {

    ged <- shiny::reactive({
       req(input$read_file)
       tidyged.io::read_gedcom(input$read_file$datapath)
   })
    
    file_server("file", ged)
    submitter_server("subm", ged)
    individual_server("indi", ged)
    family_server("famg", ged)
    source_server("sour", ged)
    repository_server("repo", ged)
    note_server("note", ged)
    multimedia_server("media", ged)
    
    observeEvent(input$read_file, {
        shinyjs::show("tabs")
        shinyjs::enable("export_gedcom")

    })
    
    output$export_gedcom <- shiny::downloadHandler(
        filename = "from_app.ged",
        content = function(file) {
            tidyged.io::write_gedcom(ged(), file)
        }
    )
    
    

})

