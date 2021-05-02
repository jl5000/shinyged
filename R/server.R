


shiny::shinyServer(function(input, output, session) {
    ged <- shiny::reactive({
       req(input$read_file)
       tidyged.io::read_gedcom(input$read_file$datapath)
   })
    
    
    
    fileServer("file", ged)
    individualServer("indi", ged)
    familyServer("famg", ged)
    sourceServer("sour", ged)
    repositoryServer("repo", ged)
    noteServer("note", ged)
    multimediaServer("media", ged)
    
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
