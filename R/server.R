


shiny::shinyServer(function(input, output) {
    ged <- shiny::reactive({
       req(input$read_file)
       tidyged.io::read_gedcom(input$read_file$datapath)
   })
    
    output$file_summary <- DT::renderDataTable({
       ged()
    })
    
    output$export_gedcom <- shiny::downloadHandler(
        filename = "from_app.ged",
        content = function(file) {
            tidyged.io::write_gedcom(ged, file)
        }
    )

})
