
library(shiny)


shinyServer(function(input, output) {
    ged <- reactive({
       req(input$read_file)
       tidyged.io::read_gedcom(input$read_file$datapath)
   })
    
    output$file_summary <- DT::renderDataTable({
       ged()
    })

})
