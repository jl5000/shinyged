
library(shiny)


shinyServer(function(input, output) {
   observeEvent(input$read_file, {
       gedcom <- tidyged.io::read_gedcom()
   })
    
    output$file_summary <- renderText({
        summary(gedcom)
    })

})
