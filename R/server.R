


shiny::shinyServer(function(input, output, session) {
    ged <- shiny::reactive({
       req(input$read_file)
       tidyged.io::read_gedcom(input$read_file$datapath)
   })
    
    output$file_summary <- shiny::renderPrint({
        req(ged())
        str(ged())
    })
    
    individualServer("indi", ged())
    familyServer("famg", ged())
    
    
    sour_list <- shiny::reactive({
        req(ged())
        tidyged::xrefs_sour(ged()) %>% 
            tidyged::describe_records(ged(), ., short_desc = TRUE)
    })
    
    repo_list <- shiny::reactive({
        req(ged())
        tidyged::xrefs_repo(ged()) %>% 
            tidyged::describe_records(ged(), ., short_desc = TRUE)
    })
    
    note_list <- shiny::reactive({
        req(ged())
        tidyged::xrefs_note(ged()) %>% 
            tidyged::describe_records(ged(), ., short_desc = TRUE)
    })
    
    media_list <- shiny::reactive({
        req(ged())
        tidyged::xrefs_media(ged()) %>% 
            tidyged::describe_records(ged(), ., short_desc = TRUE)
    })
    
    observeEvent(input$read_file, {
        shinyjs::show("tabs")
        shinyjs::enable("export_gedcom")
        shiny::updateSelectInput(session = session, inputId = "sour_list", choices = sour_list())
        shiny::updateSelectInput(session = session, inputId = "repo_list", choices = repo_list())
        shiny::updateSelectInput(session = session, inputId = "note_list", choices = note_list())
        shiny::updateSelectInput(session = session, inputId = "media_list", choices = media_list())
    })
    
    output$export_gedcom <- shiny::downloadHandler(
        filename = "from_app.ged",
        content = function(file) {
            tidyged.io::write_gedcom(ged(), file)
        }
    )
    
    

})
