


source_details_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    shiny::fluidRow(
      shiny::column(12,
                    shiny::textInput(ns("title"), "Title", width="85%")
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::textInput(ns("abbr_title"), "Short title")
      ),
      shiny::column(6,
                    shiny::textInput(ns("originator"), "Originator")
      )
    ),
    
    shiny::textAreaInput(ns("pub_details"), "Publication details", resize = "vertical") |>
      shiny::tagAppendAttributes(style = 'width: 85%;'),
    
    shiny::textAreaInput(ns("text"), "Source text", resize = "vertical") |>
      shiny::tagAppendAttributes(style = 'width: 85%;'),
    
    source_repo_ui(ns("repos"))
   
  )
  
}


source_details_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    source_repo_server("repos", r)
    
    # Load in data from gedcom object -----------------------------------------
    shiny::observe({
      req(r$ged)
      
      sour_xref <- r$ged$record[r$sour_rows[1]]
      
      shiny::updateTextInput(inputId = "originator", 
                             value = tidyged.internals::gedcom_value(r$ged, sour_xref, "AUTH", 1))
      shiny::updateTextInput(inputId = "title", 
                             value = tidyged.internals::gedcom_value(r$ged, sour_xref, "TITL", 1))
      shiny::updateTextInput(inputId = "abbr_title", 
                             value = tidyged.internals::gedcom_value(r$ged, sour_xref, "ABBR", 1))
      shiny::updateTextAreaInput(inputId = "pub_details", 
                                 value = tidyged.internals::gedcom_value(r$ged, sour_xref, "PUBL", 1))
      shiny::updateTextAreaInput(inputId = "text", 
                                 value = tidyged.internals::gedcom_value(r$ged, sour_xref, "TEXT", 1))
    }) |> 
      shiny::bindEvent(r$file_count)
    
    
    # Edit originator -------------------------------------------------------------
    shiny::observe({
      originator <- process_input(input$originator)
      err <- tidyged.internals::chk_source_originator(originator, 1)
      shinyFeedback::feedbackDanger("originator", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "sour_rows", r$ged$record[r$sour_rows[1]], 1, "AUTH", originator)
    }) |> 
      shiny::bindEvent(input$originator, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Title ---------------------------------------------------------------------
    shiny::observe({
      title <- process_input(input$title)
      err <- tidyged.internals::chk_source_descriptive_title(title, 1)
      shinyFeedback::feedbackDanger("title", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "sour_rows", r$ged$record[r$sour_rows[1]], 1, "TITL", title)
    }) |> 
      shiny::bindEvent(input$title, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Abbreviated title -----------------------------------------------------------
    shiny::observe({
      abbr_title <- process_input(input$abbr_title)
      err <- tidyged.internals::chk_source_filed_by_entry(abbr_title, 1)
      shinyFeedback::feedbackDanger("abbr_title", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "sour_rows", r$ged$record[r$sour_rows[1]], 1, "ABBR", abbr_title)
    }) |> 
      shiny::bindEvent(input$abbr_title, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Publication details -----------------------------------------------------------
    shiny::observe({
      pub_details <- process_input(input$pub_details)
      err <- tidyged.internals::chk_source_publication_facts(pub_details, 1)
      shinyFeedback::feedbackDanger("pub_details", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "sour_rows", r$ged$record[r$sour_rows[1]], 1, "PUBL", pub_details)
    }) |> 
      shiny::bindEvent(input$pub_details, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Source text -----------------------------------------------------------------
    shiny::observe({
      text <- process_input(input$text)
      err <- tidyged.internals::chk_text_from_source(text, 1)
      shinyFeedback::feedbackDanger("text", !is.null(err), err)
      req(is.null(err), cancelOutput = TRUE)
      update_ged_value(r, "sour_rows", r$ged$record[r$sour_rows[1]], 1, "TEXT", text)
    }) |> 
      shiny::bindEvent(input$text, ignoreNULL = FALSE, ignoreInit = TRUE)
    
  })
}


