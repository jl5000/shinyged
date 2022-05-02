
date_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    shiny::textOutput(ns("date_string")),
    shiny::actionButton(ns("date_click"), "Edit date")
  )
  
}


date_server <- function(id, r, section_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    date_row <- shiny::reactive({
      which(r$ged$tag == "DATE")
    })
    
    # Show pop up
    shiny::observe({
      browser()
      curr_date <- r$ged$value[date_row()]
      if(length(curr_date) == 1){
        date1_prefix <- stringr::str_extract(curr_date, "^FROM|TO|BEF|AFT|BET")
        if(is.na(date1_prefix)) date1_prefix <- ""
        
        approx <- stringr::str_extract(curr_date, "^EST|CAL|ABT")
        if(is.na(approx)) approx <- ""
        
        dates <- unlist(stringr::str_extract_all(curr_date, tidyged.internals::reg_date(only=FALSE)))
        
        day1 <- stringr::str_extract(dates[1], "\\b\\d{1,2}\\b")
        month1 <- stringr::str_extract(dates[1], "\\b[A-Z]{3}\\b")
        year1 <- stringr::str_extract(dates[1], "\\b\\d{3,4}\\b")
        
        if(length(dates) == 2){
          day2 <- stringr::str_extract(dates[2], "\\b\\d{1,2}\\b")
          month2 <- stringr::str_extract(dates[2], "\\b[A-Z]{3}\\b")
          year2 <- stringr::str_extract(dates[2], "\\b\\d{3,4}\\b")
        } else {
          day2 <- month2 <- year2 <- NULL
        }
      } else {
        date1_prefix <- day1 <- month1 <- year1 <- day2 <- month2 <- year2 <- NULL
        approx <- ""
      }
      
      shiny::modalDialog(
        title = "Edit date",
        
        footer = shiny::tagList(
          shiny::actionButton(ns("save_date"), "Save"),
          shiny::actionButton(ns("delete_date"), "Delete"),
          shiny::modalButton("Cancel")
        ),
        
        shiny::fluidRow(
          shiny::column(3, style = 'margin-top:25px',
                        shiny::selectInput(ns("date1_prefix"), label = NULL, selected = date1_prefix,
                                           choices = c("",From = "FROM",To = "TO",
                                                       Before = "BEF",After = "AFT",Between = "BET"))),
          
          shiny::column(2, shiny::numericInput(ns("day1"), "Day", value = day1, min = 1, max = 31)),
          shiny::column(3, shiny::selectInput(ns("month1"), "Month", choices = c("",month.abb), selected = month1)),
          shiny::column(3, shiny::numericInput(ns("year1"), "Year", value = year1, min = 0, max = 2050))
        ),
        shiny::fluidRow(
          shiny::column(3, style = 'margin-top:25px',
                        shiny::textOutput(ns("date2_prefix"))),
          
          shiny::column(2, shiny::numericInput(ns("day2"), "Day", value = day2, min = 1, max = 31)),
          shiny::column(3, shiny::selectInput(ns("month2"), "Month", choices = c("",month.abb), selected = month2)),
          shiny::column(3, shiny::numericInput(ns("year2"), "Year", value = year2, min = 0, max = 2050))
          
        ),
        
        shiny::radioButtons(ns("approx"), label = "Is the date approximated?", inline = TRUE, selected = approx,
                            choices = c(No = "",About = "ABT",Calculated = "CAL",Estimated = "EST")),
        
        
        
      ) |> shiny::showModal()
      
    }) |> 
      shiny::bindEvent(input$date_click)
    
    # Enable/disable second date and approx buttons
    shiny::observe({
      shinyjs::toggleState("day2", input$date1_prefix %in% c("FROM","BET"))
      shinyjs::toggleState("month2", input$date1_prefix %in% c("FROM","BET"))
      shinyjs::toggleState("year2", input$date1_prefix %in% c("FROM","BET"))
      shinyjs::toggleState("approx", input$date1_prefix == "")
    }) |> 
      shiny::bindEvent(input$date1_prefix)
    
    # Enable/disable save button
    shiny::observe({
      date1_valid <- (!is.null(input$day1) & !is.null(input$month1)) |
        !is.null(input$year1)
      
      if(!is.null(input$date1_prefix) && input$date1_prefix == "BET"){
        date2_valid <- (!is.null(input$day2) & !is.null(input$month2)) |
          !is.null(input$year2)
        
        shinyjs::toggleState("save_date", date1_valid && date2_valid)
      } else {
        shinyjs::toggleState("save_date", date1_valid)
      }
    })
    
    # Update date 2 prefix
    output$date2_prefix <- shiny::renderText({
      d2_prefix <- ""
      if(!is.null(input$date1_prefix)){
        if(input$date1_prefix == "FROM") d2_prefix <- "To"
        if(input$date1_prefix == "BET") d2_prefix <- "and"
      }
      d2_prefix
    }) |> 
      shiny::bindEvent(input$date1_prefix)
    
    # Delete the date
    shiny::observe({
      r$ged <- r$ged[-date_row(),]
    }) |> 
      shiny::bindEvent(input$delete_date)
    
    # Update date in r$ged
    shiny::observe({
      shiny::removeModal()
      if(is.null(input$day1)) day1 <- integer() else day1 <- input$day1
      if(input$month1 == "") month1 <- integer() else month1 <- which(month.abb == input$month1)
      if(is.null(input$year1)) year1 <- integer() else year1 <- input$year1
      date1 <- tidyged.internals::date_calendar(year1, month1, day1)
      
      
    }) |> 
      shiny::bindEvent(input$save_date)
    
    # Update date output
    output$date_string <- shiny::renderText({
      if(length(date_row()) == 1){
        r$ged$value[date_row()]
      } else {
        "No date defined"
      }
    })
    
  })
}
