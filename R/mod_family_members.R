


family_members_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    DT::DTOutput(ns("table")),

    shiny::br(),
    
    
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(ns("add_spouse"), "Add spouse"),
                    shiny::actionButton(ns("add_child"), "Add child"),
                    shiny::actionButton(ns("delete_member"), "Remove member") %>% shinyjs::disabled(),
                    shiny::actionButton(ns("update_relationship"), "Update child relationship") %>% shinyjs::disabled(),
                    notes_ui(ns("member_notes")) %>% shinyjs::hidden()
                    )
    )
    
   
  )
  
}


family_members_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #notes_server("member_notes", r, "family_link_rows")
    
    # create members table
    members_table <- shiny::reactive({
      req(r$ged, r$famg_rows)

      famg_xref <- r$ged$record[r$famg_rows][1]
      
      mtab <- dplyr::filter(r$ged[r$famg_rows,], level == 1, tag %in% c("HUSB","WIFE","CHIL")) %>% 
        dplyr::mutate(tag = factor(tag, levels = c("HUSB","WIFE","CHIL"), ordered = TRUE)) %>% 
        dplyr::arrange(tag) %>% 
        dplyr::mutate(tag = as.character(tag))
      
      marr_type <- tidyged.internals::gedcom_value(r$ged[r$famg_rows,], famg_xref, "TYPE", 2, "MARR")
      type <- dplyr::if_else(mtab$tag == "CHIL", "Child", "Partner")
      memb_names <- lapply(mtab$value, tidyged::describe_indi, gedcom = r$ged, name_only = TRUE)
      
      get_pedi <- function(ged, fam_xref, chil_xref){
        # need to get section first as they might be a child of multiple families
        tidyged.internals::identify_section(ged, 1, "FAMC", fam_xref, chil_xref, TRUE) %>% 
          dplyr::slice(ged, .) %>% 
          tidyged.internals::gedcom_value(chil_xref, "PEDI", 2, "FAMC")
      }
      
      rel_spou <- rep(marr_type, sum(type == "Partner"))
      
      rel_chil <- lapply(mtab$value[type == "Child"],
                         get_pedi, ged=r$ged, fam_xref=famg_xref)
      
      rel <- c(rel_spou, rel_chil)
         
      tibble::tibble(xref = mtab$value, name = memb_names, type = type, rel)
      
    })
    
    # Display members table
    output$table <- DT::renderDataTable({
      req(members_table)
      
      DT::datatable(members_table(), rownames = FALSE, selection = "single",
                    filter = "none", colnames = c("xref", "Name", "Type", "Relationship"),
                    options = list(searching = FALSE, paging = FALSE))
    })
    
    # Enable/disable add_child/spouse buttons
    shiny::observe({
      req(r$ged, members_table)
      
      shinyjs::toggleState("add_child", tidyged::num_indi(r$ged) > 0)
      shinyjs::toggleState("add_spouse", sum(members_table()$type == "Partner") < 2)
    }) %>% 
      shiny::bindEvent(members_table())
    
    # Create list of individuals
    indis <- shiny::reactive({
      req(r$ged)
      
      tidyged::xrefs_indi(r$ged) %>% 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
    })
    
    # Open modal to add child
    shiny::observe({
      req(indis)
      
      shiny::modalDialog(
        title = "Add a child to a family group",
        
        footer = shiny::tagList(
          shiny::actionButton(ns("add_child_now"), "Add") %>% shinyjs::disabled(),
          modalButton("Cancel")
        ),
        
        shiny::fluidRow(
          shiny::column(9,
                        shiny::selectizeInput(ns("new_child"), label = "Choose an individual", 
                                              choices = indis(), multiple = TRUE, width = "500px", 
                                              options = list(maxItems = 1)),
                        
          ),
          shiny::column(3,
                        shiny::selectInput(ns("pedigree"), "Relationship", 
                                           choices = tidyged.internals::val_pedigree_linkage_types()),
          )
        ),
        
      ) %>% shiny::showModal()
      
    }) %>% 
      shiny::bindEvent(input$add_child)
    
    # Enable/disable add child button
    shiny::observe({
      xref <- stringr::str_extract(input$new_child, tidyged.internals::reg_xref(FALSE))

      shinyjs::toggleState("add_child_now", !is.null(input$new_child) &&
                             !xref %in% members_table()$xref)
    }) %>% 
      shiny::bindEvent(input$new_child, ignoreNULL = FALSE)
    
    # Add child to family
    shiny::observe({
      chil_xref <- stringr::str_extract(input$new_child, tidyged.internals::reg_xref(FALSE))
      famg_xref <- r$ged$record[r$famg_rows][1]
      
      r$ged <- tidyged::add_indi_links_to_families(r$ged, xref = chil_xref, #TODO: Add famg_xref to tidyged
                                                   linkage_type = input$pedigree)
    }) %>% 
      shiny::bindEvent(input$add_child_now)
    
    # Open modal to add spouse
    shiny::observe({
      req(indis)
      
      shiny::modalDialog(
        title = "Add a spouse to a family group",
        
        footer = shiny::tagList(
          shiny::actionButton(ns("add_spouse_now"), "Add") %>% shinyjs::disabled(),
          modalButton("Cancel")
        ),
        
        shiny::fluidRow(
          shiny::column(12,
                        shiny::selectizeInput(ns("new_spouse"), label = "Choose an individual", 
                                              choices = indis(), multiple = TRUE, width = "500px", 
                                              options = list(maxItems = 1)),
                        
          )
        ),
        
        shiny::helpText("You can change the relationship type in the Events tab.")
        
      ) %>% shiny::showModal()
      
    }) %>% 
      shiny::bindEvent(input$add_spouse)
    
    # Enable/disable add spouse button
    shiny::observe({
      xref <- stringr::str_extract(input$new_spouse, tidyged.internals::reg_xref(FALSE))
      
      shinyjs::toggleState("add_spouse_now", !is.null(input$new_spouse) &&
                             !xref %in% members_table()$xref)
    }) %>% 
      shiny::bindEvent(input$new_spouse, ignoreNULL = FALSE)
    
    # Add spouse to family
    shiny::observe({
      spou_xref <- stringr::str_extract(input$new_spouse, tidyged.internals::reg_xref(FALSE))
      famg_xref <- r$ged$record[r$famg_rows][1]
      
      r$ged <- tidyged::add_indi_links_to_families(r$ged, xref = spou_xref #TODO: Add famg_xref to tidyged
                                                   )
    }) %>% 
      shiny::bindEvent(input$add_spouse_now)
    
    # Enable disable remove/update buttons
    shiny::observe({
      shinyjs::toggleState("delete_member", !is.null(input$table_rows_selected))
      shinyjs::toggleState("update_relationship", !is.null(input$table_rows_selected) &&
                             members_table()$type[input$table_rows_selected] == "Child")
    }) %>% 
      shiny::bindEvent(input$table_rows_selected, ignoreNULL = FALSE)
    
  })
}



