


family_members_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    DT::DTOutput(ns("table")),

    shiny::br(),
    
    
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(ns("add_partner"), "Add partner"),
                    shiny::actionButton(ns("add_child"), "Add child"),
                    shiny::actionButton(ns("delete_member"), "Remove member") %>% shinyjs::disabled(),
                    shiny::actionButton(ns("update_relationship"), "Update child relationship") %>% shinyjs::disabled(),
                    notes_ui(ns("member_notes"))
                    ),
    ),
    
  )
  
}


family_members_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    notes_server("member_notes", r, "family_link_rows")
    

    # Create members table ----------------------------------------------------
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
      
      rel_spou <- rep(marr_type, sum(type == "Partner"))
      
      rel_chil <- lapply(mtab$value[type == "Child"], get_pedi, ged=r$ged, fam_xref=famg_xref)
      
      rel <- c(rel_spou, rel_chil)
         
      tibble::tibble(xref = mtab$value, name = memb_names, type = type, rel)
      
    })
    

    # Display members table ---------------------------------------------------
    output$table <- DT::renderDataTable({
      req(members_table)
     
      DT::datatable(members_table(), rownames = FALSE, 
                    selection = list(mode = "single", selected = r$member_row_selected),
                    filter = "none", colnames = c("xref", "Name", "Type", "Relationship"),
                    options = list(searching = FALSE, paging = FALSE))
    })
    

    # Enable/disable add_child/partner buttons --------------------------------
    shiny::observe({
      req(r$ged, members_table)
      
      shinyjs::toggleState("add_child", tidyged::num_indi(r$ged) > 0)
      shinyjs::toggleState("add_partner", sum(members_table()$type == "Partner") < 2)
    }) %>% 
      shiny::bindEvent(members_table())
    

    # Create list of individuals ----------------------------------------------
    indis <- shiny::reactive({
      req(r$ged)
      
      tidyged::xrefs_indi(r$ged) %>% 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
    })
    

    # Open modal to add child -------------------------------------------------
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
        
        shiny::fluidRow(id = ns("new_child_summary_section"),
                        shiny::column(12,
                                      individual_summary_ui(ns("new_child_summary"))
                        )
        ) %>% shinyjs::hidden()
        
      ) %>% shiny::showModal()
      
    }) %>% 
      shiny::bindEvent(input$add_child)
    

    # Enable/disable add child button and show person summary -------------------------
    shiny::observe({
      xref <- stringr::str_extract(input$new_child, tidyged.internals::reg_xref(FALSE))

      shinyjs::toggleState("add_child_now", !is.null(input$new_child) &&
                             !xref %in% members_table()$xref)
      
      shinyjs::toggle("new_child_summary_section", condition = !is.null(input$new_child))
      
      if(!is.null(input$new_child)){
        r$indi_rows <- which(r$ged$record == xref)
        individual_summary_server("new_child_summary", r)
      } 
      
    }) %>% 
      shiny::bindEvent(input$new_child, ignoreNULL = FALSE)
    

    # Add child to family -----------------------------------------------------
    shiny::observe({
      chil_xref <- stringr::str_extract(input$new_child, tidyged.internals::reg_xref(FALSE))
      famg_xref <- r$ged$record[r$famg_rows][1]
      
      r$ged <- tidyged::add_indi_links_to_families(r$ged, xref = chil_xref, famg_xref_chil = famg_xref,
                                                   child_linkage_type = input$pedigree)
      
      shiny::removeModal()
    }) %>% 
      shiny::bindEvent(input$add_child_now)
    

    # Open modal to add partner -----------------------------------------------
    shiny::observe({
      req(indis)
      
      shiny::modalDialog(
        title = "Add a partner to a family group",
        
        footer = shiny::tagList(
          shiny::actionButton(ns("add_partner_now"), "Add") %>% shinyjs::disabled(),
          modalButton("Cancel")
        ),
        
        shiny::fluidRow(
          shiny::column(12,
                        shiny::selectizeInput(ns("new_partner"), label = "Choose an individual", 
                                              choices = indis(), multiple = TRUE, width = "500px", 
                                              options = list(maxItems = 1)),
                        
          )
        ),
        
        shiny::helpText("You can change the relationship type in the Events tab."),
        
        shiny::fluidRow(id = ns("new_partner_summary_section"),
                        shiny::column(12,
                                      individual_summary_ui(ns("new_partner_summary"))
                        )
        ) %>% shinyjs::hidden()
        
      ) %>% shiny::showModal()
      
    }) %>% 
      shiny::bindEvent(input$add_partner)
    

    # Enable/disable add partner button and show person summary -------------
    shiny::observe({
      xref <- stringr::str_extract(input$new_partner, tidyged.internals::reg_xref(FALSE))
      
      shinyjs::toggleState("add_partner_now", !is.null(input$new_partner) &&
                             !xref %in% members_table()$xref)
      
      shinyjs::toggle("new_partner_summary_section", condition = !is.null(input$new_partner))
      
      if(!is.null(input$new_partner)){
        r$indi_rows <- which(r$ged$record == xref)
        individual_summary_server("new_partner_summary", r)
      } 
    }) %>% 
      shiny::bindEvent(input$new_partner, ignoreNULL = FALSE)
    

    # Add partner to family ---------------------------------------------------
    shiny::observe({
      spou_xref <- stringr::str_extract(input$new_partner, tidyged.internals::reg_xref(FALSE))
      famg_xref <- r$ged$record[r$famg_rows][1]
      
      r$ged <- tidyged::add_indi_links_to_families(r$ged, xref = spou_xref, famg_xref_spou = famg_xref)
                                                   
      shiny::removeModal()
    }) %>% 
      shiny::bindEvent(input$add_partner_now)
    

    # Determine link tag ------------------------------------------------------
    family_link_tag <- shiny::reactive({
      req(members_table, input$table_rows_selected)
      
      if(members_table()$type[input$table_rows_selected] == "Child"){
        "FAMC"
      } else {
        "FAMS"
      }
    })
    

    # Update r$family_link_rows  ----------------------------------------------
    shiny::observe({
      if(is.null(input$table_rows_selected)){
        r$family_link_rows <- NULL
      } else {
        xref <- members_table()$xref[input$table_rows_selected]
        famg_xref <- r$ged$record[r$famg_rows][1]
        r$family_link_rows <- tidyged.internals::identify_section(r$ged, 1, family_link_tag(), 
                                                                  famg_xref, xref, TRUE)
      }
 
    }) %>% 
      shiny::bindEvent(r$ged, input$table_rows_selected)
    

    # Record selected row for when table updates ------------------------------
    shiny::observe({ 
      r$member_row_selected <- input$table_rows_selected
    }) %>% 
      shiny::bindEvent(r$ged)
    

    # Enable/disable remove/update buttons ------------------------------------
    shiny::observe({
      shinyjs::toggleState("delete_member", !is.null(input$table_rows_selected))
      shinyjs::toggleState("update_relationship", !is.null(input$table_rows_selected) &&
                             members_table()$type[input$table_rows_selected] == "Child")
    }) %>% 
      shiny::bindEvent(input$table_rows_selected, ignoreNULL = FALSE)
    

    # Remove member -----------------------------------------------------------
    shiny::observe({
      xref <- members_table()$xref[input$table_rows_selected]
      famg_xref <- r$ged$record[r$famg_rows][1]
      
      r$ged <- r$ged %>% 
        dplyr::filter(!(record == famg_xref & level == 1 & value == xref)) %>% 
        tidyged.internals::remove_section(1, family_link_tag(), famg_xref, xref, TRUE)
    }) %>% 
      shiny::bindEvent(input$delete_member)
    
    

    # Open modal to update relationship ---------------------------------------
    shiny::observe({

      xref <- members_table()$xref[input$table_rows_selected]
      famg_xref <- r$ged$record[r$famg_rows][1]
      current_rel <- get_pedi(r$ged, famg_xref, xref)
      
      shiny::modalDialog(
        title = "Update a child's relationship to a family",
        
        footer = shiny::tagList(
          shiny::actionButton(ns("update_relationship_now"), "Update") %>% shinyjs::disabled(),
          modalButton("Cancel")
        ),
        
        shiny::fluidRow(
          shiny::column(3,
                        shiny::selectInput(ns("updated_pedigree"), "Relationship", selected = current_rel,
                                           choices = tidyged.internals::val_pedigree_linkage_types())
          )
        ),
        
      ) %>% shiny::showModal()
      
    }) %>% 
      shiny::bindEvent(input$update_relationship)
    

    # Enable/disable update relationship button -------------------------------
    shiny::observe({
      xref <- members_table()$xref[input$table_rows_selected]
      famg_xref <- r$ged$record[r$famg_rows][1]
      current_rel <- get_pedi(r$ged, famg_xref, xref)
      shinyjs::toggleState("update_relationship_now", current_rel != input$updated_pedigree)
    }) %>% 
      shiny::bindEvent(input$updated_pedigree, ignoreNULL = FALSE)
    

    # Update child relationship -----------------------------------------------
    shiny::observe({
      xref <- members_table()$xref[input$table_rows_selected]
      update_ged_value(r, "family_link_rows", xref, 2, "PEDI", input$updated_pedigree)
      shiny::removeModal()
    }) %>% 
      shiny::bindEvent(input$update_relationship_now)
    
  })
}


get_pedi <- function(ged, fam_xref, chil_xref){
  # need to get section first as they might be a child of multiple families
  tidyged.internals::identify_section(ged, 1, "FAMC", fam_xref, chil_xref, TRUE) %>% 
    dplyr::slice(ged, .) %>% 
    tidyged.internals::gedcom_value(chil_xref, "PEDI", 2, "FAMC")
}
