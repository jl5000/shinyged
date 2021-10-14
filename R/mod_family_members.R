


family_members_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$br(),
    
    DT::DTOutput(ns("table")),

    shiny::br(),
    
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(ns("new_member"), label = "Member", choices = NULL, 
                                          multiple = TRUE, width = "500px", options = list(maxItems = 1)),
                    
                    ),
      shiny::column(6,
                    shiny::selectInput(ns("pedigree"), "Relationship (if child)", 
                                       choices = tidyged.internals::val_pedigree_linkage_types()),
                    )
    ),
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(ns("add_spouse"), "Add as spouse"),
                    shiny::actionButton(ns("add_child"), "Add as child"),
                    shiny::actionButton(ns("delete_member"), "Delete member"),
                    shiny::actionButton(ns("update_member"), "Update member")
                    )
    ) %>% shinyjs::disabled(),
    
    shiny::fluidRow(
      shiny::column(12,
                    notes_ui(ns("member_notes"))
      )
    ) %>% shinyjs::hidden()
    
   
  )
  
}


family_members_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    #notes_server("member_notes", r, "family_link_rows")
    
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
         
      tibble::tibble(memb_names, type, rel)
      
    })
    
    output$table <- DT::renderDataTable({
      req(members_table)
      
      DT::datatable(members_table(), rownames = FALSE, selection = "single",
                    filter = "none", colnames = c("Name", "Type", "Relationship"),
                    options = list(searching = FALSE, paging = FALSE))
    })
    
    
    shiny::observe({
      req(r$ged)
      
      indis <- tidyged::xrefs_indi(r$ged) %>% 
        tidyged::describe_records(r$ged, ., short_desc = TRUE)
      
      shiny::updateSelectizeInput(session = session, inputId = "new_member", choices = indis)
    }) %>% 
      shiny::bindEvent(r$ged)
    
    shiny::observe({
      shinyjs::toggleState("delete", !is.null(input$new_member))
    }) %>% 
      shiny::bindEvent(input$new_member, ignoreNULL = FALSE)
    
    shiny::observe({
      
    }) %>% 
      shiny::bindEvent(input$table_row_selected, ignoreNULL = FALSE)
    
  })
}



