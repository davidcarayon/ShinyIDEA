#' Separate modules for downloads
#'
#' @description A shiny Module.
#'
#' @noRd
#'
#' @import jsonlite
#' @import openxlsx
dlmodule_group <- function(input, output, session) {
  
  
  # PDF ---------------------------------------------------------------------
  output$dl_group_pdf <- downloadHandler(
    filename = function() {
      paste0("Rapport_groupe_", length(input$dir$datapath), ".pdf")
    },
    
    content = function(file) {
      progressSweetAlert(
        session = session, id = "myprogress_group_pdf",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working directory
      knitting_dir <- file.path(tempdir(), "IDEATools_reports")
      if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
      
      prefix <- paste0("Groupe_", length(input$dir$datapath))
      
      diag_idea(dirname(input$dir$datapath[[1]]),
                output_directory = knitting_dir,
                export_type = "report", type = "group", quiet = TRUE, report_format = "pdf"
      )
      
      
      file.copy(file.path(knitting_dir, Sys.Date(), prefix, paste0("Rapport_groupe_", length(input$dir$datapath), ".pdf")), file)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = " Fichier téléchargé !",
        type = "success"
      )
    }
  )
  
  
  # Powerpoint --------------------------------------------------------------
  output$dl_group_pptx <- downloadHandler(
    filename = function() {
      paste0("Rapport_groupe_", length(input$dir$datapath), ".pptx")
    },
    
    content = function(file) {
      progressSweetAlert(
        session = session, id = "myprogress_group_pptx",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working directory
      knitting_dir <- file.path(tempdir(), "IDEATools_reports")
      if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
      
      prefix <- paste0("Groupe_", length(input$dir$datapath))
      
      diag_idea(dirname(input$dir$datapath[[1]]),
                output_directory = knitting_dir,
                export_type = "report", type = "group", quiet = TRUE, report_format = "pptx"
      )
      
      
      file.copy(file.path(knitting_dir, Sys.Date(), prefix, paste0("Rapport_groupe_", length(input$dir$datapath), ".pptx")), file)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = " Fichier téléchargé !",
        type = "success"
      )
    }
  )
  
  # Word --------------------------------------------------------------
  output$dl_group_docx <- downloadHandler(
    filename = function() {
      paste0("Rapport_groupe_", length(input$dir$datapath), ".docx")
    },
    
    content = function(file) {
      progressSweetAlert(
        session = session, id = "myprogress_group_docx",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working directory
      knitting_dir <- file.path(tempdir(), "IDEATools_reports")
      if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
      
      prefix <- paste0("Groupe_", length(input$dir$datapath))
      
      diag_idea(dirname(input$dir$datapath[[1]]),
                output_directory = knitting_dir,
                export_type = "report", type = "group", quiet = TRUE, report_format = "docx"
      )
      
      
      file.copy(file.path(knitting_dir, Sys.Date(), prefix, paste0("Rapport_groupe_", length(input$dir$datapath), ".docx")), file)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = " Fichier téléchargé !",
        type = "success"
      )
    }
  )
  
  # Libreoffice ODT --------------------------------------------------------------
  output$dl_group_odt <- downloadHandler(
    filename = function() {
      paste0("Rapport_groupe_", length(input$dir$datapath), ".odt")
    },
    
    content = function(file) {
      progressSweetAlert(
        session = session, id = "myprogress_group_odt",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working directory
      knitting_dir <- file.path(tempdir(), "IDEATools_reports")
      if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
      
      prefix <- paste0("Groupe_", length(input$dir$datapath))
      
      diag_idea(dirname(input$dir$datapath[[1]]),
                output_directory = knitting_dir,
                export_type = "report", type = "group", quiet = TRUE, report_format = "odt"
      )
      
      
      file.copy(file.path(knitting_dir, Sys.Date(), prefix, paste0("Rapport_groupe_", length(input$dir$datapath), ".odt")), file)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = " Fichier téléchargé !",
        type = "success"
      )
    }
  )
  
  # ZIP --------------------------------------------------------------
  output$dl_group_zip <- downloadHandler(
    filename = function() {
      paste0("Figures_Groupe_", length(input$dir$datapath), ".zip")
    },
    
    content = function(file) {
      progressSweetAlert(
        session = session, id = "myprogress_group_zip",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      outdir <- file.path(tempdir(), "Figures")
      
      diag_idea(dirname(input$dir$datapath[[1]]),
                output_directory = outdir,
                export_type = "local", type = "group", quiet = TRUE
      )
      
      ## Définition du chemin des fichiers à archiver
      prefix <- paste0("Groupe_", length(input$dir$datapath))
      
      current_dir <- getwd()
      setwd(file.path(outdir, Sys.Date()))
      
      fs <- list.files(prefix, recursive = TRUE, full.names = TRUE)
      
      # Export du zip
      zip(zipfile = file, files = fs)
      setwd(current_dir)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = " Fichier téléchargé !",
        type = "success"
      )
    }
  )
  
  # EXCEL --------------------------------------------------------------
  output$dl_group_xlsx <- downloadHandler(
    filename = function() {
      paste0("Rapport_groupe_", length(input$dir$datapath), ".xlsx")
    },
    
    content = function(file) {
      progressSweetAlert(
        session = session, id = "myprogress_group_xlsx",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working directory
      knitting_dir <- file.path(tempdir(), "IDEATools_reports")
      if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
      
      prefix <- paste0("Groupe_", length(input$dir$datapath))
      
      diag_idea(dirname(input$dir$datapath[[1]]),
                output_directory = knitting_dir,
                export_type = "report", type = "group", quiet = TRUE, report_format = "xlsx"
      )
      
      
      file.copy(file.path(knitting_dir, Sys.Date(), prefix, paste0("Rapport_groupe_", length(input$dir$datapath), ".xlsx")), file)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = " Fichier téléchargé !",
        type = "success"
      )
    }
  )
}
