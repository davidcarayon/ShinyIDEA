#' Separate modules for downloads
#'
#' @description A shiny Module.
#'
#' @noRd 
#'
#' @import jsonlite
#' @import openxlsx
dlmodule <- function(input, output, session) {
  output$example_data <- downloadHandler(
    
    filename = function(){
      paste0("donnes_fictives_IDEA.json")
    },
    
    content = function(file){
      f <- jsonlite::fromJSON(system.file("idea_example.json",package = "IDEATools"))
      jsonlite::write_json(f, file)
    }
    
  )
  
  
  # PDF ---------------------------------------------------------------------
  output$outputId <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Rapport_individuel_",file_name_short,".pdf")
    },
    
    content = function(file){
      
      progressSweetAlert(
        session = session, id = "myprogress",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      
      # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
      knitting_dir <- file.path(tempdir(), "IDEATools_reports")
      if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
      
      diag_idea(input$files$datapath,output_directory = knitting_dir,prefix = file_name_short,
                export_type = "report", type = "single", quiet = TRUE, report_format = "pdf")
      
      
      file.copy(file.path(knitting_dir,Sys.Date(),file_name_short,paste0("Rapport_individuel_",file_name_short,".pdf")), file)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Fichier téléchargé !",
        type = "success"
      )
    }
    
  )
  
  
  # Powerpoint --------------------------------------------------------------
  output$outputId6 <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Rapport_individuel_",file_name_short,".pptx")
    },
    
    content = function(file){
      
      progressSweetAlert(
        session = session, id = "myprogress_6",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      
      # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
      knitting_dir <- file.path(tempdir(), "IDEATools_reports")
      if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
      
      diag_idea(input$files$datapath,output_directory = knitting_dir,prefix = file_name_short,
                export_type = "report", type = "single", quiet = TRUE, report_format = "pptx")
      
      incProgress(0.8)
      
      file.copy(file.path(knitting_dir,Sys.Date(),file_name_short,paste0("Rapport_individuel_",file_name_short,".pptx")), file)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Fichier téléchargé !",
        type = "success"
      )
      
    }
    
  )
  
  # Word --------------------------------------------------------------
  output$outputId3 <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Rapport_individuel_",file_name_short,".docx")
    },
    
    content = function(file){
      
      progressSweetAlert(
        session = session, id = "myprogress_3",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      
      # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
      knitting_dir <- file.path(tempdir(), "IDEATools_reports")
      if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
      
      diag_idea(input$files$datapath,output_directory = knitting_dir,prefix = file_name_short,
                export_type = "report", type = "single", quiet = TRUE, report_format = "docx")
      
      
      file.copy(file.path(knitting_dir,Sys.Date(),file_name_short,paste0("Rapport_individuel_",file_name_short,".docx")), file)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Fichier téléchargé !",
        type = "success"
      )
      
      
    }
    
  )
  
  # Libreoffice ODT --------------------------------------------------------------
  output$outputId4 <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Rapport_individuel_",file_name_short,".odt")
    },
    
    content = function(file){
      
      progressSweetAlert(
        session = session, id = "myprogress_4",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      
      # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
      knitting_dir <- file.path(tempdir(), "IDEATools_reports")
      if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
      
      diag_idea(input$files$datapath,output_directory = knitting_dir,prefix = file_name_short,
                export_type = "report", type = "single", quiet = TRUE, report_format = "odt")
      
      
      file.copy(file.path(knitting_dir,Sys.Date(),file_name_short,paste0("Rapport_individuel_",file_name_short,".odt")), file)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Fichier téléchargé !",
        type = "success"
      )
      
    }
    
  )
  
  # ZIP --------------------------------------------------------------
  output$outputId5 <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Figures_",file_name_short,".zip")
    },
    
    content = function(file){
      
      
      progressSweetAlert(
        session = session, id = "myprogress_5",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      outdir <- file.path(tempdir(),"Figures")
      
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      
      diag_idea(input$files$datapath, output_directory = outdir,type = "single", export_type = "local", quiet = TRUE, prefix = file_name_short)
      
      ## Définition du chemin des fichiers à archiver
      current_dir <- getwd()
      setwd(file.path(outdir,Sys.Date()))
      
      fs <- list.files(file_name_short, recursive=TRUE, full.names = TRUE)
      
      # Export du zip
      zip(zipfile = file, files = fs)
      setwd(current_dir)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Fichier téléchargé !",
        type = "success"
      )
      
      
    }
    
  )
  
  # EXCEL --------------------------------------------------------------
  output$outputId2 <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Rapport_individuel_",file_name_short,".xlsx")
    },
    
    content = function(file){
      
      progressSweetAlert(
        session = session, id = "myprogress_6",
        title = "Production du fichier en cours...",
        display_pct = FALSE, value = 100
      )
      
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      
      # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
      knitting_dir <- file.path(tempdir(), "IDEATools_reports")
      if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
      
      diag_idea(input$files$datapath,output_directory = knitting_dir,prefix = file_name_short,
                export_type = "report", type = "single", quiet = TRUE, report_format = "xlsx")
      
      
      file.copy(file.path(knitting_dir,Sys.Date(),file_name_short,paste0("Rapport_individuel_",file_name_short,".xlsx")), file)
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Fichier téléchargé !",
        type = "success"
      )
      
    }
    
  )
}

