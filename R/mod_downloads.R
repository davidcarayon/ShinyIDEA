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
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        
        file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
        
        # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
        knitting_dir <- file.path(tempdir(), "IDEATools_reports")
        if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
        file.copy(system.file("report/", package = "IDEATools"), knitting_dir, recursive = TRUE)
        
        diag_idea(input$files$datapath,output_directory = knitting_dir,prefix = file_name_short,
                  export_type = "report", type = "single", quiet = TRUE, report_format = "pdf")
        
        incProgress(0.8)
        
        file.copy(file.path(knitting_dir,Sys.Date(),file_name_short,paste0("Rapport_individuel_",file_name_short,".pdf")), file)
        
        
      })
      
    }
    
  )
  
  
  # Powerpoint --------------------------------------------------------------
  output$outputId6 <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Rapport_individuel_",file_name_short,".pptx")
    },
    
    content = function(file){
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        
        file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
        
        # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
        knitting_dir <- file.path(tempdir(), "IDEATools_reports")
        if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
        file.copy(system.file("report/", package = "IDEATools"), knitting_dir, recursive = TRUE)
        
        diag_idea(input$files$datapath,output_directory = knitting_dir,prefix = file_name_short,
                  export_type = "report", type = "single", quiet = TRUE, report_format = "pptx")
        
        incProgress(0.8)
        
        file.copy(file.path(knitting_dir,Sys.Date(),file_name_short,paste0("Rapport_individuel_",file_name_short,".pptx")), file)
        
        
      })
      
    }
    
  )
  
  # Word --------------------------------------------------------------
  output$outputId3 <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Rapport_individuel_",file_name_short,".docx")
    },
    
    content = function(file){
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        
        file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
        
        # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
        knitting_dir <- file.path(tempdir(), "IDEATools_reports")
        if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
        file.copy(system.file("report/", package = "IDEATools"), knitting_dir, recursive = TRUE)
        
        diag_idea(input$files$datapath,output_directory = knitting_dir,prefix = file_name_short,
                  export_type = "report", type = "single", quiet = TRUE, report_format = "docx")
        
        incProgress(0.8)
        
        file.copy(file.path(knitting_dir,Sys.Date(),file_name_short,paste0("Rapport_individuel_",file_name_short,".docx")), file)
        
        
      })
      
    }
    
  )
  
  # Libreoffice ODT --------------------------------------------------------------
  output$outputId4 <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Rapport_individuel_",file_name_short,".odt")
    },
    
    content = function(file){
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        
        file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
        
        # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
        knitting_dir <- file.path(tempdir(), "IDEATools_reports")
        if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
        file.copy(system.file("report/", package = "IDEATools"), knitting_dir, recursive = TRUE)
        
        diag_idea(input$files$datapath,output_directory = knitting_dir,prefix = file_name_short,
                  export_type = "report", type = "single", quiet = TRUE, report_format = "odt")
        
        incProgress(0.8)
        
        file.copy(file.path(knitting_dir,Sys.Date(),file_name_short,paste0("Rapport_individuel_",file_name_short,".odt")), file)
        
        
      })
      
    }
    
  )
  
  # ZIP --------------------------------------------------------------
  output$outputId5 <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Figures_",file_name_short,".zip")
    },
    
    content = function(file){
      
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        
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
        
      })
      
      
      
    }
    
  )
  
  # EXCEL --------------------------------------------------------------
  output$outputId2 <- downloadHandler(
    
    filename = function(){
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
      paste0("Rapport_individuel_",file_name_short,".xlsx")
    },
    
    content = function(file){
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        
        file_name_short <- substr(basename(tools::file_path_sans_ext(input$files$name)), start = 1, stop = 10)
        
        # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
        knitting_dir <- file.path(tempdir(), "IDEATools_reports")
        if (!dir.exists(knitting_dir)) (dir.create(knitting_dir))
        file.copy(system.file("report/", package = "IDEATools"), knitting_dir, recursive = TRUE)
        
        diag_idea(input$files$datapath,output_directory = knitting_dir,prefix = file_name_short,
                  export_type = "report", type = "single", quiet = TRUE, report_format = "xlsx")
        
        incProgress(0.8)
        
        file.copy(file.path(knitting_dir,Sys.Date(),file_name_short,paste0("Rapport_individuel_",file_name_short,".xlsx")), file)
        
        
      })
      
    }
    
  )
}

