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
      f <- jsonlite::fromJSON(system.file("example_json.json",package = "IDEATools"))
      jsonlite::write_json(f, file)
    }
    
  )
  
  
  # PDF ---------------------------------------------------------------------
  output$outputId <- downloadHandler(
    
    filename = function(){
      paste0("Rapport_individuel_",basename(tools::file_path_sans_ext(input$files$name)),".pdf")
    },
    
    content = function(file){
      
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        IDEAdata <- importIDEA(input$files$datapath)
        
        outdir <- tempdir()
        
        template <- system.file("report/rapport_individuel.Rmd", package = "IDEATools")
        file.copy(template,outdir,recursive = TRUE)
        
        style_folder <- system.file("report/bandeau.png", package = "IDEATools")
        file.copy(style_folder,outdir,recursive = TRUE)
        
        # Définition des paramètres pour le rendu
        params <- list(data =  IDEAdata,
                       outdir = outdir,
                       anon = FALSE)
        
        
        # Rendu du document dans un sous-environnement isolé
        rmarkdown::render(template,
                          params = params,
                          output_file = file,
                          envir = new.env(parent = globalenv()))
        
      })
      
      
      
    }
    
  )
  
  
  # Powerpoint --------------------------------------------------------------
  output$outputId6 <- downloadHandler(
    
    filename = function(){
      paste0("Présentation_individuelle_",basename(tools::file_path_sans_ext(input$files$name)),".pptx")
    },
    
    content = function(file){
      
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        IDEAdata <- importIDEA(input$files$datapath)
        
        outdir <- tempdir()
        
        template <- system.file("report/presentation_individuelle.Rmd", package = "IDEATools")
        file.copy(template,outdir, recursive = TRUE)
        
        style_folder <- system.file("report/bandeau.png", package = "IDEATools")
        file.copy(style_folder,outdir, recursive = TRUE)
        
        template_folder <- system.file("report/template.pptx", package = "IDEATools")
        file.copy(template_folder,outdir, recursive = TRUE)
        
        # Définition des paramètres pour le rendu
        params <- list(data =  IDEAdata,
                       outdir = outdir,
                       anon = FALSE)
        
        
        # Rendu du document dans un sous-environnement isolé
        rmarkdown::render(template,
                          params = params,
                          output_file = file,
                          envir = new.env(parent = globalenv()))
        
      })
      
      
      
    }
    
  )
  
  # Word --------------------------------------------------------------
  output$outputId3 <- downloadHandler(
    
    filename = function(){
      paste0("Rapport_individuel_",basename(tools::file_path_sans_ext(input$files$name)),".docx")
    },
    
    content = function(file){
      
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        IDEAdata <- importIDEA(input$files$datapath)
        
        outdir <- tempdir()
        
        template <- system.file("report/rapport_individuel_word.Rmd", package = "IDEATools")
        file.copy(template,outdir, recursive = TRUE)
        
        # Définition des paramètres pour le rendu
        params <- list(data =  IDEAdata,
                       outdir = outdir,
                       anon = FALSE)
        
        
        # Rendu du document dans un sous-environnement isolé
        rmarkdown::render(template,
                          params = params,
                          output_file = file,
                          envir = new.env(parent = globalenv()))
        
      })
      
      
      
    }
    
  )
  
  # Libreoffice ODT --------------------------------------------------------------
  output$outputId4 <- downloadHandler(
    
    filename = function(){
      paste0("Rapport_individuel_",basename(tools::file_path_sans_ext(input$files$name)),".odt")
    },
    
    content = function(file){
      
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        IDEAdata <- importIDEA(input$files$datapath)
        
        outdir <- tempdir()
        
        template <- system.file("report/rapport_individuel_open.Rmd", package = "IDEATools")
        file.copy(template,outdir, recursive = TRUE)
        
        # Définition des paramètres pour le rendu
        params <- list(data =  IDEAdata,
                       outdir = outdir,
                       anon = FALSE)
        
        
        # Rendu du document dans un sous-environnement isolé
        rmarkdown::render(template,
                          params = params,
                          output_file = file,
                          envir = new.env(parent = globalenv()))
        
      })
      
      
      
    }
    
  )
  
  # ZIP --------------------------------------------------------------
  output$outputId5 <- downloadHandler(
    
    filename = function(){
      paste0("Figures_",basename(tools::file_path_sans_ext(input$files$name)),".zip")
    },
    
    content = function(file){
      
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        
        outdir <- tempdir()
        
        IDEAdata <- importIDEA(input$files$datapath)
        
        v <- IDEAdata$metadata$MTD_01 %>%  stringr::str_replace_all(" ", "_")
        
        incProgress(0.3, detail = "Production des arbres éclairés...")
        
        MakeTrees(IDEAdata) %>% exportIDEA(outdir = outdir)
        
        incProgress(0.5, detail = "Production des graphiques dimensions...")
        
        dimensionsPlots(IDEAdata) %>% exportIDEA(outdir = outdir)
        
        incProgress(0.7, detail = "Production des graphiques radar...")
        
        radarPlots(IDEAdata) %>% exportIDEA(outdir = outdir)
        
        ## Définition du chemin des fichiers à archiver
        current_dir <- getwd()
        setwd(outdir)
        
        fs <- file.path(v,list.files(file.path(outdir,v), recursive=TRUE))
        
        # Export du zip
        zip(zipfile = file, files = fs)
        setwd(current_dir)
        
      })
      
      
      
    }
    
  )
  
  # EXCEL --------------------------------------------------------------
  output$outputId2 <- downloadHandler(
    
    filename = function(){
      paste0("Rapport_individuel_",basename(tools::file_path_sans_ext(input$files$name)),".xlsx")
    },
    
    content = function(file){
      
      
      withProgress(message = "Votre fichier est en cours de production...",detail = "Merci de patienter quelques instants",{
        
        outdir <- tempdir()
        current_dir <- getwd()
        
        setwd(outdir)
        
        IDEAdata <- importIDEA(input$files$datapath)
        
        v <- IDEAdata$metadata$MTD_01 %>%  stringr::str_replace_all(" ", "_")
        
        dim <- dimensionsPlots(IDEAdata)
        trees <- MakeTrees(IDEAdata)
        radars <- radarPlots(IDEAdata)
        polar <- PolarComponent(IDEAdata)
        v <- str_replace_all(names(dim)[1], " ", "_")
        
        # Creating workbook
        # If the input is a xlsx file, we can import the input file
        # Otherwise we have to create a new workbook object
        
        wb <- createWorkbook()
        
          
        ## Setting styles
        hs1 <- createStyle(fgFill = "#C0504D", halign = "CENTER", textDecoration = "Bold",
                           border = "TopBottomLeftRight", fontColour = "white")
        AEStyle <- createStyle(fontColour = "#000000", bgFill = "#2e9c15")
        STStyle <- createStyle(fontColour = "#000000", bgFill = "#5077FE")
        ECStyle <- createStyle(fontColour = "#000000", bgFill = "#FE962B")
        standard <- createStyle(fontColour = "#000000", halign = "CENTER",
                                borderStyle = "medium", border = "TopBottomLeftRight")
        
        FStyle <- createStyle(fontColour = "#000000", bgFill = "#1CDA53")
        TFStyle <- createStyle(fontColour = "#000000", bgFill = "#0D8A00")
        IStyle <- createStyle(fontColour = "#000000", bgFill = "#FFA300")
        DStyle <- createStyle(fontColour = "#000000", bgFill = "#FF6348")
        TDStyle <- createStyle(fontColour = "#000000", bgFill = "#FF0000")
        NCStyle <- createStyle(fontColour = "#000000", bgFill = "#cecece")
        
        
      
        incProgress(0.3, detail = "Ecriture des figures IDEA dans un répertoire temporaire.")
        
        ## Graph production
        exportIDEA(dim,"tmp")
        exportIDEA(trees,"tmp")
        exportIDEA(radars,"tmp")
        exportIDEA(polar,"tmp")
        
        incProgress(0.3, detail = "Création et remplissage des nouveaux onglets...")
        
        ## Beginning the openxlsx sequence
        
        # Dimensions --------------------------------------------------------------
        addWorksheet(wb, "Dimensions",gridLines = FALSE, tabColour = "#cecece")
        addWorksheet(wb, "Composantes",gridLines = FALSE, tabColour = "#cecece")
        addWorksheet(wb, "Indicateurs",gridLines = FALSE, tabColour = "#cecece")
        
        
        df <- IDEAdata$dataset %>% select(Dimension = dimension,Score = dimension_value) %>% distinct()
        
        writeData(wb, "Dimensions", df,
                  colNames = TRUE, rowNames = TRUE, startCol = "A",
                  startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
        )
        
        
        setColWidths(wb, "Dimensions", cols = 4, widths = 2)
        setColWidths(wb, "Dimensions", cols = 1:3, widths = 26)
        
        img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Dimensions.png"))
        insertImage(wb,"Dimensions",file = img, startRow = 2, startCol = "E",width = 16.61, height = 10.21, units = "cm")
        
        
        ## Composantes
        
        df <- IDEAdata$dataset %>%
          inner_join(list_max_compo, by = "composante") %>% select(Dimension = dimension,Composante = composante,Score = composante_value, `Max possible` = max_compo) %>% distinct()
        
        writeData(wb, "Composantes", df,
                  colNames = TRUE, rowNames = TRUE, startCol = "A",
                  startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
        )
        
        conditionalFormatting(wb, "Composantes", cols = 1:5, rows = 1:300, type = "contains", rule = "Agroécologique",style = AEStyle)
        conditionalFormatting(wb, "Composantes", cols = 1:5, rows = 1:300, type = "contains", rule = "Socio-",style = STStyle)
        conditionalFormatting(wb, "Composantes", cols = 1:5, rows = 1:300, type = "contains", rule = "Economique",style = ECStyle)
        
        setColWidths(wb, "Composantes", cols = c(1,2,4,5), widths = "auto")
        setColWidths(wb, "Composantes", cols = 3, widths = 60)
        setColWidths(wb, "Composantes", cols = 6, widths = 2)
        
        img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Composantes.png"))
        
        insertImage(wb,"Composantes", file = img, startRow = 2, startCol = "G", width = 16.32, height = 12.52, units = "cm")
        
        img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Composantes_polaires.png"))
        
        insertImage(wb,"Composantes", file = img, startRow = 17, startCol = "B", width = 21.59, height = 21.59, units = "cm")
        
        ## Indicateurs
        
        df <- IDEAdata$dataset %>%
          inner_join(list_max, by = "indicateur") %>% select(Dimension = dimension,Composante = composante,indicateur,nom_indicateur,Score = value, `Max possible` = valeur_max) %>% distinct() %>% unite(Indicateur,c("indicateur","nom_indicateur"), sep = " - ")
        
        writeData(wb, "Indicateurs", df,
                  colNames = TRUE, rowNames = TRUE, startCol = "A",
                  startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
        )
        
        conditionalFormatting(wb, "Indicateurs", cols = 1:2, rows = 1:300, type = "contains", rule = "Agroécologique",style = AEStyle)
        conditionalFormatting(wb, "Indicateurs", cols = 1:2, rows = 1:300, type = "contains", rule = "Socio-",style = STStyle)
        conditionalFormatting(wb, "Indicateurs", cols = 1:2, rows = 1:300, type = "contains", rule = "Economique",style = ECStyle)
        
        
        setColWidths(wb, "Indicateurs", cols = 7, widths = 2)
        setColWidths(wb, "Indicateurs", cols = 4, widths = 75)
        
        # addStyle(wb, sheet = "Indicateurs", standard, rows = 3:56,
        #          cols = 1:6, gridExpand = TRUE)
        
        
        img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Indicateurs Agroécologiques.png"))
        insertImage(wb,"Indicateurs", file = img, startRow = 2, startCol = "H", width = 14.31, height = 16.07, units = "cm")
        
        img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Indicateurs Socio-Territoriaux.png"))
        insertImage(wb,"Indicateurs", file = img, startRow = 39, startCol = "H", width = 14.31, height = 18.08, units = "cm")
        
        img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Indicateurs Economiques.png"))
        insertImage(wb,"Indicateurs", file = img, startRow = 57, startCol = "D", width = 14.31, height = 12.09, units = "cm")
        
        # Propriétés --------------------------------------------------------------
        
        addWorksheet(wb, "Synthèse Propriétés", gridLines = FALSE, tabColour = "yellow")
        
        
        props <- label_nodes %>% filter(level == "propriete") %>% pull(nom_indicateur)
        
        df <- IDEAdata$nodes$Global %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
          full_join(IDEAdata$dataset, by= "indicateur") %>%
          rowwise() %>%
          filter(indicateur %in% props) %>%
          mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
          ungroup() %>%
          select(Propriété=nom_indicateur, Résultat)
        
        FStyle <- createStyle(fontColour = "#000000", bgFill = "#1CDA53")
        TFStyle <- createStyle(fontColour = "#000000", bgFill = "#0D8A00")
        IStyle <- createStyle(fontColour = "#000000", bgFill = "#FFA300")
        DStyle <- createStyle(fontColour = "#000000", bgFill = "#FF6348")
        TDStyle <- createStyle(fontColour = "#000000", bgFill = "#FF0000")
        NCStyle <- createStyle(fontColour = "#000000", bgFill = "#cecece")
        
        to_col <- df$Résultat
        names(to_col) <- df$Propriété
        
        
        colorise = function(res){
          case_when(res == "favorable" ~ "#1CDA53",
                    res == "défavorable" ~ "#FF6348",
                    res == "très favorable" ~ "#0D8A00",
                    res == "très défavorable" ~ "#FF0000")
        }
        
        
        ## La couleur pourrait ici être conditionnelle
        addWorksheet(wb, "Robustesse", gridLines = FALSE, tabColour = colorise(to_col["Robustesse"]))
        addWorksheet(wb, "Ancrage Territorial", gridLines = FALSE, tabColour = colorise(to_col["Ancrage territorial"]))
        addWorksheet(wb, "Capacité", gridLines = FALSE, tabColour = colorise(to_col["Capacité productive et reproductive de biens et de services"]))
        addWorksheet(wb, "Autonomie", gridLines = FALSE, tabColour = colorise(to_col["Autonomie"]))
        addWorksheet(wb, "Responsabilité globale", gridLines = FALSE, tabColour = colorise(to_col["Responsabilité globale"]))
        
        
        ## Synthèse globale
        
        props <- label_nodes %>% filter(level == "propriete") %>% pull(nom_indicateur)
        
        df <- IDEAdata$nodes$Global %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
          full_join(IDEAdata$dataset, by= "indicateur") %>%
          rowwise() %>%
          filter(indicateur %in% props) %>%
          mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
          ungroup() %>%
          select(Propriété=nom_indicateur, Résultat)
        
        writeData(wb, "Synthèse Propriétés", df,
                  colNames = TRUE, rowNames = TRUE, startCol = "A",
                  startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
        )
        
        conditionalFormatting(wb, "Synthèse Propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
        conditionalFormatting(wb, "Synthèse Propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
        conditionalFormatting(wb, "Synthèse Propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
        conditionalFormatting(wb, "Synthèse Propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
        conditionalFormatting(wb, "Synthèse Propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
        conditionalFormatting(wb, "Synthèse Propriétés", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)
        
        setColWidths(wb, "Synthèse Propriétés", cols = 4, widths = 2)
        setColWidths(wb, "Synthèse Propriétés", cols = 1:3, widths = "auto")
        
        
        end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "zoom.png")
        img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)
        
        insertImage(wb,"Synthèse Propriétés", file = img, startRow = 2, startCol = "E", width = 22.67, height = 15.26, units = "cm")
        
        end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "Global.png")
        img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)
        insertImage(wb,"Synthèse Propriétés", file = img, startRow = 35, startCol = "E", width = 22.92, height = 15.85, units = "cm")
        
        ## Robustesse
        
        df <- IDEAdata$nodes$Robustesse %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
          left_join(label_nodes %>% select(code_indicateur, level), by = c("indicateur"="code_indicateur")) %>%
          full_join(IDEAdata$dataset %>% filter(indicateur %in% indicateurs_proprietes$indicateurs_robustesse), by= "indicateur") %>%
          rowwise() %>%
          mutate(level = ifelse(is.na(level), yes = "Noeud", no = "Indicateur")) %>%
          mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
          ungroup() %>%
          arrange(level) %>%
          select(Indicateur=nom_indicateur,Niveau = level, `Score déplafonné` = unscaled_value, Résultat)
        
        writeData(wb, "Robustesse", df,
                  colNames = TRUE, rowNames = TRUE, startCol = "A",
                  startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
        )
        
        conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
        conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
        conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
        conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
        conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
        conditionalFormatting(wb, "Robustesse", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)
        
        setColWidths(wb, "Robustesse", cols = 6, widths = 2)
        setColWidths(wb, "Robustesse", cols = 1:5, widths = "auto")
        
        
        end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "Robustesse.png")
        img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)
        insertImage(wb,"Robustesse", file = img, startRow = 2, startCol = "G", width = 18.86, height = 13.49, units = "cm")
        
        
        ## Ancrage
        
        df <- IDEAdata$nodes$Ancrage %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
          left_join(label_nodes %>% select(code_indicateur, level), by = c("indicateur"="code_indicateur")) %>%
          full_join(IDEAdata$dataset %>% filter(indicateur %in% indicateurs_proprietes$indicateurs_ancrage), by= "indicateur") %>%
          rowwise() %>%
          mutate(level = ifelse(is.na(level), yes = "Noeud", no = "Indicateur")) %>%
          mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
          ungroup() %>%
          arrange(level) %>%
          select(Indicateur=nom_indicateur, Niveau = level,`Score déplafonné` = unscaled_value, Résultat)
        
        writeData(wb, "Ancrage Territorial", df,
                  colNames = TRUE, rowNames = TRUE, startCol = "A",
                  startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
        )
        
        conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
        conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
        conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
        conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
        conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
        conditionalFormatting(wb, "Ancrage Territorial", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)
        
        setColWidths(wb, "Ancrage Territorial", cols = 6, widths = 2)
        setColWidths(wb, "Ancrage Territorial", cols = 1:5, widths = "auto")
        
        
        end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "Territorial.png")
        
        img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)
        
        insertImage(wb,"Ancrage Territorial", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")
        
        
        ## Capacité productive
        
        df <- IDEAdata$nodes$Capacité %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
          left_join(label_nodes %>% select(code_indicateur, level), by = c("indicateur"="code_indicateur")) %>%
          full_join(IDEAdata$dataset %>% filter(indicateur %in% indicateurs_proprietes$indicateurs_capacite), by= "indicateur") %>%
          rowwise() %>%
          mutate(level = ifelse(is.na(level), yes = "Noeud", no = "Indicateur")) %>%
          mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
          ungroup() %>%
          arrange(level) %>%
          select(Indicateur=nom_indicateur, Niveau = level,`Score déplafonné` = unscaled_value, Résultat)
        
        writeData(wb, "Capacité", df,
                  colNames = TRUE, rowNames = TRUE, startCol = "A",
                  startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
        )
        
        conditionalFormatting(wb, "Capacité", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
        conditionalFormatting(wb, "Capacité", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
        conditionalFormatting(wb, "Capacité", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
        conditionalFormatting(wb, "Capacité", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
        conditionalFormatting(wb, "Capacité", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
        conditionalFormatting(wb, "Capacité", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)
        
        setColWidths(wb, "Capacité", cols = 6, widths = 2)
        setColWidths(wb, "Capacité", cols = 1:5, widths = "auto")
        
        
        end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "services.png")
        
        img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)
        
        insertImage(wb,"Capacité", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")
        
        ## Autonomie
        
        df <- IDEAdata$nodes$Autonomie %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
          left_join(label_nodes %>% select(code_indicateur, level), by = c("indicateur"="code_indicateur")) %>%
          full_join(IDEAdata$dataset %>% filter(indicateur %in% indicateurs_proprietes$indicateurs_autonomie), by= "indicateur") %>%
          rowwise() %>%
          mutate(level = ifelse(is.na(level), yes = "Noeud", no = "Indicateur")) %>%
          mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
          ungroup() %>%
          arrange(level) %>%
          select(Indicateur=nom_indicateur, Niveau = level,`Score déplafonné` = unscaled_value, Résultat)
        
        writeData(wb, "Autonomie", df,
                  colNames = TRUE, rowNames = TRUE, startCol = "A",
                  startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
        )
        
        conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
        conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
        conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
        conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
        conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
        conditionalFormatting(wb, "Autonomie", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)
        
        setColWidths(wb, "Autonomie", cols = 6, widths = 2)
        setColWidths(wb, "Autonomie", cols = 1:5, widths = "auto")
        
        
        end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "Autonomie.png")
        
        img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)
        
        insertImage(wb,"Autonomie", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")
        
        
        ## Responsabilité globale
        
        df <- IDEAdata$nodes$Responsabilité %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
          left_join(label_nodes %>% select(code_indicateur, level), by = c("indicateur"="code_indicateur")) %>%
          full_join(IDEAdata$dataset %>% filter(indicateur %in% indicateurs_proprietes$indicateurs_responsabilite), by= "indicateur") %>%
          rowwise() %>%
          mutate(level = ifelse(is.na(level), yes = "Noeud", no = "Indicateur")) %>%
          mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
          ungroup() %>%
          arrange(level) %>%
          select(Indicateur=nom_indicateur, Niveau = level,`Score déplafonné` = unscaled_value, Résultat)
        
        writeData(wb, "Responsabilité globale", df,
                  colNames = TRUE, rowNames = TRUE, startCol = "A",
                  startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
        )
        
        conditionalFormatting(wb, "Responsabilité globale", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
        conditionalFormatting(wb, "Responsabilité globale", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
        conditionalFormatting(wb, "Responsabilité globale", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
        conditionalFormatting(wb, "Responsabilité globale", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
        conditionalFormatting(wb, "Responsabilité globale", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
        conditionalFormatting(wb, "Responsabilité globale", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)
        
        setColWidths(wb, "Responsabilité globale", cols = 6, widths = 2)
        setColWidths(wb, "Responsabilité globale", cols = 1:5, widths = "auto")
        
        
        end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "globale.png")
        
        img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)
        
        insertImage(wb,"Responsabilité globale", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")
        
        addWorksheet(wb, "Annexe", gridLines = FALSE)
        
        end <- list.files(file.path("tmp",v,"Propriétés"), pattern = "Robustesse.png")
        img <- file.path("tmp",v,"Propriétés",end)
        insertImage(wb,"Annexe", file = img, startRow = 2, startCol = "B", width = 23.42, height = 11.07, units = "cm")
        
        end <- list.files(file.path("tmp",v,"Propriétés"), pattern = "Territorial.png")
        img <- file.path("tmp",v,"Propriétés",end)
        insertImage(wb,"Annexe", file = img, startRow = 2, startCol = "M", width = 23.42, height = 11.07, units = "cm")
        
        end <- list.files(file.path("tmp",v,"Propriétés"), pattern = "services.png")
        img <- file.path("tmp",v,"Propriétés",end)
        insertImage(wb,"Annexe", file = img, startRow = 26, startCol = "B", width = 23.42, height = 11.07, units = "cm")
        
        end <- list.files(file.path("tmp",v,"Propriétés"), pattern = "Autonomie.png")
        img <- file.path("tmp",v,"Propriétés",end)
        insertImage(wb,"Annexe", file = img, startRow = 26, startCol = "M", width = 23.42, height = 11.07, units = "cm")
        
        end <- list.files(file.path("tmp",v,"Propriétés"), pattern = "globale.png")
        img <- file.path("tmp",v,"Propriétés",end)
        insertImage(wb,"Annexe", file = img, startRow = 49, startCol = "B", width = 23.42, height = 11.07, units = "cm")
        
        
        incProgress(0.3, detail = "Données correctement transférées")
        
        incProgress(0.3, detail = "Ecriture du fichier excel")
      

        
        saveWorkbook(wb, file,overwrite = TRUE)
        
        setwd(current_dir)
        
        
      })
      
      
      
    }
    
  )
}
