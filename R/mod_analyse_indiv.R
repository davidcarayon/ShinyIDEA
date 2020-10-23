#' analyse_indiv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinydashboard infoBox infoBoxOutput renderInfoBox
#' @import IDEATools
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import fresh
#' @import ggplot2
#' @import shinyWidgets
#' @import ggiraph
#' @import readr

library(IDEATools)

# On augmente la taille max autorisée pour les imports
options(shiny.maxRequestSize = 30 * 1024^2)

mod_analyse_indiv_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
    
    
    fluidRow(
      col_12(
        bs4Card(
          inputId = ns("doc_card"),
          title = "A propos de ce module",
          status = "info",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          includeMarkdown(app_sys("app", "docs", "explore.md")),
          fileInput(ns("files"), "Charger votre calculateur :", accept = c(".xls",".xlsx",".json"), width = "100%", multiple = FALSE, buttonLabel = "Charger...",placeholder = "Aucun fichier chargé")
          
        )
      )
    ),
    br(),
    uiOutput(ns("result_boxes")),
    br(),
    col_12(uiOutput(ns("download_box"), width = 12))

  )
}

#' analyse_indiv Server Function
#'
#' @noRd 
mod_analyse_indiv_server <- function(input, output, session){
  
  ns <- session$ns
  
  # Permet d'éviter une erreur liée au fichier "Rplots.pdf"
  pdf(NULL)
  
  # Définition du dossier de travail temporaire où créer puis piocher les fichiers
  outdir <- tempdir()
  
  IDEAdata <- eventReactive(input$files,{
    importIDEA(input = input$files$datapath, anonymous = FALSE)
  })
  
  observeEvent(input$files, {
    
    output$note_ae <- renderbs4InfoBox({
      req(IDEAdata())
      value <- unique(subset(IDEAdata()$dataset, dimension == "Agroécologique")$dimension_value)
      bs4InfoBox(paste0(value,"/100"), title = "Durabilité Agroécologique", icon = "leaf", status = "success", width = 12)
    })
    output$note_st <- renderbs4InfoBox({
      req(IDEAdata())
      value <- unique(subset(IDEAdata()$dataset, dimension == "Socio-Territoriale")$dimension_value)
      bs4InfoBox(paste0(value,"/100"), title = "Durabilité Socio-Territoriale", icon = "handshake", status = "primary", width = 12)
    })
    output$note_ec <- renderbs4InfoBox({
      req(IDEAdata())
      value <- unique(subset(IDEAdata()$dataset, dimension == "Economique")$dimension_value)
      bs4InfoBox(paste0(value,"/100"), title = "Durabilité Economique", icon = "euro-sign", status = "warning", width = 12)
    })
    output$IDEAtext <- renderText({
      req(IDEAdata())
      value <- min(IDEAdata()$dataset$dimension_value)
      newdim <- IDEAdata()$dataset %>% arrange(dimension_value) %>% slice(1) %>% pull(dimension)
      
      HTML(paste0("La méthode IDEA retient la note la plus faible des 3 dimensions en tant que valeur finale, puisque les dimensions ne peuvent se compenser entre elles (principe de la durabilité forte).<br> Ainsi, votre exploitation obtient la note de <b>",value,"/100 </b> avec la méthode IDEA, correspondant à la dimension <b>",newdim,"</b>."))
      
    })
    output$proptext <- renderText({
      req(IDEAdata())
      
      to_increase <- IDEAdata()$nodes$Global %>% tidyr::gather(key = nom_indicateur, value = valeur, -id_exploit) %>% inner_join(IDEATools::label_nodes, by = "nom_indicateur") %>% filter(level == "propriete") %>% filter(valeur %in% c("défavorable","très défavorable")) %>% 
        pull(nom_indicateur) %>% paste(collapse = " / ")
      
      
      if(nchar(to_increase) == 0){to_increase = "Aucune"}
      
      
      HTML(paste0("La méthode IDEA évalue également les exploitations agricoles, depuis sa version 4, selon les propriétés de la durabilité.<br>
                  Selon cette approche, votre exploitation devrait axer ses efforts sur la ou les propriété(s) : <b>",to_increase,"</b>"))
      
    })
    output$prop1 <- renderbs4InfoBox({
      req(IDEAdata())
      val <- IDEAdata()$nodes$Robustesse$Robustesse
      
      color <- replace_col(val)
      
      val <- str_to_title(val)
      
      ico <- ifelse(val %in% c("Favorable", "Très Favorable"), yes = "smile", no = "frown")
      
      bs4InfoBox(value = paste(val),
                 title = "Robustesse",
                 icon = ico,
                 status = color, href = "#")
    })
    output$prop2 <- renderbs4InfoBox({
      req(IDEAdata())
      val <- IDEAdata()$nodes$Ancrage$`Ancrage territorial`
      
      color <- replace_col(val)
      
      val <- str_to_title(val)
      
      ico <- ifelse(val %in% c("Favorable", "Très Favorable"), yes = "smile", no = "frown")
      
      bs4InfoBox(value = paste(val),
                 title = "Ancrage territorial",
                 icon = ico,
                 status = color, href = "#")
    })
    output$prop3 <- renderbs4InfoBox({
      req(IDEAdata())
      val <- IDEAdata()$nodes$Autonomie$Autonomie
      
      color <- replace_col(val)
      
      val <- str_to_title(val)
      
      ico <- ifelse(val %in% c("Favorable", "Très Favorable"), yes = "smile", no = "frown")
      
      bs4InfoBox(value = paste(val),
                 title = "Autonomie",
                 icon = ico,
                 status = color, href = "#")
    })
    output$prop4 <- renderbs4InfoBox({
      req(IDEAdata())
      val <- IDEAdata()$nodes$`Responsabilité`$`Responsabilité globale`
      
      color <- replace_col(val)
      
      val <- str_to_title(val)
      
      ico <- ifelse(val %in% c("Favorable", "Très Favorable"), yes = "smile", no = "frown")
      
      bs4InfoBox(value = paste(val),
                 title = "Responsabilité globale",
                 icon = ico,
                 status = color, href = "#")
    })
    output$prop5 <- renderbs4InfoBox({
      req(IDEAdata())
      val <- IDEAdata()$nodes$`Capacité`$`Capacité productive et reproductive de biens et de services`
      
      color <- replace_col(val)
      
      val <- str_to_title(val)
      
      ico <- ifelse(val %in% c("Favorable", "Très Favorable"), yes = "smile", no = "frown")
      
      bs4InfoBox(value = paste(val),
                 title = "Capacité productive et reproductive de biens et de services",
                 icon = ico,
                 status = color, href = "#", width = 12)
    })
  
    output$download_box <- renderUI({
    
      
      
      
      
      bs4Card(inputID = "download_card",title = "Télécharger le diagnostic complet",width = 12,
              solidHeader = TRUE, status = "info",closable = FALSE,
              div(
                style="display:inline-block;width:100%;text-align: center;",
                CustomDownloadButton(
                  "outputId",
                  label = "Télécharger au format PDF",
                  style = "simple",
                  color = "danger",
                  size = "sm",
                  block = FALSE,
                  no_outline = TRUE,
                  icon = icon("file-pdf")
                ),
                CustomDownloadButton(
                  "outputId2",
                  label = "Télécharger au format \u00A9Excel",
                  style = "simple",
                  color = "success",
                  size = "sm",
                  block = FALSE,
                  no_outline = TRUE,
                  icon = icon("file-excel")
                ),
                CustomDownloadButton(
                  "outputId3",
                  label = "(New!) Synthèse polaire des composantes",
                  style = "simple",
                  color = "royal",
                  size = "sm",
                  block = FALSE,
                  no_outline = TRUE,
                  icon = icon("chart-pie")
                )
              ))
      
    })
    output$result_boxes <- renderUI({
      div( 
        
        fluidRow(
          bs4Card(
            inputId = ns("doc_dim"),
            title = "Vos résultats par les dimensions de la durabilité",
            status = "info",
            footer = htmlOutput(ns("IDEAtext")),
            width = 6,
            collapsible = TRUE,
            collapsed = FALSE,
            closable = FALSE,
            solidHeader = TRUE,
            fluidRow(
              bs4InfoBoxOutput(ns("note_ae"), width = 4),
              bs4InfoBoxOutput(ns("note_st"), width = 4),
              bs4InfoBoxOutput(ns("note_ec"), width = 4)
            )
          ),
          
          
          bs4Card(
            inputId = ns("doc_prop"),
            title = "Vos résultats par les propriétés de la durabilité",
            status = "info",
            footer = htmlOutput(ns("proptext")),
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            collapsed = FALSE,
            closable = FALSE,
            
            fluidRow(
              bs4InfoBoxOutput(ns("prop1")),
              bs4InfoBoxOutput(ns("prop2")),
              bs4InfoBoxOutput(ns("prop3")),
              bs4InfoBoxOutput(ns("prop4")),
              bs4InfoBoxOutput(ns("prop5"), width = 8),
            )
            
            
          )),
      )
      
    })
    
  })
}

## To be copied in the UI
# mod_analyse_indiv_ui("analyse_indiv_ui_1")

## To be copied in the server
# callModule(mod_analyse_indiv_server, "analyse_indiv_ui_1")

