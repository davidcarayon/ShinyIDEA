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
#' @import fresh
#' @import ggplot2

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
          status = "secondary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          closeable = FALSE,
          includeMarkdown(app_sys("app", "docs", "explore.md")),
          fileInput(ns("files"), "Charger votre calculateur :", accept = c(".xls",".xlsx",".json"), width = "100%", multiple = FALSE, buttonLabel = "Charger...",placeholder = "Aucun fichier chargé"),
          uiOutput(ns("download_box"))
        )
      )
    ),
    
    
    
    bs4Card(
      inputId = ns("doc_dim"),
      title = "Vos résultats par les dimensions de la durabilité",
      status = "secondary",
      footer = htmlOutput(ns("IDEAtext")),
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      closeable = FALSE,
    fluidRow(
      bs4InfoBoxOutput(ns("note_ae"), width = 4),
      bs4InfoBoxOutput(ns("note_st"), width = 4),
      bs4InfoBoxOutput(ns("note_ec"), width = 4)
    )
    ),
    
    
    bs4Card(
      inputId = ns("doc_prop"),
      title = "Vos résultats par les propriétés de la durabilité",
      status = "secondary",
      footer = htmlOutput(ns("proptext")),
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      closeable = FALSE,
      
      fluidRow(
        bs4InfoBoxOutput(ns("prop1")),
        bs4InfoBoxOutput(ns("prop2")),
        bs4InfoBoxOutput(ns("prop3")),
        bs4InfoBoxOutput(ns("prop4")),
        bs4InfoBoxOutput(ns("prop5")),
      )
      
      
    ),
    
    
    bs4TabCard(width = 12,
      maximizable = TRUE,
      id = ns("tabcard"),
      title = "Exploration",
      bs4TabPanel(
        tabName = "Et maintenant ?",
        active = TRUE,
        HTML("Si le détail du calcul des dimensions et des propriétés vous intéresse, vous pouvez soit télécharger les documents proposés dans l'en-tête de cette page, soit utiliser les onglets de ce sous-module pour explorer directement vos résultats.<br> <b>NB: Ce module d'exploration peut être agrandi/rétréci via le bouton situé en haut à droite de la boîte.</b>")
      ),
      bs4TabPanel(
        tabName = "Comprendre mon évaluation : Composantes",
        active = FALSE,
        "Les composantes correspondent à des regroupements (sommes) des 53 indicateurs. Elles offrent un cadre de lecture intermédiaire entre les indicateurs et les dimensions.",
        br(),
        plotOutput(ns("plot_compo"),height = "800px")
      ),
      bs4TabPanel(
        tabName = "Comprendre mon évaluation : Indicateurs",
        active = FALSE,
        "Content"
      ),
      bs4TabPanel(
        tabName = "Comprendre mon évaluation : Arbres éclairés des propriétés",
        active = FALSE,
        "Content 2"
      )
    )
    
    
    
  
  )
}

#' analyse_indiv Server Function
#'
#' @noRd 
mod_analyse_indiv_server <- function(input, output, session){
  
  ns <- session$ns
  
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
        pull(nom_indicateur) %>% paste(collapse = " et ")
      
      
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
                 status = color, href = "#")
    })
    
    output$plot_compo <- renderPlot({
      
      dimensionP <- dimensionsPlots(IDEAdata())
      
      p <- dimensionP[[1]][[2]]$data
      
      g <- ggplot(p, aes(x = composante, y = composante_value)) +
        geom_bar(aes(x = composante, y = max_compo, fill = dimension), alpha = 0.3, color = "black", position = position_dodge(width = 0.8), stat = "identity") +
        geom_col(color = "black", aes(fill = dimension)) +
        geom_label(aes(label = paste0(composante_value,"/",max_compo)), size = 5) +
        scale_fill_manual(values = c("Agroécologique" = "#2e9c15", "Socio-Territoriale" = "#5077FE", "Economique" = "#FE962B")) +
        theme(axis.line = element_blank()) +
        theme_bw() +
        theme(panel.grid.major.x  = element_line(color = "grey90"),
              panel.grid.major.y = element_blank()) +
        theme(panel.grid.minor.x = element_line(color = "grey90"),
              panel.grid.minor.y = element_blank()) +
        theme(
          axis.title.y = element_blank(),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 17),
          strip.text = element_text(size = 9, face = "bold")
        ) +
        theme(strip.background = element_rect(fill = "white", color = "black")) +
        theme(axis.text = element_text(size = 17, color = "black"), axis.title = element_text(size = 17, face = "bold")) +
        labs(fill = "Dimension", y = "Valeur de la composante / valeur max") +
        theme(legend.position = "bottom") +
        coord_flip()  +
        theme(text = element_text(family = "Roboto"))
      
      g
      
    })
  
    
    output$download_box <- renderUI({
      
      div(style="display:inline-block;width:100%;text-align: center;",
        shinyWidgets::downloadBttn(
          "outputId",
          label = "Télécharger le rapport PDF",
          style = "gradient",
          color = "warning",
          size = "md",
          block = FALSE,
          no_outline = TRUE
        ),
        shinyWidgets::downloadBttn(
          "outputId2",
          label = "Télécharger le classeur excel augmenté",
          style = "gradient",
          color = "success",
          size = "md",
          block = FALSE,
          no_outline = TRUE
        ),
        shinyWidgets::downloadBttn(
          "outputId3",
          label = "Télécharger la nouvelle figure !",
          style = "gradient",
          color = "royal",
          size = "md",
          block = FALSE,
          no_outline = TRUE
        )
      )
      
    })
    
    
    
    
    
    
    
  })
}

## To be copied in the UI
# mod_analyse_indiv_ui("analyse_indiv_ui_1")

## To be copied in the server
# callModule(mod_analyse_indiv_server, "analyse_indiv_ui_1")

