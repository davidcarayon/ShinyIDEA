#' analyse_group UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
#' @import DT
mod_analyse_group_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    fluidRow(
      col_12(
        bs4Card(
          inputId = ns("doc_card_group"),
          title = "A propos de ce module",
          status = "info",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          includeMarkdown(app_sys("app", "docs", "explore_group.md")),
          fileInput(ns("dir"), "Charger plusieurs calculateurs", accept = c(".xls",".xlsx",".json"), width = "100%", multiple = TRUE, buttonLabel = "Charger...",placeholder = "Maintenir la touche CTRL pour une sélection multiple")
        )
      )
    ),

      uiOutput(ns("result_boxes")),
      uiOutput(ns("download_box"))
  )
}
    
#' analyse_group Server Function
#'
#' @noRd 
mod_analyse_group_server <- function(input, output, session){
  ns <- session$ns
 
  # Permet d'éviter une erreur liée au fichier "Rplots.pdf"
  pdf(NULL)
  
  # Définition du dossier de travail temporaire où créer puis piocher les fichiers
  outdir <- tempdir()
  
  IDEAcollectivedata <- eventReactive(input$dir,{
    importIDEA(input = dirname(input$dir$datapath[[1]]), anonymous = FALSE)
  })
  
  observeEvent(input$dir, {
    
    output$group_boxplot <- renderPlot({
      
      df_dim <- IDEAcollectivedata()$dataset %>%
        distinct(id_exploit,dimension,dimension_value)%>%
        mutate(dimension = factor(dimension, levels = c("Agroécologique","Socio-Territoriale","Economique")))
      
      moys <- df_dim %>% group_by(dimension) %>% summarise(Moyenne = mean(dimension_value))
      
      p <- ggplot(df_dim, aes(x = dimension, y = dimension_value)) +
        stat_boxplot(geom = "errorbar", width = 0.3) +
        geom_boxplot(color = "black", aes(fill = dimension), width = 0.8) +
        geom_point(data = moys, aes(x = dimension, y = Moyenne), size = 4, color = "darkred",shape = 18) +
        ggrepel::geom_label_repel(data = moys, aes(x = dimension, y = Moyenne, label = paste0("Moyenne = ",round(Moyenne,1))), nudge_x = 0.5, nudge_y = 5) +
        theme_tq_cust() +
        scale_fill_manual(values = c("Agroécologique" = "#2e9c15", "Socio-Territoriale" = "#5077FE", "Economique" = "#FE962B")) +
        theme(axis.title.x = element_blank()) +
        labs(y = "Valeur de la dimension",fill = "Dimension") +
        scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))
      
      p
      
    })
    output$table_prop <- DT::renderDataTable({
      
      df <- IDEAcollectivedata()$nodes$Global %>%
        gather(key = indicateur, value = resultat, -id_exploit) %>%
        mutate(indicateur = replace_indicateur(indicateur)) %>%
        inner_join(label_nodes, by = c("indicateur" = "code_indicateur")) %>%
        mutate(resultat = factor(resultat, levels = c("très favorable", "favorable", "intermédiaire", "défavorable", "très défavorable", "NC"))) %>%
        mutate(nom_indicateur = ifelse(nom_indicateur == "Capacité productive et reproductive de biens et de services", yes = "Capacité productive et \n reproductive de biens et de \n services", no = nom_indicateur)) %>%
        mutate(num_indic = parse_number(indicateur)) %>%
        arrange(dim, num_indic) %>%
        mutate(indicateur = factor(indicateur, levels = unique(indicateur))) %>%
        mutate(level = case_when(
          level == "indicateur" ~ "Indicateur",
          level == "propriete" ~ "Propriété"
        )) %>% 
        filter(level == "Propriété") %>% 
        select(id_exploit,nom_indicateur,resultat) %>% 
        spread(key = nom_indicateur, value = resultat)
      
      rown <- df$id_exploit
      df <- df %>% select(-id_exploit)
      
      
      DT::datatable(df, rownames = rown, options = list(pageLength = 4)) %>% 
        formatStyle('Robustesse', backgroundColor = styleEqual(c("favorable", "défavorable", "intermédiaire","très défavorable", "très favorable"), c('#1CDA53', '#FF6348',"#FFA300","#FF0000","#0D8A00"))) %>% 
        formatStyle('Autonomie', backgroundColor = styleEqual(c("favorable", "défavorable", "intermédiaire","très défavorable", "très favorable"), c('#1CDA53', '#FF6348',"#FFA300","#FF0000","#0D8A00"))) %>% 
        formatStyle('Ancrage territorial', backgroundColor = styleEqual(c("favorable", "défavorable", "intermédiaire","très défavorable", "très favorable"), c('#1CDA53', '#FF6348',"#FFA300","#FF0000","#0D8A00"))) %>% 
        formatStyle('Capacité productive et \n reproductive de biens et de \n services', backgroundColor = styleEqual(c("favorable", "défavorable", "intermédiaire","très défavorable", "très favorable"), c('#1CDA53', '#FF6348',"#FFA300","#FF0000","#0D8A00"))) %>% 
        formatStyle('Responsabilité globale', backgroundColor = styleEqual(c("favorable", "défavorable", "intermédiaire","très défavorable", "très favorable"), c('#1CDA53', '#FF6348',"#FFA300","#FF0000","#0D8A00"))) %>% 
        formatStyle(c("Robustesse","Autonomie", "Ancrage territorial", "Capacité productive et \n reproductive de biens et de \n services", "Responsabilité globale"), border = '1px solid #ddd')
      
    })
    output$result_boxes <- renderUI({
  
      fluidRow(  
      
      bs4Card(inputId = ns("boxplotCard"),
                title = "Résultats par les dimensions",
                status = "info",
                width = 5,
                collapsible = TRUE,
                collapsed = FALSE,
                closable = FALSE,
                solidHeader = TRUE,
                plotOutput(ns("group_boxplot")) %>% withSpinner(color="#0dc5c1")),
      
      bs4Card(inputId = ns("DTCard"),
              title = "Résultats par les propriétés",
              status = "info",
              width = 7,
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,
              solidHeader = TRUE,
        DT::dataTableOutput(ns("table_prop")) %>% withSpinner(color="#0dc5c1")
      )
        

      
      
      
      )
      
      
      
    })
    

    output$download_box <- renderUI({
      
      
      bs4Card(inputID = "download_card_group",title = "Télécharger le diagnostic de groupe complet",width = 12,
              solidHeader = TRUE, status = "info",closable = FALSE,
              div(
                style="display:inline-block;width:100%;text-align: center;",
                CustomDownloadButton(
                  ns("outputId_group"),
                  label = "Télécharger au format PDF",
                  icon = icon("file-pdf")
                ),
                CustomDownloadButton(
                  ns("outputId2_group"),
                  label = "Télécharger au format XLSX",
                  icon = icon("file-excel")
                ),
                CustomDownloadButton(
                  ns("outputId6_group"),
                  label = "Télécharger au format PPTX",
                  icon = icon("file-powerpoint")
                ),
                CustomDownloadButton(
                  ns("outputId3_group"),
                  label = "Télécharger au format DOCX",
                  icon = icon("file-word")
                ),
                CustomDownloadButton(
                  ns("outputId4_group"),
                  label = "Télécharger au format ODT",
                  icon = icon("file-word")
                ),
                CustomDownloadButton(
                  ns("outputId5_group"),
                  label = "Télécharger au format ZIP",
                  icon = icon("file-archive")
                )
              ))
      
    })
    
    
    
    
  })
  
  
  
  
  
}
    
## To be copied in the UI
# mod_analyse_group_ui("analyse_group_ui_1")
    
## To be copied in the server
# callModule(mod_analyse_group_server, "analyse_group_ui_1")
 
