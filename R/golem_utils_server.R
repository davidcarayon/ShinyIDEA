#' Inverted versions of in, is.null and is.na
#' 
#' @noRd
#' 
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#' 
#' @noRd
#' 
#' @example 
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
#' 
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NA`
#' 
#' @noRd
#' 
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList





replace_col <- function(resultat) {
  res <- dplyr::case_when(
    resultat == "NC" ~ "grey",
    resultat == "favorable" ~ "info",
    resultat == "très favorable" ~ "success",
    resultat == "défavorable" ~ "def",
    resultat == "très défavorable" ~ "danger",
    resultat == "intermédiaire" ~ "warning"
  )
  return(res)
}


CustomDownloadButton <- function(outputId, label = "Download", style = "unite", color = "primary", 
                              size = "md", block = FALSE, no_outline = TRUE, icon = icon("download")) {
  bttn <- actionBttn(inputId = paste0(outputId, "_bttn"), 
                     label = tagList(tags$a(id = outputId, class = "shiny-download-link", 
                                            href = "", target = "_blank", download = NA), label), 
                     color = color, style = style, size = size, block = block, 
                     no_outline = no_outline, icon = icon)
  htmltools::tagAppendAttributes(bttn, onclick = sprintf("getElementById('%s').click()", 
                                                         outputId))}  


theme_tq_cust <- function(base_size = 15, base_family = "Avenir Next Pro") {
  
  # Tidyquant colors
  blue  <- "#2c3e50"
  green <- "#18BC9C"
  white <- "#FFFFFF"
  grey  <- "grey80"
  
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    ggplot2::theme(
      
      # Base Inherited Elements
      line               =  ggplot2::element_line(colour = blue, size = 0.5, linetype = 1,
                                                  lineend = "butt"),
      rect               =  ggplot2::element_rect(fill = white, colour = blue,
                                                  size = 0.5, linetype = 1),
      text               =  ggplot2::element_text(family = base_family, face = "plain",
                                                  colour = blue, size = base_size,
                                                  lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                                                  margin = ggplot2::margin(), debug = FALSE),
      
      # Axes
      axis.line          = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(size = rel(0.8)),
      axis.ticks         = ggplot2::element_line(color = grey, size = rel(1/3)),
      axis.title         = ggplot2::element_text(size = rel(1.0), face = "bold"),
      
      # Panel
      panel.background   = ggplot2::element_rect(fill = white, color = NA),
      panel.border       = ggplot2::element_rect(fill = NA, size = rel(1/2), color = blue),
      panel.grid.major   = ggplot2::element_line(color = grey, size = rel(1/3)),
      panel.grid.minor   = ggplot2::element_line(color = grey, size = rel(1/3)),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.spacing      = ggplot2::unit(.75, "cm"),
      
      # Legend
      legend.key         = ggplot2::element_rect(fill = white, color = NA),
      legend.position    = "top",
      legend.title = element_text(face = "bold"),
      
      # Strip (Used with multiple panels)
      strip.background   = ggplot2::element_rect(fill = blue, color = blue),
      strip.text         = ggplot2::element_text(color = white, size = ggplot2::rel(0.8), margin = ggplot2::margin(t = 5, b = 5)),
      
      # Plot
      plot.title         = ggplot2::element_text(size = rel(1.2), hjust = 0,
                                                 margin = ggplot2::margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")),
      plot.subtitle      = ggplot2::element_text(size = rel(0.9), hjust = 0,
                                                 margin = ggplot2::margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")),
      
      # Complete theme
      complete = TRUE
    )
}

replace_indicateur <- function(indicateur) {
  res <- dplyr::case_when(
    indicateur %in% liste_indicateurs ~ indicateur,
    indicateur == "Diversité de l'organisation spatiale et temporelle" ~ "R1",
    indicateur == "Limiter l'exposition aux aléas" ~ "R2",
    indicateur == "Diversité des activités" ~ "R3",
    indicateur == "En favorisant la diversité" ~ "R4",
    indicateur == "De l'outil de production" ~ "R5",
    indicateur == "En développant l'inertie et les capacités tampon" ~ "R6",
    indicateur == "Réduire la sensibilité" ~ "R7",
    indicateur == "par l'insertion dans les réseaux" ~ "R8",
    indicateur == "Augmenter la capacité d'adaptation" ~ "R9",
    indicateur == "Robustesse" ~ "R10",
    
    indicateur == "Naturelles" ~ "CP1",
    indicateur == "Travail" ~ "CP2",
    indicateur == "Compétences et équipements" ~ "CP3",
    indicateur == "Sociales et humaines" ~ "CP4",
    indicateur == "Préserver ou créer des ressources pour l'acte de production" ~ "CP5",
    indicateur == "Développer la capacité alimentaire" ~ "CP6",
    indicateur == "Capacité à produire dans le temps des biens et services remunérés" ~ "CP7",
    indicateur == "Capacité de remboursement" ~ "CP8",
    indicateur == "Capacité à dégager un revenu dans le temps" ~ "CP9",
    indicateur == "Capacité productive et reproductive de biens et de services" ~ "CP10",
    
    indicateur == "Liberté de décision organisationnelle" ~ "AU1",
    indicateur == "Liberté de décision dans les relations commerciales" ~ "AU2",
    indicateur == "Disposer d'une liberté de décision dans ses choix de gouvernance et de production" ~ "AU3",
    indicateur == "Disposer d'une autonomie financière" ~ "AU4",
    indicateur == "Autonomie dans le processus productif" ~ "AU5",
    indicateur == "Autonomie" ~ "AU6",
    
    indicateur == "Partage et transparence des activités productives" ~ "RG1",
    indicateur == "Ouverture et relation au monde non agricole" ~ "RG2",
    indicateur == "Sécurité alimentaire" ~ "RG3",
    indicateur == "Implications et engagements sociaux" ~ "RG4",
    indicateur == "Ressources naturelles" ~ "RG5",
    indicateur == "Ressources énergétiques et manufacturées" ~ "RG6",
    indicateur == "Partager équitablement les ressources" ~ "RG7",
    indicateur == "Conditions de travail de la main d'oeuvre " ~ "RG8",
    indicateur == "Conditions de travail de la main d'oeuvre" ~ "RG8",
    indicateur == "Conditions de vie et de travail" ~ "RG9",
    indicateur == "Bien être de la vie animale" ~ "RG10",
    indicateur == "Contribuer à la qualité de vie sur l'exploitation" ~ "RG11",
    indicateur == "Réduire les émissions" ~ "RG12",
    indicateur == "Réduire l'usage des produits polluants" ~ "RG13",
    indicateur == "Réduire ses impacts sur la santé et les écosystèmes" ~ "RG14",
    indicateur == "Responsabilité globale" ~ "RG15",
    
    indicateur == "Valoriser la qualité territoriale" ~ "AN1",
    indicateur == "Contribuer à des démarches d'économie circulaire" ~ "AN2",
    indicateur == "Par le travail et l'emploi" ~ "AN3",
    indicateur == "S'inscrire dans des démarches de territoire" ~ "AN4",
    indicateur == "Ancrage territorial" ~ "AN5"
  )
  
  
  return(res)
}