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


