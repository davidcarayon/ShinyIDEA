#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_welcome_server, "welcome_ui_1")
  callModule(mod_analyse_indiv_server, "analyse_indiv_ui_1")
  callModule(dlmodule, "analyse_indiv_ui_1")
  callModule(mod_analyse_group_server, "analyse_group_ui_1")
  callModule(dlmodule_group, "analyse_group_ui_1")
}
