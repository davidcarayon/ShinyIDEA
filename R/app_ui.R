#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # List the first level UI elements here 
    bs4DashPage(
      title = "ShinyIDEA",
      sidebar_collapsed = FALSE,
      
      # navigation bar
      navbar = bs4DashNavbar(
        skin = "dark",
        status = "primary"),
      
      # left sidebar
      sidebar = bs4DashSidebar(
        skin = "dark",
        status = "primary",
        title = "ShinyIDEA",
        brandColor = "primary",
        #src = "some_image.png",
        elevation = 3,
        opacity = 0.8,
        
        # left sidebar menu
        bs4SidebarMenu(
          bs4SidebarMenuItem(
            "CGU",
            tabName = "welcome",
            icon = 'info'
          ),
          bs4SidebarMenuItem(
            "Analyse individuelle",
            tabName = "indiv",
            icon = 'user'
          ),
          bs4SidebarMenuItem(
            "Analyse de groupe",
            tabName = "animate",
            icon = 'users'
          )
        )
      ),
      
      # main body
      body = bs4DashBody(
        
       
        
        
        
        bs4TabItems(
          bs4TabItem(
            tabName = "welcome",
            mod_welcome_ui("welcome_ui_1")
          ),
          bs4TabItem(
            tabName = "indiv",
            mod_analyse_indiv_ui("analyse_indiv_ui_1")
          ),
          bs4TabItem(
            tabName = "animate",
            # mod_animate_ui("animate_ui_1")
          )
        )
      ),
      
      # footer
      footer = bs4DashFooter(
        copyrights = a(
          href = "https://idea.chlorofil.fr/idea-version-4.html",
          target = "_blank",
          "Méthode IDEA Version 4"
        ),
        right_text = "\u00A9 David Carayon (INRAE)"
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ShinyIDEA2'
    ),
    tags$style(
      type = 'text/css',
      '.info-box.bg-success {background-color: #0D8A00!important; color: #FFFFFF }'),
    tags$style(
      type = 'text/css',
      '.info-box.bg-warning {background-color: #FE942E!important; color: #FFFFFF }'),
    tags$style(
      type = 'text/css',
      '.info-box.bg-danger {background-color: #FF0000!important; color: #FFFFFF }'),
    tags$style(
      type = 'text/css',
      '.info-box.bg-info {background-color: #6db866!important; color: #FFFFFF }'),
    tags$style(
      type = 'text/css',
      '.info-box.bg-def {background-color: #FF6348!important; color: #000000; }'),
    tags$style(
      type = 'text/css',
      '.info-box-icon.elevation-3 {color: #FFFFFF; }'),
    

    
    tags$head(tags$style(".progress-bar{background-color:#28A745;}"))
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

