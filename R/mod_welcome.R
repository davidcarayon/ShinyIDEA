mod_welcome_ui <- function(id){
  ns <- NS(id)
  tagList(
    includeMarkdown(app_sys("app", "docs", "bandeau.md")),
    hr(),
    fluidRow(
      col_12(
        bs4Callout(width = 12,
          title = "Bienvenue sur Outils IDEA",
          headerBorder = FALSE,
          closable = FALSE,
          collapsible = FALSE,
          includeMarkdown(app_sys("app", "docs", "welcome.md")),
        )
      )
    ),
    
    # Acknowledgments
    fluidRow(
      col_12(
        bs4Callout(
          title = "A propos de cette application",
          width = 12,
          status = "primary",
          "L'application ShinyIDEA est un programme libre. Celui-est diffusé dans l’espoir qu’il sera utile, mais sans aucune garantie de qualité marchande ou d’adéquation à un but particulier."
        )
      )
    ),
    
    fluidRow(
      col_4(
        bs4UserCard(
          title = "David Carayon",
          subtitle = "Ingénieur statisticien | Développeur",
          status = "primary",
          width = 12,
          src = "www/david.png",
          bs4ListGroup(
            width = 12,
            bs4ListGroupItem(
              "Contact",
              type = "action",
              src = "mailto:david.carayon@inrae.fr"
            ),
            bs4ListGroupItem(
              "Github",
              type = "action",
              src = "https://github.com/davidcarayon"
            ),
            bs4ListGroupItem(
              "Twitter: @david_carayon",
              type = "action",
              src = "https://twitter.com/david_carayon"
            ),
          )
        )
      ),
      col_4(
        bs4UserCard(
          title = "Frédéric Zahm",
          subtitle = "Agroéconomiste | Président du C.S. IDEA",
          status = "primary",
          width = 12,
          src = "www/zahm.jpg",
          bs4ListGroup(
            width = 12,
            bs4ListGroupItem(
              "Contact",
              type = "action",
              src = "mailto:frederic.zahm@inrae.fr"
            )
          )
        )
      ),
      col_4(
        bs4UserCard(
          title = "Sydney Girard",
          subtitle = "Ingénieur agronome | Concepteur du calculateur IDEA4",
          status = "primary",
          width = 12,
          src = "www/girard.jpg",
          bs4ListGroup(
            width = 12,
            bs4ListGroupItem(
              "Contact",
              type = "action",
              src = "mailto:sydney.girard@inrae.fr"
            )
          )
        )
      )
    )
  )
}

#' welcome Server Function
#'
#' @noRd 
mod_welcome_server <- function(input, output, session){
  ns <- session$ns
  
}

## To be copied in the UI
# mod_welcome_ui("welcome_ui_1")

## To be copied in the server
# callModule(mod_welcome_server, "welcome_ui_1")