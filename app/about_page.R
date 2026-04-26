about_page <- function() {
  renderUI({
    fluidPage(
      tags$head(
        tags$style(HTML("
          .about-wrap { max-width: 44rem; margin: 0 auto; padding: 0.5rem 1.25rem 2.5rem; }
          .about-wrap .page-title { font-size: 1.35rem; font-weight: 600; margin: 0 0 0.25rem 0; color: #1a1a1a; }
          .about-wrap .page-sub { font-size: 0.95rem; color: #555; margin: 0 0 1.5rem 0; }
          .about-wrap h2 { font-size: 1.05rem; font-weight: 600; margin: 1.5rem 0 0.65rem 0; color: #222; border-bottom: 1px solid #e8e8e8; padding-bottom: 0.35rem; }
          .about-wrap p { line-height: 1.65; color: #444; margin: 0 0 0.9rem 0; font-size: 0.95rem; }
          .about-wrap ul { margin: 0 0 1rem 0; padding-left: 1.2rem; }
          .about-wrap li { margin-bottom: 0.45rem; line-height: 1.5; font-size: 0.95rem; color: #444; }
          .about-wrap a { color: #0d6efd; text-decoration: none; }
          .about-wrap a:hover { text-decoration: underline; }
          .about-wrap .muted { font-size: 0.88rem; color: #666; margin-top: 1.75rem; }
        "))
      ),
      div(
        class = "about-wrap",
        h1(class = "page-title", "About Portal Explorer"),
        p(
          class = "page-sub",
          "Interactive exploration of Portal Project rodent and environmental data."
        ),

        h2("What this app does"),
        p(
          "Portal Explorer is a Shiny application for visualizing data from the ",
          tags$a(href = "https://github.com/weecology/PortalData", "Portal Data", target = "_blank", rel = "noopener noreferrer"),
          " repository. Census and environmental layers are updated on a regular cadence and are openly available."
        ),
        p(
          "The app loads and shapes rodent and covariate series using the ",
          tags$a(href = "https://github.com/weecology/portalr", "portalr", target = "_blank", rel = "noopener noreferrer"),
          " R package. The Model Explorer tab uses ",
          tags$a(href = "https://github.com/weecology/portalcasting", "portalcasting", target = "_blank", rel = "noopener noreferrer"),
          " to align with the Portal forecasting workflow."
        ),

        h2("Related resources"),
        tags$ul(
          tags$li(
            tags$a(href = "https://portalexplorer.weecology.org", "Live deployment", target = "_blank", rel = "noopener noreferrer"),
            " — hosted instance of this app."
          ),
          tags$li(
            tags$a(href = "https://github.com/weecology/portal-forecasts", "portal-forecasts", target = "_blank", rel = "noopener noreferrer"),
            " — forecast archive and pipeline outputs."
          ),
          tags$li(
            tags$a(href = "https://portal.naturecast.org/", "portal.naturecast.org", target = "_blank", rel = "noopener noreferrer"),
            " — browse Portal forecasts in the NatureCast ecosystem."
          )
        ),

        h2("WEecology"),
        p(
          tags$a(href = "https://www.weecology.org/", "WEecology", target = "_blank", rel = "noopener noreferrer"),
          " is an interdisciplinary ecology research group at the University of Florida. ",
          "Morgan Ernest’s group studies how ecological systems change through time, with strong field programs; ",
          "Ethan White’s group uses computational and statistical approaches on large ecological and environmental datasets. ",
          "Together the lab publishes open data and software, builds tools for the community, and trains researchers in reproducible workflows."
        ),

        p(
          class = "muted",
          "Questions or issues? Open a discussion or issue on the ",
          tags$a(href = "https://github.com/weecology/portal-explorer", "portal-explorer", target = "_blank", rel = "noopener noreferrer"),
          " GitHub repository."
        )
      )
    )
  })
}
