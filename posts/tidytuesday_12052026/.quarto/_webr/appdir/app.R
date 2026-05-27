library(bslib)
library(shiny)
library(readr)
library(utils)
library(dplyr)
library(stringr)
library(tidyr)
library(jsonlite)
library(maps)
library(ggplot2)
if (FALSE) {
  library(munsell)
}

# Chargement des données

nodes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-12/cities.csv')
edges <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-12/links.csv')


edges_geo <- edges %>%
  left_join(nodes, by = c("source" = "id")) %>%
  rename(
    from = name,
    lon_from = lng,
    lat_from = lat
  ) %>%
  left_join(nodes, by = c("target" = "id")) %>%
  rename(
    to = name,
    lon_to = lng,
    lat_to = lat
  ) %>%
  rename(to = to)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: white !important;
      }
      .container-fluid {
        padding: 0 !important;
        /* Ajout de display: flex pour utiliser le modèle de boîte flexible */
        display: flex;
        flex-direction: column;
        height: 100vh; /* La hauteur du conteneur est la hauteur de la fenêtre */
      }
      .row {
        flex-shrink: 0; /* Empêche les lignes du haut de se réduire */
      }
      #visa_plot {
        width: 100% !important;
        /* Le graphique prend l'espace restant */
        flex-grow: 1;
      }
      /* Ajustements pour les marges des lignes */
      .fluid-row {
        margin-left: 0 !important;
        margin-right: 0 !important;
      }
    "))
  ),
  uiOutput("dynamic_title"),
  
  # Nouvelle disposition : l'entrée en haut, le graphique en dessous
  fluidRow(
    column(12,
           selectInput("country_selector", "Choisir une ville :",
                       choices = sort(unique(na.omit(nodes$name))),
                       selected = "Fukaya")
    )
  ),
  fluidRow(
    column(12,
           plotOutput("visa_plot")
    )
  )
)

server <- function(input, output, session) {
  
  output$dynamic_title <- renderUI({
    tags$div(
      class = "shiny-title-panel",
      h2(paste("Liste des villes jumelles avec :", input$country_selector))
    )
  })
  
  reactive_data <- reactive({
    
    edges_geo_selected <- edges_geo %>%
      filter(to == input$country_selector)
    
    if (nrow(edges_geo_selected) == 0) {
      return(NULL)
    }
    
    edges_for_plot <- edges_geo_selected %>%
      transmute(
        from,
        to,
        x = lon_from,
        y = lat_from,
        xend = lon_to,
        yend = lat_to
      )
    
    city_in_edges <- unique(c(edges_for_plot$from, edges_for_plot$to))
    
    filtered_nodes <- nodes %>%
      filter(name %in% city_in_edges)
    
    return(list(edges = edges_for_plot, nodes = filtered_nodes))
  })
  
  # -------------------------
  # 🔥 LA PARTIE QUI MANQUAIT
  # -------------------------
  output$visa_plot <- renderPlot({
    plot_data <- reactive_data()
    
    if (is.null(plot_data)) {
      return(
        ggplot() +
          geom_text(aes(x = 0, y = 0, label = "Aucune ville jumelle trouvée."),
                    size = 5) +
          theme_void()
      )
    }
    
    world <- map_data("world")
    
    ggplot() +
      geom_polygon(
        data = world,
        aes(long, lat, group = group),
        fill = "white",
        color = "#4A4A4A",
        linewidth = 0.15
      ) +
      geom_curve(
        data = plot_data$edges,
        aes(x = x, y = y, xend = xend, yend = yend),
        curvature = 0.33,
        color = "black",
        alpha = 0.5
      ) +
      geom_point(
        data = plot_data$nodes,
        aes(x = lng, y = lat),
        color = "black",
        size = 3
      ) +
      geom_text(
        data = plot_data$nodes,
        aes(x = lng, y = lat, label = name),
        nudge_y = 2,
        size = 3,
        fontface = "bold"
      ) +
      theme_void()
  })
}


shinyApp(ui, server)
