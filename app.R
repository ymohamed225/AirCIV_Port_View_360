library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)
library(thematic)
library(scales)

# --- THEME CONFIGURATION ---
airci_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#F77F00", # Orange AirCI
  secondary = "#009E60", # Vert AirCI
  success = "#009E60",
  info = "#F77F00",
  warning = "#F77F00",
  danger = "#e74c3c",
  light = "#f8f9fa",
  dark = "#2c3e50",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  "font-size-base" = "0.9rem"
)

# Apply theme to plotly/ggplot
thematic_on(bg = "white", fg = "#2c3e50", accent = "#F77F00", font = "Inter")

# --- DATA GENERATION (In-App) ---
# Check if data exists, otherwise generate
if (!file.exists("airci_data.rds")) {
  source("data_prep.R")
}
data_list <- readRDS("airci_data.rds")
passengers <- data_list$passengers
flights <- data_list$flights
cities <- data_list$cities
monthly_traffic <- data_list$monthly_traffic

# --- UI DEFINITION ---
ui <- page_navbar(
  title = tags$div(
    class = "d-flex align-items-center navbar-brand-custom",
    tags$img(
      src = "assets/img/air.png",
      alt = "AirCI Logo",
      class = "navbar-logo"
    ),
    tags$div(
      class = "navbar-title-wrap",
      span("AirCI", class = "navbar-title-primary"),
      span("Intelligence Dashboard", class = "navbar-title-secondary")
    )
  ),
  theme = airci_theme,
  bg = "#F77F00",
  
  # --- Page 1: Accueil ---
  nav_panel(
    title = tags$span(icon("home"), "Accueil", class = "nav-tab-text"),
    div(
      class = "container-fluid p-4 page-content",
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h1(class = "page-title", "Tableau de Bord Stratégique"),
          p(class = "page-subtitle", "Suivi en temps réel des performances et indicateurs clés d'Air Côte d'Ivoire")
        )
      ),
      # Filtres Accueil
      layout_sidebar(
        sidebar = sidebar(
          title = tags$div(icon("filter"), "Filtres Tableau de Bord", class = "sidebar-title-content"),
          selectInput("accueil_period", "Période", 
            choices = c("12 derniers mois" = "12m", "6 derniers mois" = "6m", "3 derniers mois" = "3m", "Tout" = "all"),
            selected = "12m"
          ),
          selectInput("accueil_country_filter", "Pays (graphique)", 
            choices = c("Tous les pays", sort(unique(passengers$country))),
            selected = "Tous les pays"
          ),
          width = 280
        ),
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = tags$div(icon("users"), " Total Passagers", class = "d-flex align-items-center"),
          value = format(sum(monthly_traffic$passengers), big.mark = " "),
          showcase = icon("users", class = "fa-2x"),
          theme = "primary",
          full_screen = FALSE,
          height = "140px",
          fill = TRUE
        ),
        value_box(
          title = tags$div(icon("id-card"), " Membres sMiles", class = "d-flex align-items-center"),
          value = format(nrow(passengers), big.mark = " "),
          showcase = icon("id-card", class = "fa-2x"),
          theme = "success",
          height = "140px",
          fill = TRUE
        ),
        value_box(
          title = tags$div(icon("gem"), " Clients VIP", class = "d-flex align-items-center"),
          value = format(sum(passengers$status == "Diamant"), big.mark = " "),
          showcase = icon("gem", class = "fa-2x"),
          theme = "warning",
          height = "140px",
          fill = TRUE
        ),
        value_box(
          title = tags$div(icon("plane-arrival"), " Destinations", class = "d-flex align-items-center"),
          value = nrow(cities),
          showcase = icon("plane-arrival", class = "fa-2x"),
          theme = "info",
          height = "140px",
          fill = TRUE
        ),
        value_box(
          title = tags$div(icon("heart"), " Fidélisation", class = "d-flex align-items-center"),
          value = "84%",
          showcase = icon("heart", class = "fa-2x"),
          theme = "success",
          height = "140px",
          fill = TRUE
        ),
        value_box(
          title = tags$div(icon("chart-line"), " Croissance", class = "d-flex align-items-center"),
          value = "+12.4%",
          showcase = icon("chart-line", class = "fa-2x"),
          theme = "primary",
          height = "140px",
          fill = TRUE
        )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        class = "dashboard-card",
        card_header(
          "Évolution Mensuelle du Trafic",
          icon("chart-line"),
          class = "card-header-custom"
        ),
        plotlyOutput("traffic_plot", height = "400px")
      ),
      card(
        full_screen = TRUE,
        class = "dashboard-card",
        card_header(
          "Répartition par Pays d'Origine",
          icon("globe"),
          class = "card-header-custom"
        ),
        plotlyOutput("country_plot", height = "400px")
      )
    )
    )
  ),
  
  # --- Page 2: Analyse des Passagers ---
  nav_panel(
    title = tags$span(icon("users"), "Analyse Passagers", class = "nav-tab-text"),
    div(class = "container-fluid p-4 page-content",
      h1(class = "page-title", "Analyse Détaillée des Passagers"),
      p(class = "page-subtitle", "Exploration des données clients et comportements de voyage"),
      layout_sidebar(
        sidebar = sidebar(
          title = tags$div(icon("filter"), "Filtres Clients", class = "sidebar-title-content"),
          selectInput("status_filter", "Niveau sMiles", choices = c("Tous", sort(unique(passengers$status)))),
          selectInput("segment_filter", "Segmentation", choices = c("Tous", sort(unique(passengers$segment)))),
          sliderInput("miles_range", "Nombre de Miles", min = 0, max = 200000, value = c(0, 200000)),
          selectInput("country_filter", "Pays d'origine", choices = c("Tous", sort(unique(passengers$country)))),
          width = 280
        ),
      layout_columns(
        col_widths = 12,
        card(
          full_screen = TRUE,
          class = "dashboard-card",
          card_header(
            "Base de données Passagers sMiles",
            icon("database"),
            class = "card-header-custom"
          ),
          DTOutput("passenger_table")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          class = "dashboard-card",
          card_header(
            "Distribution des Miles par Statut",
            icon("chart-bar"),
            class = "card-header-custom"
          ),
          plotlyOutput("miles_dist_plot", height = "350px")
        ),
        card(
          full_screen = TRUE,
          class = "dashboard-card",
          card_header(
            "Engagement (Nombre de Vols)",
            icon("plane"),
            class = "card-header-custom"
          ),
          plotlyOutput("flights_dist_plot", height = "350px")
        )
      )
    )
  ),
  
  # --- Page 3: Carte des Destinations ---
  nav_panel(
    title = tags$span(icon("map-marked-alt"), "Destinations", class = "nav-tab-text"),
    div(class = "container-fluid p-4 page-content",
      h1(class = "page-title", "Réseau et Performance des Destinations"),
      p(class = "page-subtitle", "Visualisation géographique des routes et analyse des performances par destination"),
      layout_sidebar(
        sidebar = sidebar(
          title = tags$div(icon("filter"), "Filtres Destinations", class = "sidebar-title-content"),
          selectInput("dest_route_filter", "Destination", 
            choices = c("Toutes", sort(unique(flights$destination))),
            selected = "Toutes"
          ),
          dateRangeInput("dest_date_range", "Période", 
            start = min(flights$date), 
            end = max(flights$date),
            format = "dd/mm/yyyy",
            language = "fr",
            separator = " au "
          ),
          sliderInput("dest_min_passengers", "Passagers min. par vol", 
            min = 50, max = 200, value = 50, step = 10
          ),
          width = 280
        ),
      layout_columns(
        col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        class = "dashboard-card",
        card_header(
          "Réseau Air Côte d'Ivoire",
          icon("map-marked-alt"),
          class = "card-header-custom"
        ),
        leafletOutput("route_map", height = "600px")
      ),
      card(
        full_screen = TRUE,
        class = "dashboard-card",
        card_header(
          "Performance des Routes",
          icon("chart-bar"),
          class = "card-header-custom"
        ),
        plotlyOutput("route_performance_plot", height = "600px")
      )
    )
    )
  ),
  
  # --- Page 4: Segmentation & Marketing ---
  nav_panel(
    title = tags$span(icon("bullseye"), "Marketing", class = "nav-tab-text"),
    div(class = "container-fluid p-4 page-content",
      h1(class = "page-title", "Segmentation Client & Stratégies Marketing"),
      p(class = "page-subtitle", "Analyse des segments clients et opportunités de croissance"),
      layout_sidebar(
        sidebar = sidebar(
          title = tags$div(icon("filter"), "Filtres Marketing", class = "sidebar-title-content"),
          selectInput("marketing_segment_filter", "Segment Client", 
            choices = c("Tous les segments", sort(unique(passengers$segment))),
            selected = "Tous les segments"
          ),
          selectInput("marketing_status_filter", "Statut sMiles", 
            choices = c("Tous", sort(unique(passengers$status))),
            selected = "Tous"
          ),
          sliderInput("marketing_revenue_range", "Revenu estimé (FCFA M)", 
            min = 0, max = 15, value = c(0, 15), step = 1
          ),
          width = 280
        ),
      layout_columns(
        col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        class = "dashboard-card",
        card_header(
          "Volume des Segments Clients",
          icon("chart-pie"),
          class = "card-header-custom"
        ),
        plotlyOutput("segment_pie", height = "400px")
      ),
      card(
        full_screen = TRUE,
        class = "dashboard-card",
        card_header(
          "Revenu par Segment (Est.)",
          icon("dollar-sign"),
          class = "card-header-custom"
        ),
        plotlyOutput("segment_revenue", height = "400px")
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        full_screen = TRUE,
        class = "dashboard-card",
        card_header(
          "Stratégies Marketing par Segment",
          icon("bullhorn"),
          class = "card-header-custom"
        ),
        accordion(
          accordion_panel(
            "💎 Business Travelers",
            "Profil: Voyageurs fréquents haut de gamme.",
            tags$ul(
              tags$li("Surclassement prioritaire"),
              tags$li("Accès Lounge Premium"),
              tags$li("Service Conciergerie")
            )
          ),
          accordion_panel(
            "✈️ Frequent Travelers",
            "Profil: Voyageurs réguliers fidèles.",
            tags$ul(
              tags$li("Bonus miles x2"),
              tags$li("Promotions exclusives"),
              tags$li("Bagage supplémentaire offert")
            )
          ),
          accordion_panel(
            "🧳 Occasional Travelers",
            "Profil: Voyageurs sporadiques.",
            tags$ul(
              tags$li("Campagnes email ciblées"),
              tags$li("Remises 'Last Minute'"),
              tags$li("Parrainage sMiles")
            )
          ),
          accordion_panel(
            "🌴 Tourists",
            "Profil: Voyageurs loisirs.",
            tags$ul(
              tags$li("Packs Famille"),
              tags$li("Offres Destinations Vacances"),
              tags$li("Partenariats Hôtels")
            )
          )
        )
      ),
      card(
        full_screen = TRUE,
        class = "dashboard-card",
        card_header(
          "Potentiel de Croissance",
          icon("chart-line"),
          class = "card-header-custom"
        ),
        div(
          class = "text-center p-4",
          tags$h3(style = "color: #009E60; font-weight: bold;", "ROI Estimé: +18%"),
          p("Analyse prédictive des opportunités marketing basées sur le comportement passager.", class = "mb-3"),
          div(
            class = "alert alert-success",
            icon("lightbulb"),
            "Optimisation des campagnes via segmentation dynamique."
          )
        )
      )
    )
    )
  ),
  
  # Footer Professionnel
  div(
    class = "footer-professional",
    div(
      class = "footer-main",
      div(
        class = "container",
        div(
          class = "row footer-row",
          div(
            class = "col-lg-4 col-md-6 footer-col",
            h5(class = "footer-heading", icon("plane"), "AirCI Intelligence"),
            p(class = "footer-desc",
              "Plateforme d'analyse stratégique et Business Intelligence pour Air Côte d'Ivoire. Tableaux de bord en temps réel, indicateurs clés et insights data-driven."
            ),
            div(
              class = "footer-social",
              a(href = "#", class = "footer-social-link", icon("linkedin"), title = "LinkedIn"),
              a(href = "#", class = "footer-social-link", icon("twitter"), title = "Twitter"),
              a(href = "#", class = "footer-social-link", icon("facebook-f"), title = "Facebook")
            )
          ),
          div(
            class = "col-lg-2 col-md-6 footer-col",
            h5(class = "footer-heading", "Navigation"),
            tags$ul(class = "footer-links",
              tags$li(a(href = "#", class = "footer-link", "Accueil")),
              tags$li(a(href = "#", class = "footer-link", "Analyse Passagers")),
              tags$li(a(href = "#", class = "footer-link", "Destinations")),
              tags$li(a(href = "#", class = "footer-link", "Marketing"))
            )
          ),
          div(
            class = "col-lg-2 col-md-6 footer-col",
            h5(class = "footer-heading", "Ressources"),
            tags$ul(class = "footer-links",
              tags$li(a(href = "#", class = "footer-link", "Documentation")),
              tags$li(a(href = "#", class = "footer-link", "Support Technique")),
              tags$li(a(href = "#", class = "footer-link", "API")),
              tags$li(a(href = "#", class = "footer-link", "Changelog"))
            )
          ),
          div(
            class = "col-lg-4 col-md-6 footer-col",
            h5(class = "footer-heading", "Contact"),
            div(class = "footer-contact-item",
              icon("map-marker-alt", class = "footer-contact-icon"),
              span("Aéroport FHB, Abidjan, Côte d'Ivoire")
            ),
            div(class = "footer-contact-item",
              icon("envelope", class = "footer-contact-icon"),
              a(href = "mailto:contact@airci.ci", class = "footer-link", "contact@airci.ci")
            ),
            div(class = "footer-contact-item",
              icon("phone", class = "footer-contact-icon"),
              a(href = "tel:+22527000000", class = "footer-link", "+225 27 00 00 00")
            ),
            div(class = "footer-contact-item",
              icon("globe", class = "footer-contact-icon"),
              a(href = "https://www.airci.ci", class = "footer-link", target = "_blank", "www.airci.ci")
            )
          )
        )
      )
    ),
    div(
      class = "footer-bottom",
      div(
        class = "container",
        div(
          class = "row align-items-center",
          div(
            class = "col-md-6 text-center text-md-start",
            p(class = "footer-copyright",
              paste0("© ", format(Sys.Date(), "%Y"), " Air Côte d'Ivoire. Tous droits réservés.")
            )
          ),
          div(
            class = "col-md-6 text-center text-md-end",
            p(class = "footer-credits",
              "Propulsé par ", icon("chart-line", style = "color: #F77F00;"), " Data Intelligence"
            )
          )
        )
      )
    )
  )
  )
),

# --- UI Custom CSS ---
tags$head(
  tags$style(HTML("
    /* ===== NAVBAR PROFESSIONNEL ===== */
    .navbar { 
      position: sticky !important;
      top: 0 !important;
      z-index: 1030 !important;
      border-bottom: 4px solid #009E60 !important; 
      box-shadow: 0 4px 20px rgba(0,0,0,0.12);
      background: linear-gradient(135deg, #F77F00 0%, #e06500 50%, #d85a00 100%) !important;
      padding: 0.6rem 1.5rem !important;
      min-height: 72px !important;
    }
    .navbar-brand-custom {
      display: flex;
      align-items: center;
      gap: 14px;
    }
    .navbar-logo {
      height: 52px;
      margin-right: 4px;
      border-radius: 10px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.2);
      transition: transform 0.3s ease;
    }
    .navbar-logo:hover {
      transform: scale(1.05);
    }
    .navbar-title-wrap {
      display: flex;
      flex-direction: column;
      line-height: 1.2;
    }
    .navbar-title-primary {
      font-weight: 800;
      font-size: 1.5rem;
      color: #ffffff !important;
      letter-spacing: -0.5px;
      text-shadow: 0 2px 4px rgba(0,0,0,0.2);
    }
    .navbar-title-secondary {
      font-weight: 500;
      font-size: 0.75rem;
      color: rgba(255,255,255,0.9) !important;
      letter-spacing: 1px;
      text-transform: uppercase;
    }
    .nav-tab-text {
      font-weight: 600 !important;
      font-size: 0.95rem;
      padding: 0.5rem 1rem !important;
    }
    .nav-link {
      font-weight: 600;
      transition: all 0.25s ease;
      border-radius: 10px;
      margin: 0 4px;
      padding: 0.6rem 1.2rem !important;
    }
    .nav-link:hover {
      background-color: rgba(255, 255, 255, 0.15) !important;
      transform: translateY(-1px);
      color: #ffffff !important;
    }
    .nav-link.active {
      background-color: rgba(255, 255, 255, 0.25) !important;
      border-bottom: 3px solid #ffffff;
      color: #ffffff !important;
      box-shadow: 0 2px 8px rgba(0,0,0,0.15);
    }
    
    /* ===== VALUE BOXES ===== */
    .bslib-value-box { 
      color: #2c3e50; 
      transition: all 0.35s cubic-bezier(0.175, 0.885, 0.32, 1.275);
      border-radius: 14px;
      overflow: hidden;
      position: relative;
      border: 1px solid rgba(0,0,0,0.06);
    }
    .bslib-value-box::before {
      content: '';
      position: absolute;
      top: 0;
      left: -100%;
      width: 100%;
      height: 100%;
      background: linear-gradient(90deg, transparent, rgba(255,255,255,0.25), transparent);
      transition: left 0.5s;
    }
    .bslib-value-box:hover {
      transform: translateY(-6px);
      box-shadow: 0 14px 28px rgba(0,0,0,0.12);
    }
    .bslib-value-box:hover::before {
      left: 100%;
    }
    
    /* ===== CARDS & GRAPHIQUES ===== */
    .dashboard-card, .card { 
      border: 1px solid rgba(0,0,0,0.06);
      box-shadow: 0 2px 12px rgba(0,0,0,0.08);
      transition: all 0.3s ease;
      border-radius: 14px;
      overflow: hidden;
      background: #ffffff;
    }
    .dashboard-card:hover, .card:hover {
      box-shadow: 0 8px 28px rgba(0,0,0,0.12);
      transform: translateY(-3px);
    }
    .card-header, .card-header-custom { 
      background: linear-gradient(135deg, #F77F00 0%, #e06500 50%, #009E60 100%) !important; 
      font-weight: 700 !important; 
      border: none !important;
      border-radius: 14px 14px 0 0 !important;
      color: #ffffff !important;
      font-size: 1.05rem !important;
      padding: 1rem 1.5rem !important;
      letter-spacing: 0.3px;
    }
    .card-header .bi, .card-header-custom .bi {
      margin-right: 8px;
      opacity: 0.95;
    }
    
    /* ===== PAGE CONTENT ===== */
    .page-content {
      padding-top: 2rem !important;
    }
    .page-title {
      color: #2c3e50 !important;
      font-weight: 700 !important;
      margin-bottom: 0.5rem !important;
      text-align: center !important;
      font-size: 1.9rem !important;
    }
    .page-title::after {
      content: '';
      display: block;
      width: 80px;
      height: 4px;
      background: linear-gradient(90deg, #F77F00, #009E60);
      margin: 1rem auto 0;
      border-radius: 2px;
    }
    .page-subtitle {
      text-align: center !important;
      color: #5a6c7d !important;
      font-size: 1.05rem !important;
      margin-bottom: 2.5rem !important;
    }
    
    /* ===== FOOTER PROFESSIONNEL ===== */
    .footer-professional {
      margin-top: 4rem;
      background: linear-gradient(180deg, #1a252f 0%, #2c3e50 50%, #1a252f 100%);
      border-top: 4px solid #F77F00;
      color: #e8eaed;
    }
    .footer-main {
      padding: 3rem 0 2rem;
    }
    .footer-row {
      row-gap: 2rem;
    }
    .footer-col {
      padding: 0 1.5rem;
    }
    .footer-heading {
      font-weight: 700 !important;
      font-size: 1rem !important;
      color: #F77F00 !important;
      margin-bottom: 1.25rem !important;
      text-transform: uppercase;
      letter-spacing: 1px;
    }
    .footer-heading .bi {
      margin-right: 6px;
    }
    .footer-desc {
      font-size: 0.9rem;
      line-height: 1.7;
      color: #b0b8c0;
      margin-bottom: 1.25rem;
    }
    .footer-social {
      display: flex;
      gap: 10px;
    }
    .footer-social-link {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      width: 38px;
      height: 38px;
      background: rgba(247, 127, 0, 0.2);
      color: #F77F00 !important;
      border-radius: 10px;
      transition: all 0.3s ease;
    }
    .footer-social-link:hover {
      background: #F77F00;
      color: #ffffff !important;
      transform: translateY(-2px);
    }
    .footer-links {
      list-style: none;
      padding: 0;
      margin: 0;
    }
    .footer-links li {
      margin-bottom: 0.6rem;
    }
    .footer-link {
      color: #b0b8c0 !important;
      text-decoration: none !important;
      font-size: 0.9rem;
      transition: color 0.25s ease;
    }
    .footer-link:hover {
      color: #F77F00 !important;
    }
    .footer-contact-item {
      display: flex;
      align-items: flex-start;
      gap: 10px;
      margin-bottom: 0.9rem;
      font-size: 0.9rem;
      color: #b0b8c0;
    }
    .footer-contact-icon {
      color: #F77F00;
      margin-top: 2px;
      flex-shrink: 0;
    }
    .footer-bottom {
      padding: 1.25rem 0;
      border-top: 1px solid rgba(255,255,255,0.08);
    }
    .footer-copyright, .footer-credits {
      font-size: 0.85rem;
      margin: 0;
      color: #8a95a0;
    }
  "))
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # 1. Accueil Plots
  output$traffic_plot <- renderPlotly({
    p <- ggplot(accueil_traffic(), aes(x = month, y = passengers)) +
      geom_line(color = "#F77F00", linewidth = 2.5, alpha = 0.8) +
      geom_point(color = "#F77F00", size = 4, alpha = 0.9) +
      geom_smooth(method = "loess", se = TRUE, fill = "#F77F00", alpha = 0.2, color = "#F77F00") +
      scale_y_continuous(labels = scales::comma_format()) +
      theme_minimal(base_size = 14, base_family = "Inter") +
      theme(
        panel.grid.major = element_line(color = "#e0e0e0", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "#2c3e50", size = 14, face = "bold"),
        axis.text = element_text(color = "#5a6c7d", size = 12),
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#fafafa")
      ) +
      labs(x = "Mois", y = "Nombre de Passagers", title = "Évolution Mensuelle du Trafic")
    ggplotly(p, tooltip = c("x", "y"), dynamicTicks = TRUE) %>%
      layout(
        hoverlabel = list(bgcolor = "#F77F00", font = list(color = "white", size = 12)),
        margin = list(t = 50, b = 60, l = 60, r = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#fafbfc",
        font = list(family = "Inter", size = 12, color = "#2c3e50")
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  output$country_plot <- renderPlotly({
    country_data <- accueil_country_data()
    if (nrow(country_data) == 0) return(plotly_empty())
    
    p <- ggplot(country_data, aes(x = reorder(country, count), y = count, fill = count)) +
      geom_col(show.legend = FALSE, width = 0.7) +
      geom_text(aes(label = paste0(count, " (", percentage, "%)")), 
                hjust = -0.1, size = 3.5, color = "#2c3e50", fontface = "bold") +
      scale_fill_gradient(low = "#009E60", high = "#F77F00") +
      scale_y_continuous(labels = scales::comma_format()) +
      coord_flip() +
      theme_minimal(base_size = 14, base_family = "Inter") +
      theme(
        panel.grid.major.x = element_line(color = "#e0e0e0", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_text(color = "#2c3e50", size = 14, face = "bold"),
        axis.text.x = element_text(color = "#5a6c7d", size = 12),
        axis.text.y = element_text(color = "#2c3e50", size = 12, face = "bold"),
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#fafafa")
      ) +
      labs(x = "Pays d'Origine", y = "Nombre de Passagers", title = "Top 10 Pays par Nombre de Passagers")
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        hoverlabel = list(bgcolor = "#009E60", font = list(color = "white", size = 12)),
        margin = list(t = 50, b = 60, l = 60, r = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#fafbfc"
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # Filtres Accueil - Données filtrées
  accueil_traffic <- reactive({
    n_months <- switch(input$accueil_period, "12m" = 12, "6m" = 6, "3m" = 3, "all" = Inf)
    df <- monthly_traffic
    if (n_months < Inf) {
      cutoff <- max(monthly_traffic$month) - months(n_months)
      df <- df %>% filter(month >= cutoff)
    }
    df
  })
  
  accueil_country_data <- reactive({
    df <- passengers
    if (input$accueil_country_filter != "Tous les pays") {
      df <- df %>% filter(country == input$accueil_country_filter)
    }
    df %>%
      group_by(country) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      head(10) %>%
      mutate(percentage = round(count / sum(count) * 100, 1))
  })
  
  # 2. Analyse Passagers
  filtered_passengers <- reactive({
    df <- passengers
    if (input$status_filter != "Tous") df <- df %>% filter(status == input$status_filter)
    if (input$segment_filter != "Tous") df <- df %>% filter(segment == input$segment_filter)
    if (input$country_filter != "Tous") df <- df %>% filter(country == input$country_filter)
    df <- df %>% filter(miles >= input$miles_range[1], miles <= input$miles_range[2])
    df
  })
  
  output$passenger_table <- renderDT({
    datatable(filtered_passengers(), 
            options = list(pageLength = 15, scrollX = TRUE),
            rownames = FALSE,
            class = 'cell-border stripe hover') %>%
      formatStyle('status', 
                  backgroundColor = styleEqual(c('Diamant', 'Or', 'Argent', 'Bronze'), 
                                             c('#F77F00', '#FFD700', '#C0C0C0', '#CD7F32')),
                  color = 'white',
                  fontWeight = 'bold')
  })
  
  output$miles_dist_plot <- renderPlotly({
    p <- ggplot(filtered_passengers(), aes(x = status, y = miles, fill = status)) +
      geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.fill = "#F77F00", outlier.size = 3) +
      geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
      scale_fill_manual(values = c("Diamant" = "#F77F00", "Or" = "#FFD700", "Argent" = "#C0C0C0", "Bronze" = "#CD7F32")) +
      scale_y_continuous(labels = scales::comma_format()) +
      theme_minimal(base_size = 14, base_family = "Inter") +
      theme(
        panel.grid.major = element_line(color = "#e0e0e0", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "#2c3e50", size = 14, face = "bold"),
        axis.text = element_text(color = "#5a6c7d", size = 12),
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#fafafa"),
        legend.position = "none"
      ) +
      labs(x = "Statut sMiles", y = "Nombre de Miles", title = "Distribution des Miles par Statut")
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(hoverlabel = list(bgcolor = "#F77F00", font = list(color = "white")))
  })
  
  output$flights_dist_plot <- renderPlotly({
    p <- ggplot(filtered_passengers(), aes(x = flights_count)) +
      geom_histogram(binwidth = 1, fill = "#009E60", color = "white", alpha = 0.8, boundary = 0) +
      geom_density(aes(y = after_stat(count) * max(after_stat(count))), 
                   fill = "#F77F00", alpha = 0.3, color = "#F77F00", size = 1.5) +
      scale_x_continuous(breaks = seq(0, max(filtered_passengers()$flights_count, na.rm = TRUE), by = 2)) +
      theme_minimal(base_size = 14, base_family = "Inter") +
      theme(
        panel.grid.major = element_line(color = "#e0e0e0", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "#2c3e50", size = 14, face = "bold"),
        axis.text = element_text(color = "#5a6c7d", size = 12),
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#fafafa")
      ) +
      labs(x = "Nombre de Vols", y = "Nombre de Passagers", title = "Distribution du Nombre de Vols")
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(hoverlabel = list(bgcolor = "#009E60", font = list(color = "white")))
  })
  
  # 3. Destinations - Données filtrées
  filtered_flights <- reactive({
    req(input$dest_date_range)
    df <- flights
    if (input$dest_route_filter != "Toutes") df <- df %>% filter(destination == input$dest_route_filter)
    df <- df %>% filter(date >= as.Date(input$dest_date_range[1]), date <= as.Date(input$dest_date_range[2]))
    df <- df %>% filter(passengers_count >= input$dest_min_passengers)
    df
  })
  
  output$route_map <- renderLeaflet({
    abidjan <- cities %>% filter(city == "Abidjan")
    others <- cities %>% filter(city != "Abidjan")
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -5, lat = 7.5, zoom = 4) %>%
      addCircleMarkers(
        data = abidjan, 
        lng = ~lon, lat = ~lat, 
        radius = 15, 
        fillColor = "#F77F00", 
        color = "#ffffff", 
        weight = 3,
        fillOpacity = 0.9,
        label = "Abidjan (HUB Principal)"
      ) %>%
      addCircleMarkers(
        data = others, 
        lng = ~lon, 
        lat = ~lat, 
        radius = 8, 
        fillColor = "#009E60", 
        color = "#ffffff", 
        weight = 2,
        fillOpacity = 0.8,
        label = ~city
      )
  })
  
  
  output$route_performance_plot <- renderPlotly({
    fl <- filtered_flights()
    if (nrow(fl) == 0) return(plotly_empty())
    route_data <- fl %>%
      group_by(destination) %>%
      summarise(passengers = sum(passengers_count), 
                flights = n(),
                avg_passengers = mean(passengers_count),
                .groups = 'drop') %>%
      arrange(desc(passengers)) %>%
      mutate(percentage = round(passengers / sum(passengers) * 100, 1))
    
    p <- ggplot(route_data, aes(x = reorder(destination, passengers), y = passengers, fill = passengers)) +
      geom_col(show.legend = FALSE, width = 0.7, alpha = 0.8) +
      geom_text(aes(label = paste0(passengers, " (", percentage, "%)")), 
                hjust = -0.1, size = 3.2, color = "#2c3e50", fontface = "bold") +
      scale_fill_gradient(low = "#009E60", high = "#F77F00") +
      scale_y_continuous(labels = scales::comma_format()) +
      coord_flip() +
      theme_minimal(base_size = 14, base_family = "Inter") +
      theme(
        panel.grid.major.x = element_line(color = "#e0e0e0", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_text(color = "#2c3e50", size = 14, face = "bold"),
        axis.text.x = element_text(color = "#5a6c7d", size = 12),
        axis.text.y = element_text(color = "#2c3e50", size = 12, face = "bold"),
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#fafafa")
      ) +
      labs(x = "Destination", y = "Total Passagers", title = "Performance des Routes par Destination")
    ggplotly(p, tooltip = c("x", "y", "flights", "avg_passengers")) %>%
      layout(hoverlabel = list(bgcolor = "#F77F00", font = list(color = "white")))
  })
  
  # 4. Segmentation - Données filtrées Marketing
  filtered_marketing <- reactive({
    df <- passengers
    if (input$marketing_segment_filter != "Tous les segments") df <- df %>% filter(segment == input$marketing_segment_filter)
    if (input$marketing_status_filter != "Tous") df <- df %>% filter(status == input$marketing_status_filter)
    df <- df %>% mutate(est_revenue_m = (flights_count * 150000) / 1e6)
    df <- df %>% filter(est_revenue_m >= input$marketing_revenue_range[1], est_revenue_m <= input$marketing_revenue_range[2])
    df
  })
  
  output$segment_pie <- renderPlotly({
    seg_data <- filtered_marketing() %>%
      group_by(segment) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round(count / sum(count) * 100, 1),
             label_text = paste0(segment, "<br>", count, " (", percentage, "%)"))
    if (nrow(seg_data) == 0) return(plotly_empty())
    
    colors <- c("#F77F00", "#009E60", "#2c3e50", "#bdc3c7")
    names(colors) <- unique(seg_data$segment)
    
    plot_ly(seg_data, 
            labels = ~segment, 
            values = ~count, 
            type = 'pie',
            text = ~label_text,
            textinfo = 'none',
            hoverinfo = 'text',
            marker = list(
              colors = colors,
              line = list(color = '#ffffff', width = 2)
            ),
            showlegend = TRUE,
            sort = FALSE) %>%
      layout(
        title = list(
          text = "<b>Répartition des Segments Clients</b>",
          font = list(size = 16, color = "#2c3e50"),
          x = 0.5
        ),
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 0.5,
          font = list(size = 12, color = "#2c3e50")
        ),
        margin = list(l = 50, r = 150, t = 50, b = 50),
        hoverlabel = list(
          bgcolor = "#F77F00",
          font = list(color = "white", size = 12)
        )
      )
  })
  
  output$segment_revenue <- renderPlotly({
    rev_data <- filtered_marketing() %>%
      group_by(segment) %>%
      summarise(
        revenue = sum(flights_count * 150000),
        avg_revenue = mean(flights_count * 150000),
        .groups = 'drop'
      ) %>%
      arrange(desc(revenue))
    if (nrow(rev_data) == 0) return(plotly_empty())
    
    p <- ggplot(rev_data, aes(x = reorder(segment, revenue), y = revenue, fill = segment)) +
      geom_col(show.legend = FALSE, width = 0.7, alpha = 0.8) +
      geom_text(aes(label = scales::comma(revenue, prefix = "FCFA ", suffix = " M")), 
                hjust = -0.05, size = 3.2, color = "#2c3e50", fontface = "bold") +
      scale_fill_manual(values = c("#F77F00", "#009E60", "#2c3e50", "#bdc3c7")) +
      scale_y_continuous(
        labels = scales::unit_format(unit = "M", scale = 1e-6, prefix = "FCFA "),
        expand = expansion(mult = c(0, 0.15))
      ) +
      coord_flip() +
      theme_minimal(base_size = 14, base_family = "Inter") +
      theme(
        panel.grid.major.x = element_line(color = "#e0e0e0", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_text(color = "#2c3e50", size = 14, face = "bold"),
        axis.text.x = element_text(color = "#5a6c7d", size = 12),
        axis.text.y = element_text(color = "#2c3e50", size = 12, face = "bold"),
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#fafafa")
      ) +
      labs(x = "Segment Client", y = "Revenu Estimé (FCFA)", title = "Revenu par Segment Client")
    ggplotly(p, tooltip = c("x", "y", "avg_revenue")) %>%
      layout(hoverlabel = list(bgcolor = "#009E60", font = list(color = "white")))
  })
}

# --- RUN APPLICATION ---
shinyApp(ui, server)
