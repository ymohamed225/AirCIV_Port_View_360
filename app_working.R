# Charger les packages nécessaires
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
      style = "height: 65px;"
    ),
    tags$div(
      class = "navbar-title-wrap",
      #span("AirCI", class = "navbar-title-primary"),
      #span("Intelligence Dashboard", class = "navbar-title-secondary")
    )
  ),
  theme = airci_theme,
  bg = "#F77F00",
  
  # --- Page 1: Accueil ---
  nav_panel(
    title = tags$span(icon("home"), "Accueil", class = "nav-tab-text"),
    div(
      class = "container-fluid p-4 page-content",
      h1(class = "page-title", "Tableau de Bord Stratégique"),
      p(class = "page-subtitle", "Suivi en temps réel des performances et indicateurs clés d'Air Côte d'Ivoire"),
      
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
          col_widths = c(6, 6),
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
              "Répartition Géographique des Segments",
              icon("map-marked-alt"),
              class = "card-header-custom"
            ),
            leafletOutput("segment_map", height = "400px")
          )
        ),
        layout_columns(
          col_widths = c(12),
          card(
            full_screen = TRUE,
            class = "dashboard-card",
            card_header(
              "Revenu par Segment (Est.)",
              icon("dollar-sign"),
              class = "card-header-custom"
            ),
            plotlyOutput("segment_revenue", height = "500px")
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
        padding: 0.4rem 1.5rem !important;
        min-height: 60px !important;
      }
      .navbar-brand-custom {
        display: flex;
        align-items: center;
        gap: 14px;
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
        margin-left: 8px;
      }
      .nav-tab-text .bi, .nav-tab-text svg {
        margin-right: 8px;
        font-size: 1rem;
      }
      .nav-link {
        font-weight: 600;
        transition: all 0.25s ease;
        border-radius: 10px;
        margin: 0 8px;
        padding: 0.8rem 1.5rem !important;
        text-align: center;
        min-width: 120px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .nav-link:hover {
        background-color: rgba(255, 255, 255, 0.15) !important;
        transform: translateY(-2px);
        color: #ffffff !important;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      .nav-link.active {
        background-color: rgba(255, 255, 255, 0.25) !important;
        border-bottom: 3px solid #ffffff;
        color: #ffffff !important;
        box-shadow: 0 4px 16px rgba(0,0,0,0.2);
        transform: translateY(-1px);
      }
      .navbar-nav {
        display: flex;
        justify-content: center;
        align-items: center;
        width: 100%;
        gap: 8px;
      }
      .navbar-nav .nav-item {
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .navbar-collapse {
        display: flex;
        justify-content: center;
        align-items: center;
        width: 100%;
      }
      .navbar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        width: 100%;
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
      
      /* ===== SIDEBAR & FILTRES ===== */
      .sidebar {
        background: linear-gradient(180deg, #ffffff 0%, #fafbfc 100%) !important;
        border-right: 1px solid #e8ecf0 !important;
        box-shadow: 2px 0 16px rgba(0,0,0,0.04);
        border-radius: 0 12px 12px 0;
      }
      .sidebar-title-content {
        color: #2c3e50 !important;
        font-weight: 700 !important;
        font-size: 1rem !important;
        padding-bottom: 0.75rem;
        margin-bottom: 1.25rem;
        border-bottom: 3px solid #F77F00;
      }
      .sidebar-title-content .bi {
        margin-right: 8px;
        color: #F77F00;
      }
      
      /* Forms */
      .form-label {
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 0.5rem;
      }
      .form-control, .form-select {
        border: 2px solid #e9ecef;
        border-radius: 8px;
        transition: all 0.3s ease;
        background: rgba(255, 255, 255, 0.9);
      }
      .form-control:focus, .form-select:focus {
        border-color: #F77F00;
        box-shadow: 0 0 0 0.2rem rgba(247, 127, 0, 0.25);
        background: #ffffff;
      }
      
      /* Buttons */
      .btn-primary {
        background: linear-gradient(135deg, #F77F00 0%, #e67300 100%);
        border: none;
        font-weight: 600;
        transition: all 0.3s ease;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(247, 127, 0, 0.3);
      }
      .btn-primary:hover {
        background: linear-gradient(135deg, #e67300 0%, #cc6600 100%);
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(247, 127, 0, 0.4);
      }
      
      /* Accordion */
      .accordion-button:not(.collapsed) {
        background-color: rgba(247, 127, 0, 0.1);
        color: #F77F00;
        font-weight: 600;
      }
      .accordion-button:focus {
        box-shadow: 0 0 0 0.2rem rgba(247, 127, 0, 0.25);
      }
      .accordion-item {
        border-radius: 8px;
        margin-bottom: 0.5rem;
        border: 1px solid #e9ecef;
      }
      
      /* DataTable */
      .dataTables_wrapper {
        font-family: 'Inter', sans-serif;
        border-radius: 12px;
        overflow: hidden;
      }
      .dataTables_filter input {
        border: 2px solid #e9ecef;
        border-radius: 8px;
        padding: 0.75rem;
        transition: all 0.3s ease;
        background: rgba(255, 255, 255, 0.9);
      }
      .dataTables_filter input:focus {
        border-color: #F77F00;
        outline: none;
        box-shadow: 0 0 0 0.2rem rgba(247, 127, 0, 0.25);
      }
      .table {
        border-radius: 8px;
        overflow: hidden;
      }
      .table thead th {
        background: linear-gradient(135deg, #F77F00 0%, #009E60 100%);
        color: #ffffff;
        font-weight: 600;
        border: none;
      }
      
      /* Plotly */
      .plotly {
        border-radius: 12px;
        overflow: hidden;
        box-shadow: 0 4px 16px rgba(0,0,0,0.1);
      }
      
      /* Leaflet */
      .leaflet-container {
        border-radius: 12px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.1);
        overflow: hidden;
      }
      
      /* Body */
      body {
        font-family: 'Inter', sans-serif;
        background: linear-gradient(135deg, #fafafa 0%, #f1f3f4 100%);
        color: #2c3e50;
      }
      .main {
        background: transparent;
      }
      
      /* Animations */
      @keyframes fadeInUp {
        from {
          opacity: 0;
          transform: translateY(30px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .card, .bslib-value-box {
        animation: fadeInUp 0.6s ease-out;
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
      
      /* Loading spinner */
      .shiny-spinner {
        border-color: #F77F00;
        border-top-color: transparent;
      }
      
      /* Responsive improvements */
      @media (max-width: 768px) {
        .bslib-value-box {
          margin-bottom: 1rem;
        }
        .card {
          margin-bottom: 1rem;
        }
        .container-fluid h1 {
          font-size: 1.75rem;
        }
      }
      
      /* Smooth scrolling */
      html {
        scroll-behavior: smooth;
      }
      
      /* Selection colors */
      ::selection {
        background: #F77F00;
        color: #ffffff;
      }
      ::-moz-selection {
        background: #F77F00;
        color: #ffffff;
      }
    "))
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # 1. Accueil Plots
  output$traffic_plot <- renderPlotly({
    traffic_data <- monthly_traffic
    n_months <- switch(input$accueil_period, "12m" = 12, "6m" = 6, "3m" = 3, "all" = Inf)
    if (n_months < Inf) {
      cutoff <- max(monthly_traffic$month) - months(n_months)
      traffic_data <- traffic_data %>% filter(month >= cutoff)
    }
    
    p <- ggplot(traffic_data, aes(x = month, y = passengers)) +
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
      layout(hoverlabel = list(bgcolor = "#F77F00", font = list(color = "white")))
  })
  
  output$country_plot <- renderPlotly({
    country_data <- passengers
    if (input$accueil_country_filter != "Tous les pays") {
      country_data <- country_data %>% filter(country == input$accueil_country_filter)
    }
    country_data <- country_data %>%
      group_by(country) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      head(10) %>%
      mutate(percentage = round(count / sum(count) * 100, 1))
    
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
      layout(hoverlabel = list(bgcolor = "#009E60", font = list(color = "white")))
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
  
  # 3. Destinations
  output$route_map <- renderLeaflet({
    # Appliquer les filtres pour déterminer les destinations à afficher
    filtered_flights_map <- flights
    if (input$dest_route_filter != "Toutes") {
      filtered_flights_map <- filtered_flights_map %>% filter(destination == input$dest_route_filter)
    }
    if (!is.null(input$dest_date_range) && length(input$dest_date_range) == 2) {
      filtered_flights_map <- filtered_flights_map %>% 
        filter(date >= as.Date(input$dest_date_range[1]), date <= as.Date(input$dest_date_range[2]))
    }
    filtered_flights_map <- filtered_flights_map %>% filter(passengers_count >= input$dest_min_passengers)
    
    # Obtenir les destinations filtrées
    filtered_destinations <- unique(filtered_flights_map$destination)
    
    abidjan <- cities %>% filter(city == "Abidjan")
    others <- cities %>% filter(city %in% filtered_destinations & city != "Abidjan")
    
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
    # Appliquer les filtres
    filtered_flights <- flights
    if (input$dest_route_filter != "Toutes") {
      filtered_flights <- filtered_flights %>% filter(destination == input$dest_route_filter)
    }
    if (!is.null(input$dest_date_range) && length(input$dest_date_range) == 2) {
      filtered_flights <- filtered_flights %>% 
        filter(date >= as.Date(input$dest_date_range[1]), date <= as.Date(input$dest_date_range[2]))
    }
    filtered_flights <- filtered_flights %>% filter(passengers_count >= input$dest_min_passengers)
    
    route_data <- filtered_flights %>%
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
  
  # 4. Segmentation
  output$segment_pie <- renderPlotly({
    # Appliquer les filtres
    filtered_marketing <- passengers
    if (input$marketing_segment_filter != "Tous les segments") {
      filtered_marketing <- filtered_marketing %>% filter(segment == input$marketing_segment_filter)
    }
    if (input$marketing_status_filter != "Tous") {
      filtered_marketing <- filtered_marketing %>% filter(status == input$marketing_status_filter)
    }
    
    seg_data <- filtered_marketing %>%
      group_by(segment) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round(count / sum(count) * 100, 1),
             label_text = paste0(segment, "<br>", count, " (", percentage, "%)"))
    
    colors <- c("#F77F00", "#009E60", "#2c3e50", "#bdc3c7")
    names(colors) <- unique(seg_data$segment)
    
    plot_ly(seg_data, 
            labels = ~segment, 
            values = ~count, 
            type = 'pie',
            textinfo = 'none',
            marker = list(
              colors = colors,
              line = list(color = '#ffffff', width = 2)
            ),
            hoverinfo = 'text',
            text = ~label_text) %>%
      layout(
        title = list(
          text = "<b>Volume des Segments Clients</b>",
          font = list(size = 16, color = "#2c3e50")
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 0.5,
          font = list(size = 12, color = "#2c3e50")
        ),
        margin = list(l = 50, r = 50, t = 50, b = 50),
        hoverlabel = list(
          bgcolor = "#F77F00",
          font = list(color = "white", size = 12)
        )
      )
  })
  
  # Carte de répartition géographique des segments
  output$segment_map <- renderLeaflet({
    # Appliquer les filtres (EXACTEMENT comme dans segment_pie)
    filtered_marketing_map <- passengers
    if (input$marketing_segment_filter != "Tous les segments") {
      filtered_marketing_map <- filtered_marketing_map %>% filter(segment == input$marketing_segment_filter)
    }
    if (input$marketing_status_filter != "Tous") {
      filtered_marketing_map <- filtered_marketing_map %>% filter(status == input$marketing_status_filter)
    }
    
    # Calculer les volumes par segment (EXACTEMENT comme dans segment_pie)
    seg_data <- filtered_marketing_map %>%
      group_by(segment) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round(count / sum(count) * 100, 1),
             label_text = paste0(segment, "<br>", count, " (", percentage, "%)"))
    
    # Couleurs par segment (EXACTEMENT comme dans segment_pie)
    colors <- c("#F77F00", "#009E60", "#2c3e50", "#bdc3c7")
    names(colors) <- unique(seg_data$segment)
    
    # Obtenir TOUS les pays où chaque segment est présent avec leurs valeurs
    country_segment_data <- filtered_marketing_map %>%
      group_by(country, segment) %>%
      summarise(passengers = n(), .groups = 'drop') %>%
      left_join(cities %>% rename(country = city), by = "country") %>%
      filter(!is.na(lat) & !is.na(lon)) %>%
      left_join(seg_data, by = "segment")  # Joindre avec les volumes totaux du segment
    
    # Créer des labels avec les valeurs par pays ET les totaux du segment
    country_segment_data$label_text <- paste0(
      "<b>", country_segment_data$country, "</b><br>",
      "<b>", country_segment_data$segment, "</b><br>",
      "<b>", country_segment_data$passengers, "</b> passagers<br>",
      "<small>Total segment: ", country_segment_data$count, " (", country_segment_data$percentage, "%)</small>"
    )
    
    # Taille des cercles basée sur le nombre de passagers dans le pays
    country_segment_data$radius_size <- sqrt(country_segment_data$passengers) * 3
    
    leaflet(country_segment_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -5, lat = 7.5, zoom = 3) %>%
      addCircleMarkers(
        lng = ~lon, 
        lat = ~lat, 
        radius = ~radius_size,
        fillColor = ~colors[segment],
        color = "#ffffff", 
        weight = 2,
        fillOpacity = 0.8,
        label = ~HTML(label_text)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = colors,
        labels = paste0(names(colors), "<br>", seg_data$count, " (", seg_data$percentage, "%)"),
        title = "Segments<br><small>(Volumes du pie chart)</small>"
      ) %>%
      addMiniMap(
        position = "bottomleft",
        tiles = providers$CartoDB.Positron,
        toggleDisplay = TRUE
      ) %>%
      addControl(
        HTML("<small><b>Valeurs par pays<br>+ totaux segments</b></small>"),
        position = "topleft"
      )
  })
  
  output$segment_revenue <- renderPlotly({
    # Appliquer les filtres
    filtered_marketing_rev <- passengers
    if (input$marketing_segment_filter != "Tous les segments") {
      filtered_marketing_rev <- filtered_marketing_rev %>% filter(segment == input$marketing_segment_filter)
    }
    if (input$marketing_status_filter != "Tous") {
      filtered_marketing_rev <- filtered_marketing_rev %>% filter(status == input$marketing_status_filter)
    }
    
    # Simulated revenue calculation
    rev_data <- filtered_marketing_rev %>%
      group_by(segment) %>%
      summarise(
        revenue = sum(flights_count * 150000),
        avg_revenue = mean(flights_count * 150000),
        .groups = 'drop'
      ) %>%
      arrange(desc(revenue)) %>%
      mutate(revenue_m = revenue / 1e6)
    
    p <- ggplot(rev_data, aes(x = reorder(segment, revenue), y = revenue, fill = segment)) +
      geom_col(show.legend = FALSE, width = 0.7, alpha = 0.8) +
      geom_text(aes(label = scales::comma(revenue, prefix = "FCFA ")), 
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
