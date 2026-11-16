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
    class = "d-flex align-items-center",
    tags$img(
      src = "assets/img/air.png",
      alt = "AirCI Logo",
      style = "height: 60px; margin-right: 15px; border-radius: 8px;"
    ),
    span("AirCI", style = "font-weight: 700; font-size: 1.4rem; color: #F77F00;"),
    #span("Intelligence Dashboard", style = "font-weight: 400; font-size: 1rem; color: #009E60; margin-left: 10px;")
  ),
  theme = airci_theme,
  bg = "#F77F00",
  
  # --- Page 1: Accueil ---
  nav_panel(
    title = tags$span(icon("home"), "Accueil", style = "font-weight: 600;"),
    div(
      class = "container-fluid p-4",
      h1(style = "color: #2c3e50; font-weight: 700; margin-bottom: 2rem; text-align: center;",
        "Tableau de Bord Stratégique"
      ),
      p(style = "text-align: center; color: #5a6c7d; font-size: 1.1rem; margin-bottom: 3rem;",
        "Suivi en temps réel des performances et indicateurs clés d'Air Côte d'Ivoire"
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
          card_header(
            "Évolution Mensuelle du Trafic",
            icon("chart-line")
          ),
          plotlyOutput("traffic_plot", height = "400px")
        ),
        card(
          full_screen = TRUE,
          card_header(
            "Répartition par Pays d'Origine",
            icon("globe")
          ),
          plotlyOutput("country_plot", height = "400px")
        )
      )
    )
  ),
  
  # --- Page 2: Analyse des Passagers ---
  nav_panel(
    title = tags$span(icon("users"), "Analyse Passagers", style = "font-weight: 600;"),
    div(class = "container-fluid p-4",
      layout_sidebar(
        sidebar = sidebar(
          title = tags$div(icon("filter"), "Filtres Clients", class = "d-flex align-items-center"),
          selectInput("status_filter", "Niveau sMiles", choices = c("Tous", unique(passengers$status))),
          selectInput("segment_filter", "Segmentation", choices = c("Tous", unique(passengers$segment))),
          sliderInput("miles_range", "Nombre de Miles", min = 0, max = 200000, value = c(0, 200000))
        ),
        layout_columns(
          col_widths = 12,
          card(
            full_screen = TRUE,
            card_header(
              "Base de données Passagers sMiles",
              icon("database")
            ),
            DTOutput("passenger_table")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            card_header(
              "Distribution des Miles par Statut",
              icon("chart-bar")
            ),
            plotlyOutput("miles_dist_plot", height = "350px")
          ),
          card(
            full_screen = TRUE,
            card_header(
              "Engagement (Nombre de Vols)",
              icon("plane")
            ),
            plotlyOutput("flights_dist_plot", height = "350px")
          )
        )
      )
    )
  ),
  
  # --- Page 3: Carte des Destinations ---
  nav_panel(
    title = tags$span(icon("map-marked-alt"), "Destinations", style = "font-weight: 600;"),
    div(class = "container-fluid p-4",
      layout_columns(
        col_widths = c(8, 4),
        card(
          full_screen = TRUE,
          card_header(
            "Réseau Air Côte d'Ivoire",
            icon("map-marked-alt")
          ),
          leafletOutput("route_map", height = "600px")
        ),
        card(
          full_screen = TRUE,
          card_header(
            "Performance des Routes",
            icon("chart-bar")
          ),
          plotlyOutput("route_performance_plot", height = "600px")
        )
      )
    )
  ),
  
  # --- Page 4: Segmentation & Marketing ---
  nav_panel(
    title = tags$span(icon("bullseye"), "Marketing", style = "font-weight: 600;"),
    div(class = "container-fluid p-4",
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header(
            "Volume des Segments Clients",
            icon("chart-pie")
          ),
          plotlyOutput("segment_pie", height = "400px")
        ),
        card(
          full_screen = TRUE,
          card_header(
            "Revenu par Segment (Est.)",
            icon("dollar-sign")
          ),
          plotlyOutput("segment_revenue", height = "400px")
        )
      )
    )
  ),
  
  # --- UI Custom CSS ---
  tags$head(
    tags$style(HTML("
      .navbar { 
        border-bottom: 4px solid #009E60 !important; 
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        background: linear-gradient(135deg, #F77F00 0%, #e67300 100%);
        padding-top: 0.5rem !important;
        padding-bottom: 0.5rem !important;
        min-height: 60px !important;
      }
      .navbar-brand {
        font-weight: 700;
        font-size: 1.3rem;
        transition: all 0.3s ease;
      }
      .bslib-value-box { 
        color: #2c3e50; 
        transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
        border-radius: 16px;
        overflow: hidden;
        position: relative;
      }
      .bslib-value-box:hover {
        transform: translateY(-8px) scale(1.02);
        box-shadow: 0 12px 24px rgba(0,0,0,0.2);
      }
      .card { 
        border: none;
        box-shadow: 0 4px 16px rgba(0,0,0,0.1);
        transition: all 0.3s ease;
        border-radius: 16px;
        overflow: hidden;
        background: linear-gradient(135deg, #ffffff 0%, #fafafa 100%);
      }
      .card:hover {
        box-shadow: 0 8px 32px rgba(0,0,0,0.15);
        transform: translateY(-4px);
      }
      .card-header { 
        background: linear-gradient(135deg, #F77F00 0%, #009E60 100%); 
        font-weight: 700; 
        border: none;
        border-radius: 16px 16px 0 0 !important;
        color: #ffffff;
        font-size: 1.1rem;
        padding: 1rem 1.5rem;
        text-shadow: 0 2px 4px rgba(0,0,0,0.2);
      }
      body {
        font-family: 'Inter', sans-serif;
        background: linear-gradient(135deg, #fafafa 0%, #f1f3f4 100%);
        color: #2c3e50;
      }
    "))
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # 1. Accueil Plots
  output$traffic_plot <- renderPlotly({
    p <- ggplot(monthly_traffic, aes(x = month, y = passengers)) +
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
    country_data <- passengers %>%
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
    route_data <- flights %>%
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
    seg_data <- passengers %>%
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
    # Simulated revenue calculation
    rev_data <- passengers %>%
      group_by(segment) %>%
      summarise(
        revenue = sum(flights_count * 150000),
        avg_revenue = mean(flights_count * 150000),
        .groups = 'drop'
      ) %>%
      arrange(desc(revenue))
    
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
