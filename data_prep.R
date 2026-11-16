library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(42)

# 1. Cities & Destinations
cities <- data.frame(
  city = c("Abidjan", "Paris", "Dakar", "Lagos", "Casablanca", "Beyrouth", "Ouagadougou", "Niamey", "Bamako", "Libreville", "Brazzaville"),
  lat = c(5.36, 48.85, 14.71, 6.52, 33.57, 33.89, 12.37, 13.51, 12.63, 0.41, -4.26),
  lon = c(-4.00, 2.35, -17.44, 3.37, -7.58, 35.50, -1.53, 2.10, -8.00, 9.45, 15.28)
)

# 2. Passengers Data
n_passengers <- 5000
first_names <- c("Jean", "Marie", "Abdoulaye", "Koffi", "Fatou", "Amadou", "Aicha", "Issa", "Saliou", "Yao", "Konan", "Adjoua", "Moussa")
last_names <- c("Coulibaly", "Touré", "Koné", "Ouattara", "Diallo", "Bamba", "Fofana", "Traoré", "Sylla", "Diabaté", "Sidibé")

countries <- c("Côte d'Ivoire", "France", "Sénégal", "Nigéria", "Maroc", "Liban", "Burkina Faso", "Niger", "Mali", "Gabon", "Congo")

passengers <- data.frame(
  id = 1:n_passengers,
  name = paste(sample(first_names, n_passengers, replace = TRUE), sample(last_names, n_passengers, replace = TRUE)),
  country = sample(countries, n_passengers, replace = TRUE, prob = c(0.4, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)),
  miles = rgamma(n_passengers, shape = 2, scale = 20000),
  flights_count = rpois(n_passengers, lambda = 5) + 1
)

# Assign Status
passengers <- passengers %>%
  mutate(
    status = case_when(
      miles >= 100000 ~ "Diamant",
      miles >= 60000 ~ "5 Étoiles",
      miles >= 25000 ~ "4 Étoiles",
      TRUE ~ "3 Étoiles"
    ),
    # Segmentation
    segment = case_when(
      status == "Diamant" | (status == "5 Étoiles" & flights_count > 10) ~ "Business Travelers",
      flights_count > 15 ~ "Frequent Travelers",
      flights_count > 5 ~ "Occasional Travelers",
      TRUE ~ "Tourists"
    )
  )

# 3. Flights Data (Past 12 months)
dates <- seq(as.Date("2025-03-01"), as.Date("2026-03-01"), by="day")
n_flights <- 2000
flights <- data.frame(
  flight_id = 1:n_flights,
  date = sample(dates, n_flights, replace = TRUE),
  origin = "Abidjan",
  destination = sample(cities$city[-1], n_flights, replace = TRUE),
  passengers_count = sample(50:200, n_flights, replace = TRUE),
  revenue = runif(n_flights, 50000, 200000)
)

# Calculate monthly traffic
monthly_traffic <- flights %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    passengers = sum(passengers_count),
    revenue = sum(revenue),
    flights = n()
  ) %>%
  arrange(month)

# Save as RDS for Shiny
saveRDS(list(passengers = passengers, flights = flights, cities = cities, monthly_traffic = monthly_traffic), "airci_data.rds")
