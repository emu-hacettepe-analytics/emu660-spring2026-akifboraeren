# ==============================================================================
# Script: data_visualizer_extra.R
# Purpose: Extra EDA plots for Turkish airport traffic project
# Run after: data_loader.R + data_processing.R
# Output: PNG files under results/
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(stringi)
library(readxl)

if (!dir.exists("results")) dir.create("results")

# ------------------------------------------------------------------------------
# Theme
# ------------------------------------------------------------------------------
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10, margin = margin(b = 8)),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "black")
    )
)

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------
airport_key <- function(x) {
  x |>
    as.character() |>
    str_replace_all("\\(\\*\\)|\\*|\\(|\\)", "") |>
    stri_trans_general("Latin-ASCII") |>
    toupper() |>
    str_squish()
}

extract_city <- function(x) {
  key <- airport_key(x)
  case_when(
    str_detect(key, "^ISTANBUL") ~ "ISTANBUL",
    str_detect(key, "^IZMIR") ~ "IZMIR",
    str_detect(key, "^ANKARA") ~ "ANKARA",
    str_detect(key, "^ANTALYA") ~ "ANTALYA",
    TRUE ~ word(key, 1)
  )
}

standardize_airport_df <- function(df) {
  df |>
    rename(
      airport = 1,
      domestic = 2,
      international = 3,
      total = 4
    ) |>
    mutate(
      airport = str_squish(as.character(airport)),
      domestic = replace_na(as.numeric(domestic), 0),
      international = replace_na(as.numeric(international), 0),
      total = replace_na(as.numeric(total), 0),
      airport_clean = airport_key(airport),
      city = extract_city(airport)
    )
}

# Clean main datasets. IMPORTANT: We use the existing Toplam column, not sum of all numeric columns.
pax     <- standardize_airport_df(df_passenger)
cargo   <- standardize_airport_df(df_cargo)
freight <- standardize_airport_df(df_freight)
comm    <- standardize_airport_df(df_commercial)
allp    <- standardize_airport_df(df_all_plane)

# Population
population_clean <- population |>
  rename(year = 1, city = 2, population = 3) |>
  mutate(
    year = as.integer(year),
    city = toupper(stri_trans_general(as.character(city), "Latin-ASCII")),
    population = as.numeric(population)
  ) |>
  select(year, city, population)

# Coordinates
coords <- read_excel("data/koordinatlar.xlsx") |>
  rename(airport = 1, lat = 2, lon = 3) |>
  mutate(
    airport_clean = airport_key(airport),
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  )

# ==============================================================================
# PLOT 1: Recovery index for all traffic dimensions, 2019 = 100
# ==============================================================================
index_data <- bind_rows(
  pax     |> group_by(year) |> summarise(value = sum(total, na.rm = TRUE), .groups = "drop") |> mutate(metric = "Passenger"),
  comm    |> group_by(year) |> summarise(value = sum(total, na.rm = TRUE), .groups = "drop") |> mutate(metric = "Commercial flights"),
  allp    |> group_by(year) |> summarise(value = sum(total, na.rm = TRUE), .groups = "drop") |> mutate(metric = "All plane movements"),
  freight |> group_by(year) |> summarise(value = sum(total, na.rm = TRUE), .groups = "drop") |> mutate(metric = "Freight"),
  cargo   |> group_by(year) |> summarise(value = sum(total, na.rm = TRUE), .groups = "drop") |> mutate(metric = "Cargo")
) |>
  group_by(metric) |>
  mutate(index_2019 = value / value[year == 2019] * 100) |>
  ungroup()

p_index <- ggplot(index_data, aes(x = year, y = index_2019, color = metric)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  scale_x_continuous(breaks = 2018:2025) +
  scale_y_continuous(labels = label_number(suffix = "")) +
  labs(
    title = "Recovery Index of Turkish Airport Traffic",
    subtitle = "Each metric is indexed to 2019 = 100. This highlights which activities recovered faster after COVID-19.",
    x = "Year", y = "Index (2019 = 100)", color = "Metric"
  )
print(p_index)
ggsave("results/01_recovery_index_all_metrics.png", p_index, width = 9, height = 5, dpi = 300)

# ==============================================================================
# PLOT 2: International passenger share over time
# ==============================================================================
int_share_year <- pax |>
  group_by(year) |>
  summarise(
    domestic = sum(domestic, na.rm = TRUE),
    international = sum(international, na.rm = TRUE),
    total = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(international_share = international / total)

p_int_share <- ggplot(int_share_year, aes(x = year, y = international_share)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2018:2025) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "International Passenger Share Over Time",
    subtitle = "Shows whether the post-pandemic recovery is driven more by domestic or international traffic.",
    x = "Year", y = "International passenger share"
  )
print(p_int_share)
ggsave("results/02_international_passenger_share.png", p_int_share, width = 8, height = 5, dpi = 300)

# ==============================================================================
# PLOT 3: Top 10 airports in 2025, domestic vs international passengers
# ==============================================================================
top10_pax_2025 <- pax |>
  filter(year == 2025) |>
  slice_max(total, n = 10) |>
  select(airport, domestic, international, total) |>
  pivot_longer(c(domestic, international), names_to = "traffic_type", values_to = "passengers") |>
  mutate(
    traffic_type = recode(traffic_type, domestic = "Domestic", international = "International"),
    airport = reorder(airport, total)
  )

p_top10 <- ggplot(top10_pax_2025, aes(x = airport, y = passengers, fill = traffic_type)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 10 Airports by Passenger Traffic in 2025",
    subtitle = "Stacked bars show the domestic and international composition of major airports.",
    x = NULL, y = "Passengers", fill = "Traffic type"
  )
print(p_top10)
ggsave("results/03_top10_airports_2025_passenger_mix.png", p_top10, width = 9, height = 5.5, dpi = 300)

# ==============================================================================
# PLOT 4: Airport concentration trend: share of Top 3 / Top 5 / Top 10 airports
# ==============================================================================
concentration_data <- pax |>
  group_by(year) |>
  arrange(desc(total), .by_group = TRUE) |>
  mutate(
    rank = row_number(),
    network_total = sum(total, na.rm = TRUE)
  ) |>
  summarise(
    `Top 3 airports` = sum(total[rank <= 3], na.rm = TRUE) / first(network_total),
    `Top 5 airports` = sum(total[rank <= 5], na.rm = TRUE) / first(network_total),
    `Top 10 airports` = sum(total[rank <= 10], na.rm = TRUE) / first(network_total),
    .groups = "drop"
  ) |>
  pivot_longer(-year, names_to = "group", values_to = "share")

p_concentration <- ggplot(concentration_data, aes(x = year, y = share, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  scale_x_continuous(breaks = 2018:2025) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Passenger Traffic Concentration in Major Airports",
    subtitle = "Shows how much of national passenger traffic is handled by the largest airports.",
    x = "Year", y = "Share of national passenger traffic", color = "Airport group"
  )
print(p_concentration)
ggsave("results/04_airport_concentration_trend.png", p_concentration, width = 8.5, height = 5, dpi = 300)

# ==============================================================================
# PLOT 5: Passenger per commercial flight, national trend
# ==============================================================================
passenger_per_flight <- pax |>
  group_by(year) |>
  summarise(passengers = sum(total, na.rm = TRUE), .groups = "drop") |>
  left_join(
    comm |> group_by(year) |> summarise(commercial_flights = sum(total, na.rm = TRUE), .groups = "drop"),
    by = "year"
  ) |>
  mutate(passenger_per_flight = passengers / commercial_flights)

p_ppf <- ggplot(passenger_per_flight, aes(x = year, y = passenger_per_flight)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2018:2025) +
  labs(
    title = "Passenger Intensity per Commercial Flight",
    subtitle = "A proxy for operational intensity: how many passengers are carried per commercial flight movement.",
    x = "Year", y = "Passengers per commercial flight"
  )
print(p_ppf)
ggsave("results/05_passenger_per_commercial_flight_trend.png", p_ppf, width = 8, height = 5, dpi = 300)

# ==============================================================================
# PLOT 6: 2025 coordinate bubble plot
# ==============================================================================
map_data_2025 <- pax |>
  filter(year == 2025, total > 0) |>
  left_join(coords |> select(airport_clean, lat, lon), by = "airport_clean") |>
  filter(!is.na(lat), !is.na(lon)) |>
  mutate(international_share = international / total)

label_data <- map_data_2025 |>
  slice_max(total, n = 8)

p_map <- ggplot(map_data_2025, aes(x = lon, y = lat)) +
  geom_point(aes(size = total, color = international_share), alpha = 0.75) +
  geom_text(
    data = label_data,
    aes(label = airport),
    size = 3,
    vjust = -0.8,
    check_overlap = TRUE
  ) +
  scale_size_continuous(labels = comma) +
  scale_color_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Spatial Distribution of Passenger Traffic in 2025",
    subtitle = "Bubble size shows total passengers; color shows international passenger share.",
    x = "Longitude", y = "Latitude", size = "Passengers", color = "International share"
  )
print(p_map)
ggsave("results/06_2025_airport_bubble_coordinates.png", p_map, width = 8.5, height = 6, dpi = 300)

# ==============================================================================
# PLOT 7: Passenger traffic per capita by city in 2025
# ==============================================================================
city_per_capita_2025 <- pax |>
  filter(year == 2025) |>
  group_by(city) |>
  summarise(passengers = sum(total, na.rm = TRUE), .groups = "drop") |>
  left_join(population_clean |> filter(year == 2025), by = "city") |>
  filter(!is.na(population), population > 0, passengers > 0) |>
  mutate(passengers_per_1000_people = passengers / population * 1000) |>
  slice_max(passengers_per_1000_people, n = 12)

p_percapita <- ggplot(city_per_capita_2025, aes(x = reorder(city, passengers_per_1000_people), y = passengers_per_1000_people)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Passenger Traffic per 1,000 Residents in 2025",
    subtitle = "Highlights tourism and hub cities where airport traffic is high relative to local population.",
    x = NULL, y = "Passengers per 1,000 residents"
  )
print(p_percapita)
ggsave("results/07_passenger_per_capita_2025.png", p_percapita, width = 8, height = 5, dpi = 300)

# ==============================================================================
# PLOT 8: Airports with the largest passenger increase from 2019 to 2025
# ==============================================================================
growth_2019_2025 <- pax |>
  filter(year %in% c(2019, 2025)) |>
  select(airport_clean, airport, year, total) |>
  group_by(airport_clean, year) |>
  summarise(airport = first(airport), total = sum(total, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = year, values_from = total, names_prefix = "y") |>
  mutate(
    y2019 = replace_na(y2019, 0),
    y2025 = replace_na(y2025, 0),
    absolute_change = y2025 - y2019,
    growth_rate = ifelse(y2019 > 0, (y2025 / y2019) - 1, NA_real_)
  ) |>
  filter(y2019 > 10000, y2025 > 0)

top_growth_abs <- growth_2019_2025 |>
  slice_max(absolute_change, n = 12)

p_growth_abs <- ggplot(top_growth_abs, aes(x = reorder(airport, absolute_change), y = absolute_change)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Largest Absolute Passenger Growth: 2019 to 2025",
    subtitle = "Identifies which airports contributed most to post-pandemic passenger expansion.",
    x = NULL, y = "Passenger change, 2025 minus 2019"
  )
print(p_growth_abs)
ggsave("results/08_largest_passenger_growth_2019_2025.png", p_growth_abs, width = 8.5, height = 5, dpi = 300)

# ==============================================================================
# PLOT 9: Cargo specialization in 2025: cargo per passenger
# ==============================================================================
cargo_specialization_2025 <- cargo |>
  filter(year == 2025) |>
  select(airport_clean, airport, cargo_total = total) |>
  left_join(
    pax |> filter(year == 2025) |> select(airport_clean, passenger_total = total),
    by = "airport_clean"
  ) |>
  mutate(cargo_per_1000_passengers = cargo_total / passenger_total * 1000) |>
  filter(!is.na(cargo_per_1000_passengers), passenger_total > 100000, cargo_total > 0) |>
  slice_max(cargo_per_1000_passengers, n = 12)

p_cargo_spec <- ggplot(cargo_specialization_2025, aes(x = reorder(airport, cargo_per_1000_passengers), y = cargo_per_1000_passengers)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Cargo Specialization by Airport in 2025",
    subtitle = "Cargo tonnage per 1,000 passengers. Higher values indicate airports with stronger logistics orientation.",
    x = NULL, y = "Cargo per 1,000 passengers"
  )
print(p_cargo_spec)
ggsave("results/09_cargo_specialization_2025.png", p_cargo_spec, width = 8.5, height = 5, dpi = 300)

# ==============================================================================
# PLOT 10: Earthquake event study, non-commercial flight anomaly in disaster cities
# ==============================================================================
disaster_cities <- c("ADANA", "GAZIANTEP", "KAHRAMANMARAS", "HATAY", "ADIYAMAN", "MALATYA")

emergency_proxy <- allp |>
  select(year, airport_clean, city, all_total = total) |>
  left_join(
    comm |> select(year, airport_clean, commercial_total = total),
    by = c("year", "airport_clean")
  ) |>
  mutate(
    commercial_total = replace_na(commercial_total, 0),
    non_commercial_proxy = all_total - commercial_total
  ) |>
  filter(city %in% disaster_cities)

baseline_emergency <- emergency_proxy |>
  filter(year %in% 2019:2022) |>
  group_by(city) |>
  summarise(baseline_2019_2022 = mean(non_commercial_proxy, na.rm = TRUE), .groups = "drop")

earthquake_anomaly <- emergency_proxy |>
  filter(year == 2023) |>
  group_by(city) |>
  summarise(value_2023 = sum(non_commercial_proxy, na.rm = TRUE), .groups = "drop") |>
  left_join(baseline_emergency, by = "city") |>
  mutate(anomaly = value_2023 - baseline_2019_2022)

p_eq_anomaly <- ggplot(earthquake_anomaly, aes(x = reorder(city, anomaly), y = anomaly)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "2023 Non-Commercial Flight Anomaly in Earthquake-Affected Cities",
    subtitle = "2023 value minus the 2019-2022 average. This is a proxy for extraordinary emergency/logistics activity.",
    x = NULL, y = "Additional non-commercial flight movements"
  )
print(p_eq_anomaly)
ggsave("results/10_earthquake_noncommercial_anomaly_2023.png", p_eq_anomaly, width = 8, height = 5, dpi = 300)

# ==============================================================================
# Optional summary tables for quick reporting
# ==============================================================================
write.csv(int_share_year, "results/table_international_share_by_year.csv", row.names = FALSE)
write.csv(concentration_data, "results/table_concentration_by_year.csv", row.names = FALSE)
write.csv(growth_2019_2025, "results/table_growth_2019_2025.csv", row.names = FALSE)
write.csv(city_per_capita_2025, "results/table_passenger_per_capita_2025.csv", row.names = FALSE)

message("Extra analysis completed. Check the results/ folder for plots and CSV tables.")




