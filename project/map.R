# ==============================================================================
# Script: data_visualizer.R
# Purpose: Extra visual analysis for airport traffic project
# IMPORTANT: This version avoids Turkish column names in the code.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------------------------

packages <- c("readxl", "dplyr", "ggplot2", "stringr", "stringi",
              "scales", "maps", "ggrepel", "tibble", "viridis")

missing_packages <- packages[!packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(scales)
library(maps)
library(ggrepel)
library(tibble)
library(viridis)

if (!dir.exists("results")) {
  dir.create("results")
}

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray35", size = 10),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "black")
    )
)

# ------------------------------------------------------------------------------
# 1. Helper functions
# ------------------------------------------------------------------------------

clean_text <- function(x) {
  x |>
    as.character() |>
    str_replace_all("\\(\\*\\)", "") |>
    str_replace_all("\\*", "") |>
    str_squish() |>
    str_to_upper(locale = "tr") |>
    stringi::stri_trans_general("Latin-ASCII")
}

extract_province <- function(airport_name) {
  x <- clean_text(airport_name)
  
  case_when(
    str_detect(x, "^ISTANBUL") ~ "ISTANBUL",
    str_detect(x, "^ANKARA") ~ "ANKARA",
    str_detect(x, "^IZMIR") ~ "IZMIR",
    str_detect(x, "^MUGLA") ~ "MUGLA",
    str_detect(x, "^GAZIPASA ALANYA") ~ "ANTALYA",
    str_detect(x, "^KAPADOKYA") ~ "NEVSEHIR",
    str_detect(x, "^CUKUROVA") ~ "MERSIN",
    str_detect(x, "^ORDU-GIRESUN") ~ "ORDU",
    str_detect(x, "^RIZE-ARTVIN") ~ "RIZE",
    str_detect(x, "^ZAFER") ~ "KUTAHYA",
    str_detect(x, "^CANAKKALE") ~ "CANAKKALE",
    str_detect(x, "^TEKIRDAG") ~ "TEKIRDAG",
    TRUE ~ word(x, 1)
  )
}

# ------------------------------------------------------------------------------
# 2. Coordinates data
# ------------------------------------------------------------------------------

coords <- read_excel("data/koordinatlar.xlsx")

# Make coordinate column names standard by position:
# col 1 = airport name, col 2 = latitude, col 3 = longitude
coords <- coords |>
  rename(
    airport = 1,
    lat = 2,
    lon = 3
  ) |>
  mutate(
    airport_clean = clean_text(airport),
    airport_clean = case_when(
      airport_clean == "HAKKARI YUKSEKOVA" ~ "HAKKARI YUKSEKOVA SELAHADDIN EYYUBI",
      airport_clean == "TEKIRDAG CORLU" ~ "TEKIRDAG CORLU ATATURK",
      airport_clean == "CANAKKALE MERKEZ" ~ "CANAKKALE",
      TRUE ~ airport_clean
    )
  ) |>
  distinct(airport_clean, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# 3. Passenger data for 2025
# ------------------------------------------------------------------------------

# df_passenger columns are:
# 1 = airport, 2 = domestic, 3 = international, 4 = total, 5 = year
# We use column positions to avoid encoding problems with Turkish column names.

passenger_2025 <- df_passenger |>
  filter(year == 2025) |>
  rename(
    airport = 1,
    domestic = 2,
    international = 3,
    total = 4
  ) |>
  mutate(
    airport_clean = clean_text(airport),
    province = extract_province(airport),
    international_share = if_else(total > 0, international / total, NA_real_)
  )

# ------------------------------------------------------------------------------
# 4. Population data for 2025
# ------------------------------------------------------------------------------

# population columns are:
# 1 = year, 2 = province, 3 = total population

population_2025 <- population |>
  rename(
    pop_year = 1,
    province = 2,
    pop_total = 3
  ) |>
  filter(pop_year == 2025) |>
  mutate(province = clean_text(province)) |>
  select(province, pop_total)

# ------------------------------------------------------------------------------
# 5. Final merged dataset for maps
# ------------------------------------------------------------------------------

map_df <- passenger_2025 |>
  left_join(coords |> select(airport_clean, lat, lon), by = "airport_clean") |>
  left_join(population_2025, by = "province") |>
  mutate(
    passenger_per_1000 = if_else(
      !is.na(pop_total) & pop_total > 0,
      total / pop_total * 1000,
      NA_real_
    )
  )

write.csv(map_df, "results/airport_map_population_2025.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 6. Data quality checks
# ------------------------------------------------------------------------------

quality_passenger <- df_passenger |>
  rename(
    airport = 1,
    domestic = 2,
    international = 3,
    total = 4
  ) |>
  mutate(diff = domestic + international - total)

quality_summary <- quality_passenger |>
  summarise(
    n_rows = n(),
    inconsistent_rows = sum(abs(diff) > 0.001, na.rm = TRUE),
    zero_total_rows = sum(total == 0, na.rm = TRUE),
    missing_totals = sum(is.na(total))
  )

join_summary <- map_df |>
  summarise(
    total_airports = n(),
    matched_coordinates = sum(!is.na(lat) & !is.na(lon)),
    matched_population = sum(!is.na(pop_total)),
    matched_both = sum(!is.na(lat) & !is.na(lon) & !is.na(pop_total))
  )

unmatched_rows <- map_df |>
  filter(is.na(lat) | is.na(lon) | is.na(pop_total)) |>
  select(airport, province, lat, lon, pop_total)

write.csv(quality_summary, "results/data_quality_summary.csv", row.names = FALSE)
write.csv(join_summary, "results/join_quality_summary.csv", row.names = FALSE)
write.csv(unmatched_rows, "results/unmatched_airport_rows.csv", row.names = FALSE)

print("Passenger data quality summary:")
print(quality_summary)

print("Coordinate and population matching summary:")
print(join_summary)

print("Unmatched rows, if any:")
print(unmatched_rows)

# ------------------------------------------------------------------------------
# 7. Map background
# ------------------------------------------------------------------------------

turkey_map <- map_data("world", region = "Turkey")

# ------------------------------------------------------------------------------
# PLOT 1: Airport locations and passenger volume
# ------------------------------------------------------------------------------

p_airport_map <- ggplot() +
  geom_polygon(
    data = turkey_map,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray70",
    linewidth = 0.3
  ) +
  geom_point(
    data = map_df |> filter(!is.na(lat), !is.na(lon)),
    aes(x = lon, y = lat, size = total),
    color = "#2b8cbe",
    alpha = 0.75
  ) +
  geom_text_repel(
    data = map_df |> filter(!is.na(lat), !is.na(lon)) |> slice_max(total, n = 10),
    aes(x = lon, y = lat, label = airport),
    size = 3,
    max.overlaps = 20
  ) +
  scale_size_continuous(labels = comma) +
  coord_quickmap(xlim = c(25, 45), ylim = c(35, 43)) +
  labs(
    title = "Airport Locations and Passenger Volume in Turkey (2025)",
    subtitle = "Bubble size represents total passenger traffic.",
    x = NULL,
    y = NULL,
    size = "Passengers"
  )

print(p_airport_map)

ggsave(
  "results/plot_01_airport_locations_passenger_volume_2025.png",
  p_airport_map,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# PLOT 2: Passenger intensity relative to population
# ------------------------------------------------------------------------------

p_percapita_map <- ggplot() +
  geom_polygon(
    data = turkey_map,
    aes(x = long, y = lat, group = group),
    fill = "gray96",
    color = "gray75",
    linewidth = 0.3
  ) +
  geom_point(
    data = map_df |> filter(!is.na(lat), !is.na(lon), !is.na(passenger_per_1000)),
    aes(x = lon, y = lat, color = passenger_per_1000, size = total),
    alpha = 0.85
  ) +
  geom_text_repel(
    data = map_df |>
      filter(!is.na(lat), !is.na(lon), !is.na(passenger_per_1000)) |>
      slice_max(passenger_per_1000, n = 10),
    aes(x = lon, y = lat, label = airport),
    size = 3,
    max.overlaps = 20
  ) +
  scale_color_viridis_c(labels = comma) +
  scale_size_continuous(labels = comma) +
  coord_quickmap(xlim = c(25, 45), ylim = c(35, 43)) +
  labs(
    title = "Airport Traffic Intensity Relative to Population (2025)",
    subtitle = "Color shows passengers per 1,000 residents. Size shows total passengers.",
    x = NULL,
    y = NULL,
    color = "Passengers\nper 1,000",
    size = "Passengers"
  )

print(p_percapita_map)

ggsave(
  "results/plot_02_passenger_per_1000_population_map_2025.png",
  p_percapita_map,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# PLOT 3: Top airports by passenger intensity
# ------------------------------------------------------------------------------

top_intensity <- map_df |>
  filter(!is.na(passenger_per_1000), total > 0) |>
  arrange(desc(passenger_per_1000)) |>
  slice_head(n = 15)

p_top_intensity <- ggplot(
  top_intensity,
  aes(x = reorder(airport, passenger_per_1000), y = passenger_per_1000)
) +
  geom_col(fill = "#31a354") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top Airports by Passenger Intensity (2025)",
    subtitle = "Passenger intensity is calculated as passengers per 1,000 residents.",
    x = NULL,
    y = "Passengers per 1,000 residents"
  )

print(p_top_intensity)

ggsave(
  "results/plot_03_top_airports_passenger_intensity_2025.png",
  p_top_intensity,
  width = 9,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------------------------
# PLOT 4: International passenger share map
# ------------------------------------------------------------------------------

p_international_share <- ggplot() +
  geom_polygon(
    data = turkey_map,
    aes(x = long, y = lat, group = group),
    fill = "gray96",
    color = "gray75",
    linewidth = 0.3
  ) +
  geom_point(
    data = map_df |> filter(!is.na(lat), !is.na(lon), !is.na(international_share)),
    aes(x = lon, y = lat, color = international_share, size = total),
    alpha = 0.85
  ) +
  geom_text_repel(
    data = map_df |>
      filter(!is.na(lat), !is.na(lon), total > 1000000) |>
      slice_max(international_share, n = 10),
    aes(x = lon, y = lat, label = airport),
    size = 3,
    max.overlaps = 20
  ) +
  scale_color_viridis_c(labels = percent) +
  scale_size_continuous(labels = comma) +
  coord_quickmap(xlim = c(25, 45), ylim = c(35, 43)) +
  labs(
    title = "International Passenger Share by Airport (2025)",
    subtitle = "Color shows international passenger share. Size shows total passenger volume.",
    x = NULL,
    y = NULL,
    color = "International\nshare",
    size = "Passengers"
  )

print(p_international_share)

ggsave(
  "results/plot_04_international_share_map_2025.png",
  p_international_share,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# PLOT 5: Growth from 2019 to 2025
# ------------------------------------------------------------------------------

passenger_2019 <- df_passenger |>
  filter(year == 2019) |>
  rename(
    airport = 1,
    total_2019 = 4
  ) |>
  mutate(airport_clean = clean_text(airport)) |>
  select(airport_clean, total_2019)

growth_df <- passenger_2025 |>
  select(airport, airport_clean, total) |>
  left_join(passenger_2019, by = "airport_clean") |>
  mutate(
    growth_pct = if_else(
      !is.na(total_2019) & total_2019 > 0,
      (total - total_2019) / total_2019 * 100,
      NA_real_
    )
  ) |>
  filter(!is.na(growth_pct)) |>
  arrange(desc(growth_pct)) |>
  slice_head(n = 15)

p_growth <- ggplot(
  growth_df,
  aes(x = reorder(airport, growth_pct), y = growth_pct)
) +
  geom_col(fill = "#756bb1") +
  coord_flip() +
  labs(
    title = "Fastest-Growing Airports from 2019 to 2025",
    subtitle = "Growth is calculated using total passenger traffic.",
    x = NULL,
    y = "Growth (%)"
  )

print(p_growth)

ggsave(
  "results/plot_05_fastest_growing_airports_2019_2025.png",
  p_growth,
  width = 9,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------------------------
# PLOT 6: Top airports by total passengers
# ------------------------------------------------------------------------------

top_passenger <- passenger_2025 |>
  filter(total > 0) |>
  arrange(desc(total)) |>
  slice_head(n = 15)

p_top_passenger <- ggplot(
  top_passenger,
  aes(x = reorder(airport, total), y = total)
) +
  geom_col(fill = "#3182bd") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top Airports by Total Passenger Traffic (2025)",
    subtitle = "Total passenger traffic includes domestic and international passengers.",
    x = NULL,
    y = "Total passengers"
  )

print(p_top_passenger)

ggsave(
  "results/plot_06_top_airports_total_passenger_2025.png",
  p_top_passenger,
  width = 9,
  height = 7,
  dpi = 300
)

print("DONE: All extra visualization plots were saved in the results folder.")