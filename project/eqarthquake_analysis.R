# ==============================================================================
# Script: earthquake_analysis.R
# Purpose: Granular Analysis of the 2023 Earthquake Disaster Relief Air Traffic
#          (Both Receiver/Disaster Zone and Dispatcher/Major Hubs)
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(stringi)

# Profesyonel Tema
theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 10)),
                  strip.text = element_text(face = "bold", size = 12, color = "white"),
                  strip.background = element_rect(fill = "#2c3e50", color = NA),
                  panel.grid.minor = element_blank()))

# ------------------------------------------------------------------------------
# VER?? HAZIRLI??I ORTAK FONKS??YON
# ------------------------------------------------------------------------------
clean_city_names <- function(df) {
  df |>
    rename(airport = 1) |>
    mutate(City = sub(" .*$", "", toupper(airport))) |>
    mutate(City = stri_trans_general(City, "Latin-ASCII")) |>
    mutate(City = toupper(City))
}

# ------------------------------------------------------------------------------
# B??L??M 1: AFET B??LGES?? (THE RECEIVERS)
# ------------------------------------------------------------------------------
target_cities <- c("ADANA", "GAZIANTEP", "KAHRAMANMARAS", "HATAY", "ADIYAMAN", "MALATYA")

eq_comm <- df_commercial |> clean_city_names() |> filter(City %in% target_cities) |>
  group_by(year, City) |> summarise(Comm_Flights = sum(across(where(is.numeric) & !matches("year")), na.rm = TRUE), .groups = "drop")

eq_all <- df_all_plane |> clean_city_names() |> filter(City %in% target_cities) |>
  group_by(year, City) |> summarise(All_Flights = sum(across(where(is.numeric) & !matches("year")), na.rm = TRUE), .groups = "drop")

eq_data <- eq_all |>
  left_join(eq_comm, by = c("year", "City")) |>
  mutate(Comm_Flights = replace_na(Comm_Flights, 0),
         Emergency_Flights = All_Flights - Comm_Flights) |>
  filter(year >= 2019 & year <= 2025)

p_earthquake <- ggplot(eq_data, aes(x = year, y = Emergency_Flights, fill = City)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~City, scales = "free_y", ncol = 3) +
  scale_fill_viridis_d(option = "inferno", begin = 0.3, end = 0.9) +
  scale_x_continuous(breaks = c(2019, 2021, 2023, 2025)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "1. The Receivers: Relief Flights into the Disaster Zone (2023)",
       subtitle = "Spikes indicate the influx of military, cargo, and emergency flights to affected cities.",
       x = "Year", y = "Emergency / Relief Flights")

# ------------------------------------------------------------------------------
# B??L??M 2: B??Y??K??EH??RLER (THE DISPATCHERS)
# ------------------------------------------------------------------------------
# ??stanbul kelimesi hem IST, hem SAW, hem de ISL'yi otomatik toplayacak.
major_cities <- c("ISTANBUL", "ANKARA", "IZMIR", "BURSA", "ANTALYA")

major_comm <- df_commercial |> clean_city_names() |> filter(City %in% major_cities) |>
  group_by(year, City) |> summarise(Comm_Flights = sum(across(where(is.numeric) & !matches("year")), na.rm = TRUE), .groups = "drop")

major_all <- df_all_plane |> clean_city_names() |> filter(City %in% major_cities) |>
  group_by(year, City) |> summarise(All_Flights = sum(across(where(is.numeric) & !matches("year")), na.rm = TRUE), .groups = "drop")

major_data <- major_all |>
  left_join(major_comm, by = c("year", "City")) |>
  mutate(Comm_Flights = replace_na(Comm_Flights, 0),
         Dispatch_Flights = All_Flights - Comm_Flights) |>
  filter(year >= 2019 & year <= 2025)

p_major <- ggplot(major_data, aes(x = year, y = Dispatch_Flights, fill = City)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~City, scales = "free_y", ncol = 3) +
  # Dispatcher'lar?? afet b??lgesinden ay??rmak i??in Mavi/Ye??il tonlar?? kullan??yoruz
  scale_fill_viridis_d(option = "mako", begin = 0.4, end = 0.8) +
  scale_x_continuous(breaks = c(2019, 2021, 2023, 2025)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "2. The Dispatchers: Relief Flights Originating from Major Hubs",
       subtitle = " ",
       x = "Year", y = "Non-Commercial (Dispatch) Flights")

# Grafikleri S??rayla Bast??r
print(p_earthquake)
print(p_major)