# Load required packages
library(readxl)
library(dplyr)
library(tidyr)

# ==============================================================================
# 2018 Data (Note: 2018 uses 'commercial_plane' instead of 'commercial')
# ==============================================================================
data_cargo_2018      <- read_excel("data/data_2018.xlsx", sheet = "cargo") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_freight_2018    <- read_excel("data/data_2018.xlsx", sheet = "freight") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_commercial_2018 <- read_excel("data/data_2018.xlsx", sheet = "commercial") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_passenger_2018  <- read_excel("data/data_2018.xlsx", sheet = "passenger") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_all_plane_2018  <- read_excel("data/data_2018.xlsx", sheet = "all_plane") |> mutate(across(where(is.numeric), ~replace_na(., 0)))

# ==============================================================================
# 2019 Data
# ==============================================================================
data_cargo_2019      <- read_excel("data/data_2019.xlsx", sheet = "cargo") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_freight_2019    <- read_excel("data/data_2019.xlsx", sheet = "freight") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_commercial_2019 <- read_excel("data/data_2019.xlsx", sheet = "commercial") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_passenger_2019  <- read_excel("data/data_2019.xlsx", sheet = "passenger") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_all_plane_2019  <- read_excel("data/data_2019.xlsx", sheet = "all_plane") |> mutate(across(where(is.numeric), ~replace_na(., 0)))

# ==============================================================================
# 2020 Data
# ==============================================================================
data_cargo_2020      <- read_excel("data/data_2020.xlsx", sheet = "cargo") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_freight_2020    <- read_excel("data/data_2020.xlsx", sheet = "freight") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_commercial_2020 <- read_excel("data/data_2020.xlsx", sheet = "commercial") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_passenger_2020  <- read_excel("data/data_2020.xlsx", sheet = "passenger") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_all_plane_2020  <- read_excel("data/data_2020.xlsx", sheet = "all_plane") |> mutate(across(where(is.numeric), ~replace_na(., 0)))

# ==============================================================================
# 2021 Data
# ==============================================================================
data_cargo_2021      <- read_excel("data/data_2021.xlsx", sheet = "cargo") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_freight_2021    <- read_excel("data/data_2021.xlsx", sheet = "freight") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_commercial_2021 <- read_excel("data/data_2021.xlsx", sheet = "commercial") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_passenger_2021  <- read_excel("data/data_2021.xlsx", sheet = "passenger") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_all_plane_2021  <- read_excel("data/data_2021.xlsx", sheet = "all_plane") |> mutate(across(where(is.numeric), ~replace_na(., 0)))

# ==============================================================================
# 2022 Data
# ==============================================================================
data_cargo_2022      <- read_excel("data/data_2022.xlsx", sheet = "cargo") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_freight_2022    <- read_excel("data/data_2022.xlsx", sheet = "freight") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_commercial_2022 <- read_excel("data/data_2022.xlsx", sheet = "commercial") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_passenger_2022  <- read_excel("data/data_2022.xlsx", sheet = "passenger") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_all_plane_2022  <- read_excel("data/data_2022.xlsx", sheet = "all_plane") |> mutate(across(where(is.numeric), ~replace_na(., 0)))

# ==============================================================================
# 2023 Data
# ==============================================================================
data_cargo_2023      <- read_excel("data/data_2023.xlsx", sheet = "cargo") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_freight_2023    <- read_excel("data/data_2023.xlsx", sheet = "freight") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_commercial_2023 <- read_excel("data/data_2023.xlsx", sheet = "commercial") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_passenger_2023  <- read_excel("data/data_2023.xlsx", sheet = "passenger") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_all_plane_2023  <- read_excel("data/data_2023.xlsx", sheet = "all_plane") |> mutate(across(where(is.numeric), ~replace_na(., 0)))

# ==============================================================================
# 2024 Data
# ==============================================================================
data_cargo_2024      <- read_excel("data/data_2024.xlsx", sheet = "cargo") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_freight_2024    <- read_excel("data/data_2024.xlsx", sheet = "freight") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_commercial_2024 <- read_excel("data/data_2024.xlsx", sheet = "commercial") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_passenger_2024  <- read_excel("data/data_2024.xlsx", sheet = "passenger") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_all_plane_2024  <- read_excel("data/data_2024.xlsx", sheet = "all_plane") |> mutate(across(where(is.numeric), ~replace_na(., 0)))

# ==============================================================================
# 2025 Data
# ==============================================================================
data_cargo_2025      <- read_excel("data/data_2025.xlsx", sheet = "cargo") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_freight_2025    <- read_excel("data/data_2025.xlsx", sheet = "freight") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_commercial_2025 <- read_excel("data/data_2025.xlsx", sheet = "commercial") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_passenger_2025  <- read_excel("data/data_2025.xlsx", sheet = "passenger") |> mutate(across(where(is.numeric), ~replace_na(., 0)))
data_all_plane_2025  <- read_excel("data/data_2025.xlsx", sheet = "all_plane") |> mutate(across(where(is.numeric), ~replace_na(., 0)))

# ==============================================================================
# Population
# ==============================================================================
population      <- read_excel("data/population.xlsx", sheet = "population")