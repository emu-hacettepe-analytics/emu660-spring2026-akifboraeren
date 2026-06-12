library(dplyr)

# ==============================================================================
# 1. Combine Passenger Data (2018-2025)
# ==============================================================================
df_passenger <- bind_rows(
  mutate(data_passenger_2018, year = 2018),
  mutate(data_passenger_2019, year = 2019),
  mutate(data_passenger_2020, year = 2020),
  mutate(data_passenger_2021, year = 2021),
  mutate(data_passenger_2022, year = 2022),
  mutate(data_passenger_2023, year = 2023),
  mutate(data_passenger_2024, year = 2024),
  mutate(data_passenger_2025, year = 2025)
)

# ==============================================================================
# 2. Combine Cargo Data (2018-2025)
# ==============================================================================
df_cargo <- bind_rows(
  mutate(data_cargo_2018, year = 2018),
  mutate(data_cargo_2019, year = 2019),
  mutate(data_cargo_2020, year = 2020),
  mutate(data_cargo_2021, year = 2021),
  mutate(data_cargo_2022, year = 2022),
  mutate(data_cargo_2023, year = 2023),
  mutate(data_cargo_2024, year = 2024),
  mutate(data_cargo_2025, year = 2025)
)

# ==============================================================================
# 3. Combine Freight Data (2018-2025)
# ==============================================================================
df_freight <- bind_rows(
  mutate(data_freight_2018, year = 2018),
  mutate(data_freight_2019, year = 2019),
  mutate(data_freight_2020, year = 2020),
  mutate(data_freight_2021, year = 2021),
  mutate(data_freight_2022, year = 2022),
  mutate(data_freight_2023, year = 2023),
  mutate(data_freight_2024, year = 2024),
  mutate(data_freight_2025, year = 2025)
)

# ==============================================================================
# 4. Combine Commercial Flight Data (2018-2025)
# ==============================================================================
df_commercial <- bind_rows(
  mutate(data_commercial_2018, year = 2018),
  mutate(data_commercial_2019, year = 2019),
  mutate(data_commercial_2020, year = 2020),
  mutate(data_commercial_2021, year = 2021),
  mutate(data_commercial_2022, year = 2022),
  mutate(data_commercial_2023, year = 2023),
  mutate(data_commercial_2024, year = 2024),
  mutate(data_commercial_2025, year = 2025)
)

# ==============================================================================
# 5. Combine All Plane Movements Data (2018-2025)
# ==============================================================================
df_all_plane <- bind_rows(
  mutate(data_all_plane_2018, year = 2018),
  mutate(data_all_plane_2019, year = 2019),
  mutate(data_all_plane_2020, year = 2020),
  mutate(data_all_plane_2021, year = 2021),
  mutate(data_all_plane_2022, year = 2022),
  mutate(data_all_plane_2023, year = 2023),
  mutate(data_all_plane_2024, year = 2024),
  mutate(data_all_plane_2025, year = 2025)
)