# ==============================================================================
# STEP 1: Data Preparation & Feature Engineering
# ==============================================================================

pass_2025 <- df_passenger |>
  filter(year == 2025) |>
  rename(airport = 1) |>
  mutate(total_passenger = rowSums(across(where(is.numeric) & !matches("year")))) |>
  select(airport, total_passenger)

cargo_2025 <- df_cargo |>
  filter(year == 2025) |>
  rename(airport = 1) |>
  mutate(total_cargo = rowSums(across(where(is.numeric) & !matches("year")))) |>
  select(airport, total_cargo)

df_kmeans_prep <- pass_2025 |>
  left_join(cargo_2025, by = "airport") |>
  mutate(city_match = word(toupper(airport), 1)) |>
  left_join(
    population |> 
      select(city = 1, pop_2025 = contains("2025")) |> 
      mutate(city_match = toupper(city)),
    by = "city_match"
  ) |>
  drop_na(pop_2025) |>
  filter(total_passenger > 0)

df_model_data <- df_kmeans_prep |>
  mutate(
    Aviation_Impact = total_passenger / pop_2025,
    Cargo_Ratio = total_cargo / total_passenger
  ) |>
  filter(is.finite(Aviation_Impact) & is.finite(Cargo_Ratio)) |>
  select(airport, total_passenger, total_cargo, Aviation_Impact, Cargo_Ratio)

# ==============================================================================
# STEP 2: Feature Scaling & K-Means Execution
# ==============================================================================

df_features <- df_model_data |> select(-airport)
df_scaled <- scale(df_features)

set.seed(660) 

kmeans_result <- kmeans(df_scaled, centers = 4, nstart = 25)

df_model_data <- df_model_data |>
  mutate(Cluster = as.factor(kmeans_result$cluster))

table(df_model_data$Cluster)