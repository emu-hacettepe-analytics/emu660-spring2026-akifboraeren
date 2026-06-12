# ==============================================================================
# Script: clear_pandemic_visuals.R
# Purpose: Clear, high-impact COVID-19 visualizations for the entire network
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales) # Y??zdelik dilimler ve temiz eksenler i??in

# Akademik raporlara (LaTeX/Quarto) uygun ultra temiz tema
theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold", size = 15),
                  plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 10)),
                  legend.position = "top",
                  legend.title = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text = element_text(color = "black")))

# ==============================================================================
# PLOT 1: THE DIVERGENCE (Yolcu vs. Kargo Makas??)
# ==============================================================================

# T??m T??rkiye toplamlar??n?? al??yoruz
nat_pass <- df_passenger |> group_by(year) |> 
  summarise(Total_Pass = sum(across(where(is.numeric) & !matches("year")), na.rm = TRUE))

nat_cargo <- df_cargo |> group_by(year) |> 
  summarise(Total_Cargo = sum(across(where(is.numeric) & !matches("year")), na.rm = TRUE))

# 2019 y??l??n?? "100" baz alarak endeksliyoruz
plot1_data <- nat_pass |>
  left_join(nat_cargo, by = "year") |>
  mutate(
    Pass_Index = (Total_Pass / Total_Pass[year == 2019]) * 100,
    Cargo_Index = (Total_Cargo / Total_Cargo[year == 2019]) * 100
  ) |>
  select(year, Pass_Index, Cargo_Index) |>
  pivot_longer(cols = c(Pass_Index, Cargo_Index), names_to = "Metric", values_to = "Index") |>
  # ??simleri grafikte g??zel g??r??ns??n diye de??i??tiriyoruz
  mutate(Metric = ifelse(Metric == "Pass_Index", "Passenger Volume", "Cargo Tonnage"))

p1 <- ggplot(plot1_data, aes(x = year, y = Index, color = Metric)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4, shape = 21, fill = "white", stroke = 1.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  annotate("text", x = 2018.5, y = 105, label = "2019 Baseline (100)", color = "gray50", fontface = "bold") +
  scale_color_manual(values = c("Passenger Volume" = "#2171b5", "Cargo Tonnage" = "#d94801")) +
  scale_x_continuous(breaks = 2018:2025) +
  labs(title = "System Resilience: Passenger Collapse vs. Cargo Stability",
       subtitle = "National aviation indexed to 2019. Shows the extreme divergence during the 2020 COVID-19 shock.",
       x = "Year", y = "Performance Index (2019 = 100)")

# ==============================================================================
# PLOT 2: THE CAPACITY SHIFT (%100 Stacked Bar Chart)
# ==============================================================================

# Toplam Ticari ve Toplam (T??m) u??u??lar?? al??yoruz
nat_comm <- df_commercial |> group_by(year) |> 
  summarise(Comm_Flights = sum(across(where(is.numeric) & !matches("year")), na.rm = TRUE))

nat_all <- df_all_plane |> group_by(year) |> 
  summarise(All_Flights = sum(across(where(is.numeric) & !matches("year")), na.rm = TRUE))

plot2_data <- nat_comm |>
  left_join(nat_all, by = "year") |>
  # Ticari olmayan u??u??lar?? (Kargo, Askeri, ??zel jet vb.) farktan buluyoruz
  mutate(Non_Comm_Flights = All_Flights - Comm_Flights) |>
  select(year, Comm_Flights, Non_Comm_Flights) |>
  pivot_longer(cols = c(Comm_Flights, Non_Comm_Flights), names_to = "Type", values_to = "Count") |>
  mutate(Type = factor(Type, levels = c("Non_Comm_Flights", "Comm_Flights"), 
                       labels = c("Non-Commercial (Cargo, Military, Relief)", "Commercial Passenger Flights")))

p2 <- ggplot(plot2_data, aes(x = factor(year), y = Count, fill = Type)) +
  # position = "fill" komutu bunu %100'e tamamlanan oransal bir grafi??e ??evirir
  geom_col(position = "fill", width = 0.7, color = "white") +
  scale_fill_manual(values = c("Commercial Passenger Flights" = "#6baed6", 
                               "Non-Commercial (Cargo, Military, Relief)" = "#cb181d")) +
  # Y eksenini y??zdelik dilim olarak g??sterir
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Operational Shift: The Proportion of Air Traffic",
       subtitle = "In 2020, the share of non-commercial flights quadrupled as the system pivoted to logistics.",
       x = "Year", y = "Percentage of Total Flight Movements")

# Grafikleri Ekrana Bast??r
print(p1)
print(p2)