library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(countrycode)
library(rnaturalearth)
library(sf)
library(gganimate)
library(zoo)

temperature <- read_csv("data/temperature.csv") %>%
  select(-ISO2, -Indicator, -Unit, -Source, -`CTS Code`, -`CTS Name`, -`CTS Full Descriptor`)

temperature <- temperature %>%
  rowwise() %>%
  filter(any(!is.na(c_across(`1961`:`2023`)))) %>%
  ungroup()

temperature_long <- temperature %>%
  pivot_longer(cols = `1961`:`2023`, names_to = "Year", values_to = "Temperature_Change_Celsius") %>%
  mutate(Year = as.integer(Year)) %>%
  arrange(Country, Year)

temperature_long <- temperature_long %>%
  mutate(Continent = countrycode(Country, "country.name", "continent"))

continent_trend <- temperature_long %>%
  group_by(Continent, Year) %>%
  summarise(continent_mean = mean(Temperature_Change_Celsius, na.rm = TRUE), .groups = "drop") %>%
  arrange(Continent, Year) %>%
  group_by(Continent) %>%
  mutate(continent_change = continent_mean - lag(continent_mean)) %>%
  ungroup()

first_known <- temperature_long %>%
  filter(!is.na(Temperature_Change_Celsius)) %>%
  group_by(Country) %>%
  slice_min(Year, n = 1, with_ties = FALSE) %>%
  select(Country, first_year = Year, first_temp = Temperature_Change_Celsius)

valid_countries <- first_known$Country
temperature_long <- temperature_long %>%
  filter(Country %in% valid_countries)

temperature_long <- temperature_long %>%
  left_join(continent_trend %>% select(Continent, Year, continent_change), by = c("Continent", "Year")) %>%
  left_join(first_known, by = "Country") %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(
    Temperature_Change_Celsius = {
      is_missing_before <- is.na(Temperature_Change_Celsius) & Year < first_year
      rev_change <- rev(replace_na(continent_change, 0))
      backfill_values <- first_temp[1] - cumsum(rev_change)
      ifelse(is_missing_before, rev(backfill_values[seq_len(sum(is_missing_before))]), Temperature_Change_Celsius)
    }
  ) %>%
  ungroup()

continent_mins <- temperature_long %>%
  filter(!is.na(Temperature_Change_Celsius)) %>%
  group_by(Continent) %>%
  summarise(continent_min = min(Temperature_Change_Celsius, na.rm = TRUE), .groups = "drop")

temperature_long <- temperature_long %>%
  left_join(continent_mins, by = "Continent") %>%
  mutate(
    Temperature_Change_Celsius = pmax(Temperature_Change_Celsius, continent_min)
  ) %>%
  select(-continent_min)

temperature_long <- temperature_long %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(Temperature_Change_Celsius = na.approx(Temperature_Change_Celsius, x = Year, na.rm = FALSE)) %>%
  fill(Temperature_Change_Celsius, .direction = "downup") %>%
  ungroup()

africa   <- filter(temperature_long, Continent == "Africa")
americas <- filter(temperature_long, Continent == "Americas")
asia     <- filter(temperature_long, Continent == "Asia")
europe   <- filter(temperature_long, Continent == "Europe")
oceania  <- filter(temperature_long, Continent == "Oceania")

# line plot
temperature_long %>%
  group_by(Year) %>%
  summarise(Global_Avg_Change = mean(Temperature_Change_Celsius)) %>%
  ggplot(aes(x = Year, y = Global_Avg_Change)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Global Average Temperature Change Over Time",
       y = "Temperature Change (°C)", x = "Year") +
  theme_minimal()

# heatmap
temperature_long %>%
  mutate(Country = fct_rev(factor(Country, levels = sort(unique(Country))))) %>%
  ggplot(aes(x = Year, y = Country, fill = Temperature_Change_Celsius)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Temperature Change Heatmap by Country and Year",
       x = "Year", y = "Country", fill = "Temp Change (°C)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 2.5))

# heatmaps by continent
global_min <- min(temperature_long$Temperature_Change_Celsius, na.rm = TRUE)
global_max <- max(temperature_long$Temperature_Change_Celsius, na.rm = TRUE)

heatmap_continent <- function(df, region_name, global_min, global_max) {
  df %>%
    filter(Continent == region_name) %>%
    mutate(Country = fct_rev(factor(Country, levels = sort(unique(Country))))) %>%
    ggplot(aes(x = Year, y = Country, fill = Temperature_Change_Celsius)) +
    geom_tile() +
    scale_fill_viridis_c(option = "C", limits = c(global_min, global_max)) +
    labs(title = paste("Temperature Change in", region_name, "(1961–2023)"),
         x = "Year", y = "Country", fill = "Temp Change (°C)") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 5),
          plot.title = element_text(face = "bold", size = 14))
}

heatmap_continent(temperature_long, "Africa", global_min, global_max)
heatmap_continent(temperature_long, "Americas", global_min, global_max)
heatmap_continent(temperature_long, "Asia", global_min, global_max)
heatmap_continent(temperature_long, "Europe", global_min, global_max)
heatmap_continent(temperature_long, "Oceania", global_min, global_max)

# faceted line plots by region
region_lists <- list(
  Africa = africa,
  Americas = americas,
  Asia = asia,
  Europe = europe,
  Oceania = oceania
)

for (region_name in names(region_lists)) {
  df <- region_lists[[region_name]]
  
  p <- ggplot(df, aes(x = Year, y = Temperature_Change_Celsius, color = Country)) +
    geom_line(linewidth = 0.7) +
    ylim(global_min, global_max) +
    facet_wrap(~Country, scales = "fixed") +
    labs(
      title = paste("Temperature Change Over Time in", region_name),
      y = "Temperature Change (°C)", x = "Year"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 7),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  print(p)
  ggsave(
    filename = paste0("temperature_plot_", region_name, ".png"),
    plot = p,
    width = 12, height = 8
  )
}

# boxplots for global temp change by decade 
temperature_long %>%
  mutate(Decade = paste0(floor(Year / 10) * 10, "s")) %>%
  ggplot(aes(x = Decade, y = Temperature_Change_Celsius)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribution of Temperature Change by Decade",
       y = "Temperature Change (°C)", x = "Decade") +
  theme_minimal()

#animated map of temperature change per country by decade
temperature_long <- temperature_long %>%
  mutate(
    iso_a3 = ISO3,
    Decade = factor(paste0(floor(Year / 10) * 10, "s"))
  )

temperature_decade <- temperature_long %>%
  group_by(iso_a3, Decade) %>%
  summarise(Avg_Temp_Change = mean(Temperature_Change_Celsius, na.rm = TRUE), .groups = "drop")

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso_a3 = iso_a3_eh)

decades <- unique(temperature_decade$Decade)

world_expanded <- world %>%
  slice(rep(1:n(), each = length(decades))) %>%
  mutate(Decade = rep(decades, times = nrow(world)))

map_data <- left_join(world_expanded, temperature_decade, by = c("iso_a3", "Decade"))

p <- ggplot(map_data) +
  geom_sf(aes(fill = Avg_Temp_Change), color = "gray70", size = 0.1) +
  scale_fill_viridis_c(option = "C", na.value = "grey90") +
  labs(
    title = "Average Temperature Change by Country",
    subtitle = "Decade: {closest_state}",
    fill = "Temp Change (°C)"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  transition_states(Decade, transition_length = 2, state_length = 1) +
  ease_aes("linear")

anim <- animate(p, width = 1000, height = 500, fps = 4, renderer = gifski_renderer())
anim_save("temperature_change_by_decade.gif", animation = anim)


