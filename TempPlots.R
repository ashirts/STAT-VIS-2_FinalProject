library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(countrycode)
library(rnaturalearth)
library(sf)
library(gganimate)

temperature_unclean <- read_csv("C:\\University\\Stat 5560\\Final Project\\temperature.csv")

# clean the temp data
temperature <- temperature %>%
  dplyr::select(-ISO2, -Indicator, -Unit, -Source, -`CTS Code`,
                -`CTS Name`,-`CTS Full Descriptor`)

sum(complete.cases(temperature))       
nrow(temperature) - sum(complete.cases(temperature))

temperature <- drop_na(temperature)

# puts temp data in long form
temperature_long <- temperature %>%
  pivot_longer(
    cols = `1961`:`2023`,
    names_to = "Year",
    values_to = "Temperature_Change_Celsius"
  ) %>%
  mutate(Year = as.integer(Year)) %>%
  drop_na(Temperature_Change_Celsius)

# adds a continent vector
temperature_long <- temperature_long %>%
  mutate(Continent = countrycode(sourcevar = Country,
                                 origin = "country.name",
                                 destination = "continent"))
table(temperature_long$Continent, useNA = "ifany")

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
  theme(axis.text.y = element_text(size = 3.8))

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

p <- ggplot(df, aes(x = Year, y = Temperature_Change_Celsius, color = Country)) +
  geom_line(linewidth = 0.7) +
  ylim(global_min, global_max) +
  facet_wrap(~Country, scales = "fixed") +
  labs(title = paste("Temperature Change Over Time in", region_name),
       y = "Temperature Change (°C)", x = "Year") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 7),
        plot.title = element_text(face = "bold", size = 14))
p


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

animate(p, width = 1000, height = 500, fps = 4)

anim_save("temperature_change_by_decade.gif", animation = last_animation())
