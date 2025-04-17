library(tidyverse)
library(psych)
library(RColorBrewer)
library(countrycode)
library(gridExtra)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(gganimate)

FaC <- read_csv("forest_carbon.csv")

FaC3 <- FaC %>%
  filter(!Country %in% c("Africa", "Americas", "Asia", "Euro Area", "Oceania")) %>%
  select(-c(Country, ISO2, Source, CTS_Code, CTS_Name, CTS_Full_Descriptor)) %>%
  filter(Indicator %in% c("Carbon stocks in forests", "Forest area"))

Forest.long2 <- FaC3 %>%
  filter(Indicator == "Forest area") %>%
  pivot_longer(cols = starts_with("F"),
               names_to = "Year", values_to = "ForestArea") %>%
  mutate(Year = as.numeric(sub("^F", "", Year)),
         Decade = paste0(floor(Year / 10) * 10, "s"))

Carbon.long2 <- FaC3 %>%
  filter(Indicator == "Carbon stocks in forests") %>%
  pivot_longer(cols = starts_with("F"),
               names_to = "Year", values_to = "Carbon") %>%
  mutate(Year = as.numeric(sub("^F", "", Year)))

forest_decade <- Forest.long2 %>%
  group_by(ISO3, Decade) %>%
  filter(ISO3 != "QAT") %>%
  summarize(AvgForestArea = mean(ForestArea, na.rm = TRUE)) %>%
  mutate(iso_a3 = ISO3,
         forestLog = log(AvgForestArea)) %>%
  filter(!is.na(forestLog))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso_a3 = iso_a3_eh)

decade_split <- function(decade_choice){
  forest10 <- forest_decade %>%
    filter(Decade == decade_choice)
  decades <- unique(forest10$Decade)
  
  world_expanded <- world %>%
    slice(rep(1:n(), each = length(decades))) %>%
    mutate(Decade = rep(decades, times = nrow(world)))
  
  map_data <- left_join(world_expanded, forest10, by = c("iso_a3", "Decade"))
  return(map_data)
}

map_data <- decade_split("1990s")

p <- ggplot(map_data) +
  geom_sf(aes(fill = forestLog), color = "gray70", size = 0.1) +
  scale_fill_gradientn(
    colours = c("#8B4513", "#DEB887", "#ADFF2F", "#006400"),
    na.value = "grey90"
  ) +
  labs(
    title = "Log of Forest Area by Country",
    subtitle = "1990s",
    fill = "Log Forest\nArea"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

p

# animated plot
Forest.long2 <- FaC3 %>%
  filter(Indicator == "Forest area") %>%
  pivot_longer(cols = starts_with("F"),
               names_to = "Year", values_to = "ForestArea") %>%
  mutate(
    Year = as.numeric(sub("^F", "", Year)),
    iso_a3 = ISO3,
    Decade = factor(paste0(floor(Year / 10) * 10, "s"))
  )

forest_decade <- Forest.long2 %>%
  filter(ISO3 != "QAT") %>%
  group_by(iso_a3, Decade) %>%
  summarise(
    Avg_Forest_Area = mean(ForestArea, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    forestLog = log(Avg_Forest_Area)
  ) %>%
  filter(!is.na(forestLog))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso_a3 = iso_a3_eh)

decades <- unique(forest_decade$Decade)

world_expanded <- world %>%
  slice(rep(1:n(), each = length(decades))) %>%
  mutate(Decade = rep(decades, times = nrow(world)))

map_data <- left_join(world_expanded, forest_decade, by = c("iso_a3", "Decade"))

p <- ggplot(map_data) +
  geom_sf(aes(fill = forestLog), color = "gray70", size = 0.1) +
  scale_fill_gradientn(
    colours = c("#8B4513", "#DEB887", "#ADFF2F", "#006400"),
    na.value = "grey90"
  ) +
  labs(
    title = "Average Forest Area by Country (Log Scale)",
    subtitle = "Decade: {closest_state}",
    fill = "Log Forest\nArea"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  transition_states(Decade, transition_length = 2, state_length = 1) +
  ease_aes("linear")

animate(p, width = 1000, height = 500, fps = 4)

anim_save("forest_area_by_decade.gif", animation = last_animation())

