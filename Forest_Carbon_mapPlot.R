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


########################## READ IN THE DATA ###################################
FaC <- read_csv("data/forest_carbon.csv")
land_area <- read_csv("data/API_AG.LND.TOTL.K2_DS2_en_csv_v2_19570.csv", skip =3)

############################ Data Manipulation including pivot ################

land_area |> 
  dplyr::select(-c("1960", "2023", "2024", "...70")) |> 
  tidyr::pivot_longer(
    cols = -c(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`), 
    values_to = "land_area", 
    names_to = "year"
  ) |> 
  mutate(
    year = as.numeric(year)
  ) |> select(`Country Code`, year, land_area) -> land_area

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
###############################################################################

Forest.long2 |> 
  inner_join(land_area, by = c("ISO3" = "Country Code", 
                               "Year" = "year" )) |>
  mutate(
    prop_forest = ForestArea * 1000 / (land_area * 100)
  ) -> Forest.long2

forest_decade <- Forest.long2 %>%
  group_by(ISO3, Decade) %>%
  filter(ISO3 != "QAT") %>%
  summarize(AvgForestArea = mean(ForestArea, na.rm = TRUE), 
            prop_forest = mean(prop_forest, na.rm = TRUE)) %>%
  mutate(iso_a3 = ISO3,
         forestLog = log(AvgForestArea)) %>%
  filter(!is.na(prop_forest))

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
  ) |> 
  inner_join(land_area, by = c("ISO3" = "Country Code", 
                               "Year" = "year" )) |>
  mutate(
    prop_forest = ForestArea * 1000 / (land_area * 100)
  )

forest_decade <- Forest.long2 %>%
  filter(ISO3 != "QAT") %>%
  group_by(iso_a3, Decade) %>%
  summarise(
    prop_forest = mean(prop_forest, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(prop_forest))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso_a3 = iso_a3_eh)

decades <- unique(forest_decade$Decade)

world_expanded <- world %>%
  slice(rep(1:n(), each = length(decades))) %>%
  mutate(Decade = rep(decades, times = nrow(world)))

map_data <- left_join(world_expanded, forest_decade, by = c("iso_a3", "Decade"))

p <- ggplot(map_data) +
  geom_sf(aes(fill = prop_forest), color = "gray70", size = 0.1) +
  scale_fill_gradientn(
    colours = c("#8B4513", "#DEB887", "#ADFF2F", "#006400"),
    na.value = "grey90"
  ) +
  labs(
    title = "Proportion of Forest Area to Land Area (Hectacres)",
    subtitle = "Decade: {closest_state}",
    fill = "Proportion of Forest\nArea"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  transition_states(Decade, transition_length = 2, state_length = 1) +
  ease_aes("linear")

anim <- animate(p, width = 1000, height = 500, fps = 4, renderer = gifski_renderer())
anim_save("forest_area_by_decade.gif", animation = anim)

