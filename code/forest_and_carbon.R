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

FaC <- rcountrycodeFaC <- read_csv("data/forest_carbon.csv")

FaC3 <- FaC %>%
  filter(!Country %in% c("Africa", "Americas", "Asia", "Euro Area", "Oceania")) %>%
  select(-c(Country, ISO2, Source, CTS_Code, CTS_Name, CTS_Full_Descriptor)) %>%
  filter(Indicator %in% c("Carbon stocks in forests", "Forest area"))

Forest.long2 <- FaC3 %>%
  filter(Indicator %in% c("Forest area")) %>%
  pivot_longer(cols = starts_with("F"),
               names_to = "Year", values_to = "ForestArea") %>%
  mutate(Year = as.numeric(sub("^F", "", Year))) %>%
  mutate(Decade = paste0(floor(Year / 10) * 10, "s"))

Carbon.long2 <- FaC3 %>%
  filter(Indicator %in% c("Carbon stocks in forests")) %>%
  pivot_longer(cols = starts_with("F"),
               names_to = "Year", values_to = "Carbon") %>%
  mutate(Year = sub("^F", "", Year))

forest_decade <- Forest.long2 %>%
  group_by(ISO3, Decade) %>%
  filter(ISO3 != "QAT") %>%
  summarize(AvgForestArea = mean(ForestArea)) %>%
  mutate(iso_a3 = ISO3,
         forestLog = log(AvgForestArea)) %>%
  filter(!is.na(forestLog))
  


world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso_a3 = iso_a3_eh)

decade_split <- function(decade_choice){
  forest10 <- forest_decade %>%
    filter(Decade == decade_choice)
  print(head(forest10))
  decades <- unique(forest10$Decade)
  print(head(decades))
  world_expanded <- world %>%
    slice(rep(1:n(), each = length(decades))) %>%
    mutate(Decade = rep(decades, times = nrow(world)))
  map_data <- left_join(world_expanded, forest10, by = c("iso_a3"))
  
}

decade_split("1990s")

p <- ggplot(map_data) +
  geom_sf(aes(fill = forestLog), color = "gray70", size = 0.1) +
  scale_fill_viridis_c(option = "E", na.value = "grey90", direction = -1) +
  labs(
    title = "Log of forest area",
    subtitle = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "right")
  
p
