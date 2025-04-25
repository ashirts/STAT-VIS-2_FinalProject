library(tidyverse)
library(psych)
library(RColorBrewer)
library(countrycode)
library(gridExtra)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

FaC <- rcountrycodeFaC <- read_csv("data/forest_carbon.csv")
NDF <- read_csv("data/disasters.csv")

NDF2 <- NDF %>%
  mutate(Indicator = sub(".*: ", "", Indicator)) %>% # Clean Indicator 
  rename(CTS_Code = `CTS Code`, CTS_Name = `CTS Name`, 
         CTS_Full_Descriptor = `CTS Full Descriptor`) |>
  select(-c(ISO2, Country, ObjectId, Unit, Source, CTS_Code, CTS_Name, CTS_Full_Descriptor)) %>% # Remove unneeded columns
  mutate(across(everything(), replace_na, 0)) # Replace NAs with 0

NDF3 <- NDF %>%
  mutate(Indicator = sub(".*: ", "", Indicator)) %>%
  rename(CTS_Code = `CTS Code`, CTS_Name = `CTS Name`, 
         CTS_Full_Descriptor = `CTS Full Descriptor`) |>
  select(-c(ISO2, ObjectId, Unit, Source, CTS_Code, CTS_Name, CTS_Full_Descriptor)) %>% # Remove unneeded columns
  mutate(across(everything(), replace_na, 0)) %>%
  mutate(continent = countrycode(Country, "country.name.en", "continent")
         ) %>%
  rename_with(~ gsub("^F", "", .x), starts_with("F")) %>% # Replace NAs with 0
  select(continent, everything()) %>%
  drop_na()
  


# Split disaster types into their own datasets
disaster_types <- c("Drought", "Extreme temperature", "Flood", "Landslide", "Storm", "TOTAL", "Wildfire")

disasters <- NDF2 %>% 
  filter(Indicator %in% disaster_types) %>%
  select(-Indicator) %>%
  split(NDF2$Indicator)

disasters2 <- NDF3 %>%
  filter(Indicator %in% disaster_types) %>%
  select(-Indicator) %>%
  split(NDF3$Indicator)

Droughts <- disasters[["Drought"]]
HotTemps <- disasters[["Extreme temperature"]]
Floods <- disasters[["Flood"]]
Landslides <- disasters[["Landslide"]]
Storms <- disasters[["Storm"]]
TotalDisasters <- disasters[["TOTAL"]]
Wildfires <- disasters[["Wildfire"]]

long_disasters <- function(input_table) {
  input_table %>%
  pivot_longer(
    cols = `1980`:`2022`,  # selects columns like "1990", "1991", etc.
    names_to = "year",
    values_to = "disasters"
  ) %>%
  mutate(year = as.integer(year)) %>%
  group_by(continent, year) %>%
  summarize(total_disasters = sum(disasters, na.rm = TRUE), .groups = "drop")
}



Droughts2 <- long_disasters(disasters2[["Drought"]])
HotTemps2 <- long_disasters(disasters2[["Extreme temperature"]])
Floods2 <- long_disasters(disasters2[["Flood"]])
Landslides2 <- long_disasters(disasters2[["Landslide"]])
Storms2 <- long_disasters(disasters2[["Storm"]])
TotalDisasters2 <- long_disasters(disasters2[["TOTAL"]])
Wildfires2 <- long_disasters(disasters2[["Wildfire"]])




disaster_names <- c("Droughts", "HotTemps", "Floods", "Landslides", "Storms",
                    "TotalDisasters", "Wildfires")

# Do color brewer for the correct number of graphs.
color_num <- function(input_table, col_num){
  # Input_table = the input table
  # col_num = The number of colors you want in the graphs.
  rep(brewer.pal(col_num, "Paired"), length.out = dim(table(input_table[,1])))
}




# Plot the disasters.
plot_disasters <- function(input_table, min_num = 0, col_num = 11){
  # input_table = the input table
  # min_num = You need to have more than this number of disasters to be included
  # col_num = the number of colors that you want in the graphs.
  final_table <- input_table %>%
    mutate(max_value = apply(select(., -1), 1, max)) %>%
    filter(max_value > min_num) %>%
    select(-max_value) %>%
    pivot_longer(cols = starts_with("F"),
                 names_to = "Year",
                 values_to = "Disasters")
  print(dim(table(final_table$ISO3)))
  
  ggplot(final_table, aes(x = Year, y = Disasters, color = ISO3, group=ISO3)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ ISO3) +
    labs(title = paste("All countries which had more than", min_num,
                       deparse(substitute(input_table)),"in a single year"),
         y = deparse(substitute(input_table))
         ) +
    theme_minimal() +
    scale_color_manual(values = color_num(final_table, col_num)) +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          title = element_text(size = 10))
}

plot_continents <- function(input_table, legend.pos = "right", 
                            title = element_text(size = 10),
                            title_text = paste("The number of",
                                               sub("2$", "", deparse(substitute(input_table))),
                                               "per year,",min(input_table$year), "-", 
                                               max(input_table$year))){
  ggplot(input_table, aes(x = year, y = total_disasters, 
                          color = continent,
                          group = continent)) +
    geom_line(size = 1) +
    labs(title = title_text,
         y = ""
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "RdYlBu") +
    theme(legend.position = legend.pos,
          legend.title = element_blank(),
          legend.background = element_rect(fill = "darkgrey"),
          axis.text.x = element_blank(), 
          title = title,
          panel.background = element_rect(fill = "darkgrey"))
    
}

p1 <- plot_continents(Droughts2, "none", title_text = "Droughts")
p2 <- plot_continents(HotTemps2, "none", title_text = "Hot Temps")
p3 <- plot_continents(Floods2, "none", title_text = "Floods")
p4 <- plot_continents(Landslides2, "none", title_text = "Landslides")
p5 <- plot_continents(Storms2, "none", title_text = "Storms")
p7 <- plot_continents(TotalDisasters2, "none", title_text = "Totals")
p6 <- plot_continents(Wildfires2, "none", title_text = "Wildfires")

text_box <- ggplot() +
  theme_void() +
  annotate("text", x = 0.5, y = 0.5, label = "Droughts\nHot Temps\nFloods\nLandslides\nStorms\nWildfires\nTotals", size = 3, hjust = 0.5) +
  xlim(0,1) + ylim(0,1)



p1 + p2 + p3 + p4 + p5 + p6 + p7 + guide_area() +
  plot_layout(guides = "collect", heights = c(2, 2)) &
  theme(legend.position = "right")

# Disaster_counts is the number of graphs you have where one country has more
# than that number for that many countries. (e.g. 188 countries where they had
# more than 0 storm disasters)
disaster_counts <- tibble(disaster_names,
                                "Disaster0" = c(140, 80, 182, 83, 188, 215, 82),
                                "Disaster1" = c(4, 28, 123, 35, 105, 177, 16),
                                "Disaster2" = c(1, 7, 77, 13, 48, 129, 3),
                                "Disaster3" = c(0, 0, 57, 7, 29, 90, 2),
                                "Disaster5" = c(0, 0, 27, 1, 17, 51, 1),
                                "Disaster7" = c(0, 0, 14, 1, 8, 28, 1),
                                "Disaster8" = c(0, 0, 8, 0, 6, 19, 0),
                                "Disaster10" = c(0, 0, 6, 0, 4, 14, 0),
                                "Disaster15" = c(0, 0, 3, 0, 2, 6, 0),
                                "Disaster20" = c(0, 0, 1, 0, 1, 5, 0))

disaster_counts 
plot_disasters(Floods,5, 12)
plot_disasters(Droughts, 1, 4)
plot_disasters(HotTemps, 2, 7)
plot_disasters(Floods, 8, 8)

plot_decade <- function(data){
  ggplot(data, aes(x = Decade, y = Average,
                   color = continent,
                   group = continent)) +
    geom_line(size = 1) +
    labs(title = paste("Average", sub("2$", "", str_extract(deparse(substitute(data)), 
                                              "(?<=\\().*?(?=\\))"))),
         y = ""
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "RdYlBu") +
    theme(legend.position = "none",
          legend.title = element_blank(),
          legend.background = element_rect(fill = "darkgrey"),
          axis.text.x = element_blank(), 
          title = element_text(size = 10),
          panel.background = element_rect(fill = "darkgrey"))
    
}


collapse_years <- function(data){
  data %>%
    mutate(Decade = as.numeric(paste0(floor(year / 10) * 10))) %>%
    group_by(continent, Decade) %>%
    summarise(Average = mean(total_disasters, na.rm = TRUE), .groups = "drop")
}

collapse_years(Droughts2)
q1 <- plot_decade(collapse_years(Droughts2))
q2 <- plot_decade(collapse_years(HotTemps2))
q3 <- plot_decade(collapse_years(Floods2))
q4 <- plot_decade(collapse_years(Landslides2))
q5 <- plot_decade(collapse_years(Storms2))
q6 <- plot_decade(collapse_years(Wildfires2))
q7 <- plot_decade(collapse_years(TotalDisasters2))

q1 + q2 + q3 + q4 + q5 + q6 + q7 + guide_area() +
  plot_layout(guides = "collect", heights = c(2, 2)) &
  theme(legend.position = "right")


FaC2 <- FaC %>%
  filter(Country %in% c("Africa", "Americas", "Asia", "Euro Area", "Oceania")) %>%
  select(-c(Country, ISO2, Source, CTS_Code, CTS_Name, CTS_Full_Descriptor)) %>%
  filter(Indicator %in% c("Carbon stocks in forests", "Forest area"))

Forest.long <- FaC2 %>%
  filter(Indicator %in% c("Forest area")) %>%
  pivot_longer(cols = starts_with("F"),
               names_to = "Year", values_to = "ForestArea") %>%
  mutate(Year = sub("^F", "", Year))

Carbon.long <- FaC2 %>%
  filter(Indicator %in% c("Carbon stocks in forests")) %>%
  pivot_longer(cols = starts_with("F"),
               names_to = "Year", values_to = "Carbon") %>%
  mutate(Year = sub("^F", "", Year))

ggplot(data = Forest.long, 
       aes(x = Year, y = ForestArea, col=ISO3, group = ISO3)) +
  geom_line(size = 2) +
  labs(title = "The amount of Forest Area over time by continent")

ggplot(data = Carbon.long,
       aes(x = Year, y = Carbon, col=ISO3, group=ISO3)) +
  geom_line(size = 2) +
  labs(title = "The amount of Carbon over time by continent")



  
