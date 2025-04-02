library(tidyverse)
library(psych)

FaC <- read_csv("forest_and_carbon.csv")
ASTC <- read_csv("Annual_Surface_temperature_change.csv")
NDF <- read_csv("Climate-related_Disasters_Frequency.csv")

NDF2 <- NDF %>%
  mutate(Indicator = sub(".*: ", "", Indicator)) %>% # Clean Indicator
  select(-c(ISO2, Country, ObjectId, Unit, Source, CTS_Code, CTS_Name, CTS_Full_Descriptor)) %>% # Remove unneeded columns
  mutate(across(everything(), replace_na, 0)) # Replace NAs with 0


# Split disaster types into their own datasets
disaster_types <- c("Drought", "Extreme temperature", "Flood", "Landslide", "Storm", "TOTAL", "Wildfire")

disasters <- NDF2 %>% 
  filter(Indicator %in% disaster_types) %>%
  select(-Indicator) %>%
  split(NDF2$Indicator)

Droughts <- disasters[["Drought"]]
HotTemps <- disasters[["Extreme temperature"]]
Floods <- disasters[["Flood"]]
Landslides <- disasters[["Landslide"]]
Storms <- disasters[["Storm"]]
TotalDisasters <- disasters[["TOTAL"]]
Wildfires <- disasters[["Wildfire"]]

disaster_names <- c("Droughts", "HotTemps", "Floods", "Landslides", "Storms",
                    "TotalDisasters", "Wildfires")

# Do color brewer for the correct number of graphs.
color_num <- function(input_table, col_num){
  # Input_table = the input table
  # col_num = The number of colors you want in the graphs.
  rep(brewer.pal(col_num, "Paired"), length.out = dim(table(input_table$ISO3)))
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
  
  ggplot(final_table, aes(x = Year, y = Disasters, color = ISO3)) +
    geom_point() +
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





  
