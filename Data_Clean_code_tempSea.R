library(readr)
library(dplyr)
library(tidyr)


temperature <- read_csv("C:\\University\\Stat 5560\\Final Project\\temperature.csv")
sea_levels <- read_csv("C:\\University\\Stat 5560\\Final Project\\sea_levels.csv")
forest_carbon <- read_csv("C:\\University\\Stat 5560\\Final Project\\forest_carbon.csv")
disasters <- read_csv("C:\\University\\Stat 5560\\Final Project\\disasters.csv")


# clean the temp data
temperature <- temperature %>%
  dplyr::select(-ISO2, -Indicator, -Unit, -Source, -`CTS Code`,
                -`CTS Name`,-`CTS Full Descriptor`)

sum(complete.cases(temperature))       
nrow(temperature) - sum(complete.cases(temperature))

temperature <- drop_na(temperature)

# clean the sea level data
sea_levels <- sea_levels %>%
  dplyr::select(-ISO2, -Indicator, -Unit, -Source, -`CTS Code`,
                -`CTS Name`,-`CTS Full Descriptor`)

sum(complete.cases(sea_levels))       
nrow(sea_levels) - sum(complete.cases(sea_levels))

# puts temp data in long form
temperature_long <- temperature %>%
  pivot_longer(
    cols = `1961`:`2023`,
    names_to = "Year",
    values_to = "Temperature_Change_Celsius"
  ) %>%
  mutate(Year = as.integer(Year)) %>%
  drop_na(Temperature_Change_Celsius)
