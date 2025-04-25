library(readr)
library(dplyr)
library(tidyr)


temperature <- read_csv("data/temperature.csv")
sea_levels <- read_csv("data/sea_levels.csv")
forest_carbon <- read_csv("data/forest_carbon.csv")
disasters <- read_csv("data/disasters.csv")


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
sea_long <- sea_levels %>%
  mutate(measured_year = as.numeric(str_extract(Date, "[[:digit:]]{4}$")))
 


sea_long %>%
  mutate(Measure = fct_rev(factor(Measure, levels = sort(unique(Measure))))) %>%
  ggplot(aes(x = measured_year, y = Measure, fill = Value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Sea Level by Region and Year",
       x = "Year", y = "Region", fill = "Sea Level Change") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 8))


sea_long %>% group_by(measured_year, Measure) %>%
  summarize(avg_level = mean(Value)) %>%

ggplot( aes(x = measured_year, y = avg_level, color = Measure)) +
  geom_point(size = 1.5) +
  #coord_cartesian(ylim = c(global_min, global_max)) +
  #ylim(global_min, global_max) +
  facet_wrap(~Measure, scales = "fixed") +
  labs(title = paste("Sea Level change by Sea"),
       y = "Sea Level Change", x = "Year") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 7),
        plot.title = element_text(face = "bold", size = 14), 
        axis.text.x = element_text(angle = 45, size = 5))
