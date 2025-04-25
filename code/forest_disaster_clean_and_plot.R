library(dplyr)
library(tidyr)
library(readr)
library(tidyplots)

forest_data <- read_csv("forest_carbon.csv")
disaster_freq_data <- read_csv("Indicator_11_1_Physical_Risks_Climate_related_disasters_frequency_7212563912390016675.csv")


forest_data |> select(-c(ISO2, CTS_Name)) |>
  #drop_na() |>
  pivot_longer(
    cols = `F1992`:`F2022`, 
    values_to = 'number', 
    names_to = "year", 
    names_pattern = "([[:digit:]]{4}$)"
  )  |> 
  pivot_wider(
    names_from = Indicator, 
    values_from = number
  )  |> 
  select(-c(Source,
            "CTS_Code", 
            "CTS_Full_Descriptor")) -> new_forest

sapply(new_forest, summary)

boxplot(new_forest[,7:11])

disaster_freq_data |> 
  select(-c(ISO2)) |> 
  #drop_na() |> 
  pivot_longer(
    cols = `1980`:`2024`,
    values_to = 'number', 
    names_to = "year"
      ) |> 
  separate(Indicator, into = c("Category", "Metric_Disaster"),
           sep = ", ", extra = "merge") |>
  separate(Metric_Disaster,
           into = c("Metric", "Disaster_Type"), sep = ": ", extra = "merge") |>
  # pivot_wider(
  #   names_from = Disaster_Type, 
  #   values_from = number, 
  # ) |> 
  select(-c(Category, Source,
            "CTS Code", "CTS Name", 
            "CTS Full Descriptor")) |>
  filter(Metric != 'Number of Disasters') -> people

sapply(people, summary)
sapply(disasters, summary)

par(mar = c(4, 4, 2, 1))
# Adjust the bottom, left, top, and right margins
boxplot(people[,7:13])

boxplot(disasters[,7:13])


people |> group_by(year) |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |>
  mutate(across(where(is.numeric), ~ log(.x + 1))) |> 
  pivot_longer(cols = `Drought`:`Wildfire`,
               values_to = "value", names_to = "disaster_type") -> simple_graphs_people

simple_graphs_people |> tidyplot(x = year, y = value)|> 
  add_data_points() |> add_curve_fit(se = FALSE) |> adjust_size(NA, NA)|>
  split_plot(by = disaster_type) 

new_forest |> group_by(year) |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |>
  mutate(across(where(is.numeric), ~ log(.x + 1))) |> 
  pivot_longer(cols = `Carbon stocks in forests`:`Share of forest area`,
               values_to = "value", names_to = "indicator") -> simple_graphs_forest

simple_graphs_forest |> tidyplot(x = year, y = value) |> add_data_points() |> add_curve_fit(se = FALSE) |> adjust_size(NA, NA)|>
  split_plot(by = indicator) 
