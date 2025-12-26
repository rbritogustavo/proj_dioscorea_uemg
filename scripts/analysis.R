# 1. SETUP ----------------------------------------------------------------

library(tidyverse)
source("scripts/functions.R")

# 2. Import dataset -------------------------------------------------------

raw_data <- readxl::read_excel("data/raw/dataset_completo_dioscorea.xlsx", sheet = 4) %>%
  janitor::clean_names() %>%
  rename(title = ti, year = py)

analysis_table <- readxl::read_excel("data/raw/dataset_completo_dioscorea.xlsx", sheet = 1) %>%
  janitor::clean_names() %>%
  slice(1:374)

# 3. Update "Year" column -------------------------------------------------

analysis_table <- analysis_table %>%
  left_join(raw_data %>% select(title, year), "title") # recuperate year column

analysis_table <- analysis_table %>%
  mutate(year.y = replace_na(year.y, 1994)) %>%  # replace NA with the year
  relocate(year.y, .after = 2) %>% # relocate column to original position
  rename(year = year.y) %>% # rename
  select(-year.x) # remove empty column

# 4. Temporal trend analysis ----------------------------------------------

## Get temporal trend from the dataset
trend <- analysis_table %>%
  group_by(year) %>%
  summarise(n_papers = n(), .groups = "drop") %>%
  filter(year != 2025) %>%
  arrange(year)

## Calculate the complete trend
years <- tibble(year = 1973:2024) # complete time period

pub_trend <- years %>% # update time period
  left_join(trend, by = "year") %>%
  mutate(n_papers = replace_na(n_papers, 0))

## Calculate moving averages (3, 5, and 10 years)
pub_trend <- pub_trend %>%
  mutate(
    moving_avg_3 = round(zoo::rollmean(
      n_papers,
      k = 3,
      fill = NA,
      align = "right"
    ), 2),
    moving_avg_5 = round(zoo::rollmean(
      n_papers,
      k = 5,
      fill = NA,
      align = "right"
    ), 2),
    moving_avg_10 = round(zoo::rollmean(
      n_papers,
      k = 10,
      fill = NA,
      align = "right"
    ), 2)
  )

## Export dataset
writexl::write_xlsx(pub_trend, "results/publication_trends.xlsx")