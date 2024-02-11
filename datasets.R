library(tidyverse)

results <- read_csv("https://raw.githubusercontent.com/martj42/international_results/master/results.csv") %>%
  mutate(home_result = case_when(
    home_score > away_score ~ "W",
    home_score == away_score ~ "D",
    home_score < away_score ~ "L"
  )) %>%
  mutate(home_points = case_when(
    home_result == "W" ~ 3,
    home_result == "D" ~ 1,
    home_result == "L" ~ 0
  )) %>%
  mutate(away_result = case_when(
    away_score > home_score ~ "W",
    away_score == home_score ~ "D",
    away_score < home_score ~ "L"
  )) %>%
  mutate(away_points = case_when(
    away_result == "W" ~ 3,
    away_result == "D" ~ 1,
    away_result == "L" ~ 0
  ))

fbref_advanced <- read_csv("FBRef-Advanced-Match-Data.csv")
