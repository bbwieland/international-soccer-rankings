
# What This Script Does ---------------------------------------------------

# This R script produces the necessary raw data files for the rest of the project's analysis.
# Basic data cleaning and manipulation is carried out here.
# The script can be sourced in other files for analysis.

# Package Installations ------------------------------------------------------

# Un-comment if necessary.
# install.packages("devtools")
# devtools::install_github("JaseZiv/worldfootballR")

# Necessary Package Loads -------------------------------------------------

library(worldfootballR)
library(tidyverse)

# Import International Results --------------------------------------------

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