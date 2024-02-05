library(tidyverse)
library(worldfootballR)

competitions <- read_csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv")

intl_comps <- c("National Team Competitions",
                "National Team Qualification")

intl_df <- competitions %>%
  filter(competition_type %in% intl_comps & gender == "M")

write_csv(intl_df, "FBRef-International-Competitions.csv")
