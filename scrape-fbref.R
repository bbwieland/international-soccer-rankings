library(worldfootballR)
library(tidyverse)

competitions <- read_csv("https://raw.githubusercontent.com/bbwieland/international-soccer-rankings/main/FBRef-International-Competitions.csv?token=GHSAT0AAAAAACMPKLU3UDGCHIUXMSLY4H6IZNX77EA")

competitions$comp_url
competitions$season_end_year

# match-level data: https://jaseziv.github.io/worldfootballR/articles/fbref-data-internationals.html#match-level-data

scrape_international_results_fbref <- function() {
  international_results <- fb_match_results(country = "", gender = "M", season_end_year = 2021, tier = "", non_dom_league_url = "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons")
  
}

results <- map2_dfr(.x = competitions$comp_url, .y = competitions$season_end_year,
         .f = ~ fb_match_results(country = "",
                                 gender = "M",
                                 season_end_year = .y,
                                 tier = "",
                                 non_dom_league_url = .x))

write_csv(results, "FBRef-Advanced-Match-Data.csv")