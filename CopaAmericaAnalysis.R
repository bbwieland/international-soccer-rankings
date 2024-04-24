library(tidyverse)
library(gt)
library(gtExtras)
library(readxl)
library(countrycode)

brackets <- read_csv("copa-america-2024/CopaBrackets.csv")
groups <- read_csv("copa-america-2024/CopaGroupStages.csv")
teams <- read_excel("CopaStructure.xlsx")

n_sims = 1000

count_round <- function(round) {
  round_results <- brackets %>% 
    filter(Round == round)
  
  data.frame(Team = c(round_results$Team1, round_results$Team2)) %>%
    group_by(Team) %>%
    count() %>%
    magrittr::set_colnames(c("Team", round)) %>%
    ungroup()
}

group_ranks <- groups %>%
  mutate(group_rk = as.numeric(gsub("[A-Z]","",group_rk)))

count_placement <- function(spot, name) {
  round_results <- group_ranks %>% 
    filter(group_rk == spot)
  
  data.frame(Team = round_results$team) %>%
    group_by(Team) %>%
    count() %>%
    magrittr::set_colnames(c("Team", name)) %>%
    ungroup()
}

count_winners <- function(name = "Winner") {
  round_results <- brackets %>%
    filter(Round == "Final")
  
  data.frame(Team = round_results$Winner) %>%
    group_by(Team) %>%
    count() %>%
    magrittr::set_colnames(c("Team", name)) %>%
    ungroup()
}

bracket_counts <- count_round("Quarterfinal") %>%
  left_join(count_round("Semifinal"), by = "Team") %>%
  left_join(count_round("Final"), by = "Team") %>%
  left_join(count_placement(1, "First"), by = "Team") %>%
  left_join(count_placement(2, "Second"), by = "Team") %>%
  left_join(count_winners(), by = "Team") %>%
  arrange(-Quarterfinal) %>%
  mutate(across(where(is.numeric), function(x) x / n_sims)) %>%
  mutate(across(where(is.numeric), function(x) if_else(is.na(x), 0, x))) %>%
  left_join(teams, by = "Team") %>%
  select(Team, Group = CopaGroup, First, Second, Quarterfinal, Semifinal, Final, Winner) %>%
  mutate(Flag = countrycode(Team, origin = 'country.name', destination = 'iso2c')) %>%
  select(Flag, everything())

copa_html <- bracket_counts %>%
  arrange(-Quarterfinal) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_percent(columns = where(is.numeric), decimals = 1) %>%
  gt_add_divider(Quarterfinal, color = "black") %>%
  cols_label(Quarterfinal = "Advance", Flag = "") %>%
  tab_spanner(columns = First:Quarterfinal, label = "Group Stage") %>%
  tab_spanner(columns = Semifinal:Winner, label = "Bracket Play") %>%
  cols_align("center", columns = where(is.numeric)) %>%
  cols_align("center", c(Group, Flag)) %>%
  data_color(columns = Quarterfinal, palette = c("white","purple"), domain = c(0,1)) %>%
  data_color(columns = Winner, palette = c("white","purple"), domain = c(0,0.4)) %>%
  tab_header("2024 Copa America Forecasted Results") %>%
  tab_source_note("Based on 1,000 tournament simulations. Knockout round penalty shootouts determined randomly.") %>%
  fmt_flag(Flag)


gtsave(copa_html, "copa-america-2024/CopaOdds.html")
