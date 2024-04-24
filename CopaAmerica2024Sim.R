# Global Variables:
library(readr)
library(dplyr)
library(devtools)
library(bivpois)
library(purrr)
library(janitor)
library(magrittr)
library(tidyverse)
library(lme4)
library(readxl)
library(nflreadr)

set.seed(4133)

clean_rankings <- function(df) {
  # Purpose: pre-processes the `rankings` dataframe for clean output.
  output <- df %>%
    mutate(
      o_rk = dense_rank(-o_effect),
      d_rk = dense_rank(d_effect),
      n_rk = dense_rank(-net_effect)
    ) %>%
    select(
      team,
      n_val = net_effect,
      n_rk,
      o_val = o_effect,
      o_rk,
      d_val = d_effect,
      d_rk
    ) %>%
    arrange(n_rk)
}

# import Data -------------------------------------------------------------

rankings <-
  read_csv("ModelRankingsCurrent.csv") %>% clean_rankings()

off_model <- readRDS("OffModelCurrent.Rds")
def_model <- readRDS("DefModelCurrent.Rds")

# Prediction Function -----------------------------------------------------

clip_predictions <- function(x) {
  if_else(x < 0.05, 0.05, x)
}

predict_match <- function(team1, team2, location) {
  # location = (1 = home, 0 = neutral, -1 = away)
  
  location_numeric <- case_when(location == "Home" ~ 1,
                                location == "Neutral" ~ 0,
                                location == "Away" ~ -1,
                                TRUE ~ NA)
  
  prediction_df <-
    data.frame(team = team1,
               opponent = team2,
               location = location_numeric)
  team_goals <-
    predict(off_model, prediction_df, type = "response") %>% clip_predictions()
  opp_goals <-
    predict(def_model, prediction_df, type = "response") %>% clip_predictions()
  
  go_to = 10
  
  goals <- expand.grid(seq(0, go_to), seq(0, go_to))
  
  lik <- map2_vec(
    .x = goals$Var1,
    .y = goals$Var2,
    .f = ~ dbp(
      x1 = .x,
      x2 = .y,
      lambda = c(team_goals, opp_goals, 0),
      logged = FALSE
    )
  )
  
  likelihoods <- cbind(goals, lik)
  
  likelihoods <- likelihoods %>%
    mutate(result = case_when(Var1 > Var2 ~ "W",
                              Var1 == Var2 ~ "D",
                              Var1 < Var2 ~ "L",
                              TRUE ~ NA)) %>%
    mutate(lik = if_else(result == "D", lik * 1.1, lik))
  
  # rescaling to be valid PMF
  
  scale_factor <- 1 / sum(likelihoods$lik)
  
  likelihoods$lik <- likelihoods$lik * scale_factor
  
  match_probs <- likelihoods %>%
    group_by(result) %>%
    summarise(lik_sum = sum(lik)) %>%
    mutate(result = factor(result, levels = c("L", "D", "W")))
  
  return(match_probs)
}

# Begin Simulation --------------------------------------------------------

groups <- read_excel("CopaStructure.xlsx", sheet = "Teams")
bracket <- read_excel("CopaStructure.xlsx", sheet = "Bracket Play")

simulate_match <- function(team1, team2, location) {
  location_numeric <- case_when(location == "Home" ~ 1,
                                location == "Neutral" ~ 0,
                                location == "Away" ~ -1,
                                TRUE ~ NA)
  
  prediction_df <-
    data.frame(team = team1,
               opponent = team2,
               location = location_numeric)
  team_goals <-
    predict(off_model, prediction_df, type = "response") %>% clip_predictions()
  opp_goals <-
    predict(def_model, prediction_df, type = "response") %>% clip_predictions()
  
  match_result <- rbp(n = 1, lambda = c(team_goals, opp_goals, 0))
  
  
  go_to = 10
  
  goals <- expand.grid(seq(0, go_to), seq(0, go_to))
  
  lik <- map2_vec(
    .x = goals$Var1,
    .y = goals$Var2,
    .f = ~ dbp(
      x1 = .x,
      x2 = .y,
      lambda = c(team_goals, opp_goals, 0),
      logged = FALSE
    )
  )
  
  likelihoods <- cbind(goals, lik)
  
  likelihoods <- likelihoods %>%
    mutate(result = case_when(Var1 > Var2 ~ "W",
                              Var1 == Var2 ~ "D",
                              Var1 < Var2 ~ "L",
                              TRUE ~ NA)) %>%
    mutate(lik = if_else(result == "D", lik * 1.1, lik))
  
  # rescaling to be valid PMF
  
  scale_factor <- 1 / sum(likelihoods$lik)
  
  likelihoods$lik <- likelihoods$lik * scale_factor
  
  
  score_row <- sample(seq_along(1:nrow(likelihoods)),
                      size = 1,
                      prob = likelihoods$lik)
  
  score_sample <- likelihoods[score_row,]
  
  score_output <- data.frame(
    home_team = team1,
    away_team = team2,
    home_score = score_sample$Var1,
    away_score = score_sample$Var2,
    Result = score_sample$result
  ) %>%
    mutate(
      home_points = case_when(Result == "W" ~ 3,
                              Result == "D" ~ 1,
                              Result == "L" ~ 0),
      away_points = case_when(Result == "W" ~ 0,
                              Result == "D" ~ 1,
                              Result == "L" ~ 3)
    ) %>%
    select(-Result)
  return(score_output)
}

# Pool Play Games ----

simulate_group_stage <- function() {
  copa_groups <- unique(groups$CopaGroup)
  
  build_group_schedule <- function(group) {
    group_teams <- groups %>% filter(CopaGroup == group)
    combn(group_teams$Team, 2) %>% t() %>% data.frame() %>% set_colnames(c("Team1", "Team2")) %>%
      mutate(
        Location = case_when(
          Team1 == "United States" ~ "Home",
          Team2 == "United States" ~ "Away",
          TRUE ~ "Neutral"
        )
      )
  }
  
  copa_group_schedule <-
    map_dfr(.x = copa_groups, .f = ~ build_group_schedule(.x))
  
  copa_group_sim <-
    map_dfr(
      .x = seq_along(1:nrow(copa_group_schedule)),
      .f = ~ simulate_match(
        team1 = copa_group_schedule$Team1[.x],
        team2 = copa_group_schedule$Team2[.x],
        location = copa_group_schedule$Location[.x]
      )
    )
  
  
  copa_group_results <- rbind(
    copa_group_sim %>%
      group_by(home_team) %>%
      summarise(
        pts = sum(home_points),
        gf = sum(home_score),
        ga = sum(away_score),
        g = n()
      ) %>%
      rename(team = home_team),
    
    copa_group_sim %>%
      group_by(away_team) %>%
      summarise(
        pts = sum(away_points),
        gf = sum(away_score),
        ga = sum(home_score),
        g = n()
      ) %>%
      rename(team = away_team)
  ) %>%
    group_by(team) %>%
    summarise(
      g = sum(g),
      pts = sum(pts),
      gf = sum(gf),
      ga = sum(ga)
    ) %>%
    mutate(gd = gf - ga) %>%
    left_join(groups, by = c("team" = "Team")) %>%
    rename(group = CopaGroup) %>%
    select(group, everything()) %>%
    arrange(group, -pts, -gd, -gf) %>%
    group_by(group) %>%
    mutate(group_rk = paste0(group, row_number(group))) %>%
    ungroup()
  
  return(list(
    results = copa_group_results,
    games = copa_group_sim
  ))
}

simulate_knockouts <- function(group_stage) {
  group_teams <- group_stage %>% select(team, group_rk)
  
  quarters <- bracket %>%
    filter(Round == "Quarterfinal") %>%
    left_join(group_teams, by = c("Team1" = "group_rk")) %>%
    left_join(group_teams,
              by = c("Team2" = "group_rk"),
              suffix = c("1", "2")) %>%
    select(round = Game,
           Team1 = team1,
           Team2 = team2) %>%
    mutate(
      Location = case_when(
        Team1 == "United States" ~ "Home",
        Team2 == "United States" ~ "Away",
        TRUE ~ "Neutral"
      )
    )
  
  quarters_sim <- map_dfr(
    .x = seq_along(1:nrow(quarters)),
    .f = ~ simulate_match(
      team1 = quarters$Team1[.x],
      team2 = quarters$Team2[.x],
      location = quarters$Location[.x]
    )
  ) %>%
    mutate(game = paste0("G", row_number())) %>%
    mutate(tiebreak = if_else(rnorm(4) > 0.5, home_team, away_team)) %>%
    mutate(
      winner = case_when(
        home_score > away_score ~ home_team,
        home_score < away_score ~ away_team,
        home_score == away_score ~ tiebreak
      )
    )
  
  quarters_winners <- quarters_sim %>%
    select(game, winner)
  
  semis <- bracket %>%
    filter(Round == "Semifinal") %>%
    left_join(quarters_winners, by = c("Team1" = "game")) %>%
    left_join(quarters_winners,
              by = c("Team2" = "game"),
              suffix = c("1", "2")) %>%
    select(round = Game,
           Team1 = winner1,
           Team2 = winner2) %>%
    mutate(
      Location = case_when(
        Team1 == "United States" ~ "Home",
        Team2 == "United States" ~ "Away",
        TRUE ~ "Neutral"
      )
    )
  
  semis_sim <- semis %>%
    map_dfr(
      .x = seq_along(1:nrow(semis)),
      .f = ~ simulate_match(
        team1 = semis$Team1[.x],
        team2 = semis$Team2[.x],
        location = semis$Location[.x]
      )
    ) %>%
    mutate(game = paste0("G", row_number() + 4)) %>%
    mutate(tiebreak = if_else(rnorm(2) > 0.5, home_team, away_team)) %>%
    mutate(
      winner = case_when(
        home_score > away_score ~ home_team,
        home_score < away_score ~ away_team,
        home_score == away_score ~ tiebreak
      )
    )
  
  semis_winners <- semis_sim %>%
    select(game, winner)
  
  finals <- bracket %>%
    filter(Round == "Final") %>%
    left_join(semis_winners, by = c("Team1" = "game")) %>%
    left_join(semis_winners,
              by = c("Team2" = "game"),
              suffix = c("1", "2")) %>%
    select(round = Game,
           Team1 = winner1,
           Team2 = winner2) %>%
    mutate(
      Location = case_when(
        Team1 == "United States" ~ "Home",
        Team2 == "United States" ~ "Away",
        TRUE ~ "Neutral"
      )
    )
  
  finals_sim <- map_dfr(
    .x = seq_along(1:nrow(finals)),
    .f = ~ simulate_match(
      team1 = finals$Team1[.x],
      team2 = finals$Team2[.x],
      location = finals$Location[.x]
    )
  ) %>%
    mutate(game = paste0("G", row_number() + 6)) %>%
    mutate(tiebreak = if_else(rnorm(1) > 0.5, home_team, away_team)) %>%
    mutate(
      winner = case_when(
        home_score > away_score ~ home_team,
        home_score < away_score ~ away_team,
        home_score == away_score ~ tiebreak
      )
    )
  
  finals_winner <- finals_sim %>%
    select(game, winner)
  
  bracket_results <- rbind(quarters_sim, semis_sim, finals_sim) %>%
    left_join(bracket, by = c("game" = "Game")) %>%
    select(
      Round,
      Team1 = home_team,
      Team2 = away_team,
      Score1 = home_score,
      Score2 = away_score,
      Winner = winner
    )
 
  return(list(
    results = bracket_results
  )) 
}

simulate_copa <- function(sim_number) {
  group_play <- simulate_group_stage()
  bracket_play <- simulate_knockouts(group_stage = group_play$results)
  
  return(list(
    bracket = bracket_play$results %>% mutate(sim = sim_number),
    groupstage = group_play$results %>% mutate(sim = sim_number)
  ))
}

copas = seq(1,1000)

copa_full_sims <- map(.x = copas, .f = ~ simulate_copa(.x), .progress = "Running Copa America simulations...")

pull_bracket <- function(copa_sims) {
  n <- length(copa_sims)
  
  map_dfr(seq(1,n), ~ copa_sims[[.x]]$bracket)
}

pull_group_stage <- function(copa_sims) {
  n <- length(copa_sims)
  
  map_dfr(seq(1,n), ~ copa_sims[[.x]]$groupstage)
}


copa_brackets <- pull_bracket(copa_full_sims)
copa_group_stages <- pull_group_stage(copa_full_sims)

write_csv(copa_brackets, "copa-america-2024/CopaBrackets.csv")
write_csv(copa_group_stages, "copa-america-2024/CopaGroupStages.csv")
