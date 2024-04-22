library(tidyverse)
library(lme4)
library(bivpois)
library(dplyr)

# read in dataset
team_results <- read_csv("team_results.csv")

head(team_results)

# filter dataset for the fifa world cup results of 2022
wc_2022_results <- team_results %>%
  filter(tournament == "FIFA World Cup" & abs(year(date) - 2022) <= 1)

# get the earliest date of the world cup results
wc_start_date <- min(wc_2022_results$date)

# ---------------------------------- PLAYER DATA -----------------------------
player_stats <- read_csv('Players22.csv')

# Aggregate player stats by team
team_player_stats <- player_stats %>%
  group_by(nationality_name) %>%
  summarise(
    avg_shooting = mean(shooting, na.rm = TRUE),
    avg_passing = mean(passing, na.rm = TRUE),
    avg_defending = mean(defending, na.rm = TRUE),
    avg_overall = mean(overall, na.rm = TRUE)
  )

team_stats <- team_player_stats %>%
  rename(team_avg_shooting = avg_shooting,
         team_avg_passing = avg_passing,
         team_avg_defending = avg_defending,
         team_avg_overall = avg_overall,
         team_name = nationality_name)

opponent_stats <- team_player_stats %>%
  rename(opponent_avg_shooting = avg_shooting,
         opponent_avg_passing = avg_passing,
         opponent_avg_defending = avg_defending,
         opponent_avg_overall = avg_overall,
         opponent_name = nationality_name)

# Merge aggregated player stats with team results
wc_2022_results <- wc_2022_results %>%
  left_join(team_stats, by = c("team" = "team_name"))

# Merge aggregated player stats with opponent results
wc_2022_results <- wc_2022_results %>%
  left_join(opponent_stats, by = c("opponent" = "opponent_name"))

# training_data <- full_team_results %>%
#   filter(date < wc_start_date & date > 1992)

---------------------------------------------------------------------------

# create training dataset using all the data before the world cup first date
training_data <- team_results %>%
  filter(date < wc_start_date & date > 1992)

# time decay function for weighting recent games heavier since they're more telling
time_decay_function <- function(game_date, half_life, current_date = Sys.Date()) {
  
  game_date <- as.Date(game_date)
  current_date <- as.Date(current_date)
  
  days_elapsed <- as.numeric(difftime(current_date, game_date, units = "days"))
  
  decay_value <- (1/2) ^ (days_elapsed / half_life)
  
  return(decay_value)
}

# half life for decay function
model_half_life <- 365 * 6

# apply time decay function to training data to get a time-weighted dataset
model_data <- training_data %>%
  mutate(time_weight = time_decay_function(date, model_half_life)) 


# off_model <- lme4::glmer(score + 1 ~ (1 | team) + (1 | opponent) + location,
#             data = model_data,
#             weights = time_weight,
#             family = Gamma(link = "identity"))
# 
# def_model <- lme4::glmer(opp_score + 1 ~ (1 | team) + (1 | opponent) + location,
#                          data = model_data,
#                          weights = time_weight,
#                          family = Gamma(link = "identity"))

# define and fit offensive and defensive mixed-effects poisson models
off_model <- lme4::glmer(score ~ (1 | team) + (1 | opponent) + location,
            data = model_data,
            weights = time_weight,
            family = poisson)

def_model <- lme4::glmer(opp_score ~ (1 | team) + (1 | opponent) + location,
                         data = model_data,
                         weights = time_weight,
                         family = poisson)

saveRDS(off_model, file = "OffModel.RDS") 
saveRDS(def_model, file = "DefModel.RDS") 

# extract and process random effects from the models
off_eff <- ranef(off_model) %>% 
  as.data.frame() %>% 
  filter(grpvar == "team") %>%
  select(team = grp, o_effect = condval, o_sd = condsd)

def_eff <- ranef(def_model) %>% 
  as.data.frame() %>% 
  filter(grpvar == "team") %>%
  select(team = grp, d_effect = condval, d_sd = condsd)

# calculate net effects and join the offensive and defensive effects
net_eff <- inner_join(off_eff, def_eff, by = "team") %>%
  mutate(net_effect = o_effect - d_effect)

# define function to clip predicted values at a minimum threshold
clip_predictions <- function(x) {
  ifelse(x < 0.05, 0.05, x)
}

predict_data <- wc_2022_results %>%
  select(team, opponent, location, team_avg_overall, team_avg_passing, team_avg_shooting
         , team_avg_defending, opponent_avg_overall.x, opponent_avg_passing.x, 
         opponent_avg_shooting.x, opponent_avg_defending.x)

# apply model to world cup 2022 data, predict scores, and adjust with clip predictions
wc_2022_eval <- wc_2022_results %>%
  mutate(pred_score = predict(off_model, ., re.form = NULL, type = "response"),
         pred_opp_score = predict(def_model, ., re.form = NULL, type = "response")) %>%
  mutate(across(c(pred_score, pred_opp_score), clip_predictions))

# Define an adjustment function considering player stats
adjust_predictions <- function(base_prediction, team_stat, opponent_stat) {
  # Example of a simple adjustment: you could weigh predictions by the average player ratings
  # This is a naive adjustment and should be informed by your understanding of the data and domain
  adjusted_prediction <- base_prediction * (team_stat / opponent_stat)
  return(adjusted_prediction)
}

# Adjust the predictions with the player stats
wc_2022_eval <- wc_2022_eval %>%
  mutate(
    pred_score = adjust_predictions(pred_score, team_avg_overall, opponent_avg_overall.x),
    pred_opp_score = adjust_predictions(pred_opp_score, opponent_avg_overall.x, team_avg_overall)
  ) %>%
  mutate(across(c(pred_score, pred_opp_score), clip_predictions))

# predict match outcomes based on score likelihoods from bivariate poisson distributions 
predict_match <- function(team1, team2) {
  get_score_likelihood <- function(team1, team2, go_to = 5) {
    
    goals <- expand.grid(seq(0,go_to), seq(0,go_to))
    
    lik <- map2_vec(.x = goals$Var1, .y = goals$Var2, .f = ~ bivpois::dbp(
      x1 = .x, x2 = .y, lambda = c(team1, team2, 0), logged = FALSE
    ))
    
    goals <- cbind(goals, lik)
    colnames(goals) <- c("team1", "team2", "lik")
    return(goals)
    
  }
  
  
  likelihoods <- get_score_likelihood(team1, team2)
  
  likelihoods <- likelihoods %>%
    mutate(result = case_when(
      team1 > team2 ~ "W",
      team1 == team2 ~ "D",
      team1 < team2 ~ "L",
      TRUE ~ NA
    )) %>%
    mutate(lik = ifelse(result == "D", lik * 1.1, lik))
  
  # rescaling to be valid PMF
  
  scale_factor <- 1 / sum(likelihoods$lik)
  
  likelihoods$lik <- likelihoods$lik * scale_factor
  
  match_probs <- likelihoods %>% 
    group_by(result) %>% 
    summarise(lik_sum = sum(lik)) %>%
    mutate(result = factor(result, levels = c("W","D","L"))) %>%
    t() %>%
    data.frame() %>%
    janitor::row_to_names(row_number = 1) %>%
    magrittr::set_rownames(NULL)
  
  return(match_probs)
}

# generate and compile the match predictions
wc_2022_pred_results <- map2_dfr(.x = wc_2022_eval$pred_score,
         .y = wc_2022_eval$pred_opp_score,
         .f = ~ predict_match(.x, .y))

# bind predictions with actual match results and compute outcomes
wc_2022_final <- cbind(wc_2022_eval, wc_2022_pred_results) %>%
  mutate(is_draw = as.numeric(score == opp_score),
         is_win = as.numeric(score > opp_score),
         is_loss = as.numeric(score < opp_score))

# output final dataset to csv
write_csv(wc_2022_final,"WorldCup2022Predictions.csv")

# Gamma Distribution Analysis ---------------------------------------------

# Dispersion and shape parameters allow us to observe the regression's 


