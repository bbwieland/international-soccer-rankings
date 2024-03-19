library(tidyverse)
library(lme4)
library(bivpois)

team_results <- read_csv("team_results.csv")

wc_2022_results <- team_results %>%
  filter(tournament == "FIFA World Cup" & abs(year(date) - 2022) <= 1)

wc_start_date <- min(wc_2022_results$date)

training_data <- team_results %>%
  filter(date < wc_start_date)

time_decay_function <- function(game_date, half_life, current_date = Sys.Date()) {
  
  game_date <- as.Date(game_date)
  current_date <- as.Date(current_date)
  
  days_elapsed <- as.numeric(difftime(current_date, game_date, units = "days"))
  
  decay_value <- (1/2) ^ (days_elapsed / half_life)
  
  return(decay_value)
}

model_half_life <- 365 * 5

model_data <- training_data %>%
  mutate(time_weight = time_decay_function(date, model_half_life)) 

off_model <- lme4::glmer(score + 1 ~ (1 | team) + (1 | opponent) + location,
            data = model_data,
            weights = time_weight,
            family = Gamma(link = "identity"))

def_model <- lme4::glmer(opp_score + 1 ~ (1 | team) + (1 | opponent) + location,
                         data = model_data,
                         weights = time_weight,
                         family = Gamma(link = "identity"))

off_eff <- ranef(off_model) %>% 
  as.data.frame() %>% 
  filter(grpvar == "team") %>%
  select(team = grp, o_effect = condval, o_sd = condsd)

def_eff <- ranef(def_model) %>% 
  as.data.frame() %>% 
  filter(grpvar == "team") %>%
  select(team = grp, d_effect = condval, d_sd = condsd)

net_eff <- inner_join(off_eff, def_eff, by = "team") %>%
  mutate(net_effect = o_effect - d_effect)

clip_predictions <- function(x) {
  ifelse(x < 0.05, 0.05, x)
}

wc_2022_eval <- wc_2022_results %>%
  mutate(pred_score = predict(off_model, wc_2022_results) - 1,
         pred_opp_score = predict(def_model, wc_2022_results) - 1) %>%
  mutate(across(c(pred_score, pred_opp_score), clip_predictions))

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

wc_2022_pred_results <- map2_dfr(.x = wc_2022_eval$pred_score,
         .y = wc_2022_eval$pred_opp_score,
         .f = ~ predict_match(.x, .y))

wc_2022_final <- cbind(wc_2022_eval, wc_2022_pred_results) %>%
  mutate(is_draw = as.numeric(score == opp_score),
         is_win = as.numeric(score > opp_score),
         is_loss = as.numeric(score < opp_score))

write_csv(wc_2022_final,"WorldCup2022Predictions.csv")
