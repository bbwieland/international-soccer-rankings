library(tidyverse)
library(lme4)
library(footBayes)

team_results <- read_csv("team_results.csv")

time_decay_function <- function(game_date, half_life, current_date = Sys.Date()) {
  
  game_date <- as.Date(game_date)
  current_date <- as.Date(current_date)
  
  days_elapsed <- as.numeric(difftime(current_date, game_date, units = "days"))
  
  decay_value <- (1/2) ^ (days_elapsed / half_life)
  
  return(decay_value)
}

model_half_life <- 365 * 2

model_data <- team_results %>%
  mutate(time_weight = time_decay_function(date, model_half_life)) %>%
  filter(year(date) >= 2000)

intercept_term <- mean(c(model_data$score, model_data$opp_score))

off_model <- lme4::lmer(opp_score ~ location + (1 | team) + (1 | opponent), 
                    data = model_data, 
                    weights = time_weight)

def_model <- lme4::lmer(opp_score ~ location + (1 | team) + (1 | opponent), 
                         data = model_data, 
                         weights = time_weight)

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

footBayes::mle_foot()

?mle_foot()
