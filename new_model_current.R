library(tidyverse)
library(lme4)
library(bivpois)

team_results <- read_csv("team_results.csv")
training_data <- team_results %>%
  filter(date > 1992)

time_decay_function <- function(game_date, half_life, current_date = Sys.Date()) {
  
  game_date <- as.Date(game_date)
  current_date <- as.Date(current_date)
  
  days_elapsed <- as.numeric(difftime(current_date, game_date, units = "days"))
  
  decay_value <- (1/2) ^ (days_elapsed / half_life)
  
  return(decay_value)
}

model_half_life <- 365 * 6

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

off_model <- lme4::glmer(score ~ (1 | team) + (1 | opponent) + location,
            data = model_data,
            weights = time_weight,
            family = poisson)

def_model <- lme4::glmer(opp_score ~ (1 | team) + (1 | opponent) + location,
                         data = model_data,
                         weights = time_weight,
                         family = poisson)

saveRDS(off_model, file = "OffModelCurrent.RDS") 
saveRDS(def_model, file = "DefModelCurrent.RDS") 

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

write_csv(net_eff, "ModelRankingsCurrent.csv")
