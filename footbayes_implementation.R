library(tidyverse)
library(footBayes)
library(bayesplot)
library(loo)

results <- read_csv("https://raw.githubusercontent.com/martj42/international_results/master/results.csv")

## Modifying team results dataframe to match the format required by footBayes package

model_data <- results %>%
  select(Date = date,
         home = home_team,
         visitor = away_team,
         hgoal = home_score,
         vgoal = away_score,
         neutral) %>%
  mutate(Season = year(Date))

reduced_data <- model_data %>%
  select(Season, home, visitor, hgoal, vgoal) %>%
  na.omit() %>%
  filter(home != "NA" & visitor != "NA")
  
n_iter = 200
fit_stan <- stan_foot(data = reduced_data,
                       model="biv_pois",
                       dynamic_type ="seasonal", 
                       #cores = 4,
                       iter = n_iter) # biv poisson

print(fit_stan, pars =c("home", "rho", "sigma_att",
                         "sigma_def"))
  
