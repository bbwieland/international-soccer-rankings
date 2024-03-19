library(tidyverse)

preds <- read_csv("WorldCup2022Predictions.csv")
odds <- read_csv("WorldCup2022Odds.csv")
names <- read_csv("NameMatch.csv")
odds_simple <- odds %>%
  left_join(names, by = c("team" = "OddsName")) %>%
  select(-team) %>%
  rename(team = PredName) %>%
  select(matchId, team, prob)

predId <- NULL

for (i in seq(1, nrow(preds))) {
  team1 = preds$team[i]
  team2 = preds$opponent[i]
  teams = sort(c(team1,team2))
  combn = paste0(teams, collapse = "")
  predId = c(predId, combn)
}

reformat_odds <- function(id) {
  
  dprob = odds_simple %>%
    filter(matchId == id) %>%
    filter(team == "Draw") %>%
    pull(prob)
  
  wprob = odds_simple %>%
    filter(matchId == id) %>%
    filter(team != "Draw") %>%
    arrange(team) %>%
    head(1) %>%
    pull(prob)
  
  lprob = odds_simple %>%
    filter(matchId == id) %>%
    filter(team != "Draw") %>%
    arrange(team) %>%
    tail(1) %>%
    pull(prob)
  
  team1 = odds_simple %>%
    filter(matchId == id) %>%
    filter(team != "Draw") %>%
    arrange(team) %>%
    head(1) %>%
    pull(team)
  
  team2 = odds_simple %>%
    filter(matchId == id) %>%
    filter(team != "Draw") %>%
    arrange(team) %>%
    tail(1) %>%
    pull(team)
  
  output = data.frame(
    matchId = id,
    team = team1,
    opponent = team2,
    W_odds = wprob,
    D_odds = dprob,
    L_odds = lprob
  )
  
  return(output)
}

odds_new <- map_dfr(.x = unique(odds$matchId),
        .f = ~ reformat_odds(.x))

df <- inner_join(preds, odds_new, by = c("team","opponent"), relationship = "many-to-many")

# Betting -----------------------------------------------------------------

compute_payout <- function(d) {
  d %>%
    select(contains("payout")) %>%
    apply(2, sum)
}

df_bet <- df %>%
  select(date, team, opponent, score, opp_score, result, W_model = W, D_model = D, L_model = L, W_odds, D_odds, L_odds, W = is_win, D = is_draw, L = is_loss) %>%
  mutate(W_base = 1/3, D_base = 1/3, L_base = 1/3,
         W_naive = 0.3849273, D_naive = 0.2301455, L_naive = 0.3849273)
  
df_bet_model_v_vegas <- df_bet %>%
  mutate(bet_W = W_model > W_odds, bet_D = D_model > D_odds, bet_L = L_model > L_odds) %>%
  mutate(across(contains("bet"), as.numeric)) %>%
  mutate(payout_W = ifelse(bet_W == W, 1 - W_odds, - W_odds),
         payout_D = ifelse(bet_D == D, 1 - D_odds, - D_odds),
         payout_L = ifelse(bet_L == L, 1 - L_odds, - L_odds),
         payout_game = payout_W + payout_D + payout_L)

df_bet_vegas_v_model <- df_bet %>%
  mutate(bet_W = W_odds > L_model, bet_D = W_odds > L_model, bet_L = W_odds > L_model) %>%
  mutate(across(contains("bet"), as.numeric)) %>%
  mutate(payout_W = ifelse(bet_W == W, 1 - W_model, - W_model),
         payout_D = ifelse(bet_D == D, 1 - D_model, - D_model),
         payout_L = ifelse(bet_L == L, 1 - L_model, - L_model),
         payout_game = payout_W + payout_D + payout_L)

df_bet_model_v_base <- df_bet %>%
  mutate(bet_W = W_model > W_base, bet_D = D_model > D_base, bet_L = L_model > L_base) %>%
  mutate(across(contains("bet"), as.numeric)) %>%
  mutate(payout_W = ifelse(bet_W == W, 1 - W_base, - W_base),
         payout_D = ifelse(bet_D == D, 1 - D_base, - D_base),
         payout_L = ifelse(bet_L == L, 1 - L_base, - L_base),
         payout_game = payout_W + payout_D + payout_L)

df_bet_model_v_naive <- df_bet %>%
  mutate(bet_W = W_model > W_naive, bet_D = D_model > D_naive, bet_L = L_model > L_naive) %>%
  mutate(across(contains("bet"), as.numeric)) %>%
  mutate(payout_W = ifelse(bet_W == W, 1 - W_naive, - W_naive),
         payout_D = ifelse(bet_D == D, 1 - D_naive, - D_naive),
         payout_L = ifelse(bet_L == L, 1 - L_naive, - L_naive),
         payout_game = payout_W + payout_D + payout_L)

compute_payout(df_bet_model_v_base)
compute_payout(df_bet_model_v_naive)
compute_payout(df_bet_model_v_vegas)

betting_results_list = list(
  base = compute_payout(df_bet_model_v_base),
  naive = compute_payout(df_bet_model_v_naive),
  vegas = compute_payout(df_bet_model_v_vegas)
)

clean_results = function(d) {
  inter <- d %>%
    data.frame() %>%
    t() 
  output <- cbind(opponent = rownames(inter), inter) %>%
    magrittr::set_rownames(NULL) %>%
    data.frame() %>%
    mutate(across(contains("payout"), as.numeric))
  
  return(output)
}


betting_results <- clean_results(betting_results_list) %>%
  mutate(roi = payout_game / (nrow(df_bet) * 3))

betting_results

library(gt)
library(gtExtras)

gt(betting_results) %>%
  gt_theme_538() %>%
  fmt_number(payout_W:payout_L, force_sign = TRUE, decimals = 1) %>%
  fmt_number(payout_game, decimals = 0, pattern = "{x} units", force_sign = TRUE) %>%
  fmt_percent(roi, decimals = 1) %>%
  tab_spanner(columns = payout_W:payout_L, label = "Payouts by Result Type") %>%
  cols_label(payout_W ~ "Wins",
             payout_D ~ "Draws",
             payout_L ~ "Losses",
             payout_game = "Winnings") %>%
  tab_spanner(columns = c(payout_game, roi), label = "Betting Results") %>%
  tab_footnote(locations = cells_body(columns = opponent, rows = 1),
               footnote = "A 1 in 3 chance of each outcome occurring.") %>%
  tab_footnote(locations = cells_body(columns = opponent, rows = 2),
               footnote = "Using historical win-loss-draw rates to forecast 38.5% win/loss and 23% draw.") %>%
  tab_footnote(locations = cells_body(columns = opponent, rows = 3),
               footnote = "Bet365 match odds scraped from Fotmob.") %>%
  tab_options(footnotes.padding = 0) %>%
  tab_header("Team-Strength Model Evaluation: Betting Against Other Model Types")
