library(tidyverse)
library(jsonlite)
library(glue)

games <- fromJSON("https://www.fotmob.com/api/leagues?id=77&ccode3=USA_VA")
match_urls <- games$matches$allMatches$pageUrl
matchIds <- gsub(".*#","",match_urls)

collect_odds <- function(matchId) {
  api_query <- glue("https://www.fotmob.com/api/matchOdds?matchId={matchId}&ccode3=USA_VA&bettingProvider=Bet365_Virginia")
  jsonfile <- fromJSON(api_query)
  bets <- jsonfile$bets$whoWins$coeffs
  
  output <- data.frame(matchId = matchId,
             team = bets[,1],
             odds = bets[,2])
  
  Sys.sleep(0.2)
  
  return(output)
}

odds <- map_dfr(.x = matchIds, .f = ~ collect_odds(.x))

euro_odds_to_prob <- function(x) {
  (1 / x)
}

final_odds <- odds %>%
  mutate(odds = as.numeric(odds)) %>%
  mutate(prob = euro_odds_to_prob(odds)) %>%
  group_by(matchId) %>%
  mutate(vig = sum(prob)) %>%
  mutate(prob_no_vig = prob / vig) %>%
  select(matchId, team, odds, prob_with_vig = prob, prob = prob_no_vig) %>%
  ungroup()

write_csv(final_odds, "WorldCup2022Odds.csv")
