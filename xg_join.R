source("load-data.R")

xg_data <- read_csv("FBRef-Advanced-Match-Data.csv")

clean_home <- function(team_name) {
  gsub("\\s\\b[a-z]{2,3}\\b", "", team_name)
}

clean_away <- function(team_name) {
  gsub("\\b[a-z]{2,3}\\b\\s", "", team_name)
}

xg_clean <- xg_data %>%
  mutate(home_clean = clean_home(Home),
         away_clean = clean_away(Away))

clean_home("St. Kitts & Nevis kn")

clean_away("ec Ecuador")

xg_simple <- xg_clean %>%
  select(date = Date,
         home_team = home_clean,
         home_score = HomeGoals,
         home_xg = Home_xG,
         away_team = away_clean,
         away_score = AwayGoals,
         away_xg = Away_xG)

# Begin Join --------------------------------------------------------------

results_xg <- left_join(results, xg_simple, by = c("date", "home_score", "away_score","home_team","away_team"))

test <- results_xg %>%
  group_by(year = year(date)) %>%
  summarise(games = n(),
            games_with_xg = length(home_xg[!is.na(home_xg)]),
            pct_with_xg = games_with_xg / games)

ggplot(test, aes(x = year, y = pct_with_xg)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(2000,2024))

results_xg %>%
  filter(year(date) >= 2018) %>%
  group_by(tournament) %>%
  summarise(games = n(),
            games_with_xg = length(home_xg[!is.na(home_xg)])) %>%
  arrange(-games_with_xg)

