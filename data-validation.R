# Dependencies ------------------------------------------------------------

source("load-data.R")

# Value Count Investigations ----------------------------------------------

# How many games are neutral-site? 

table(results$neutral) / nrow(results)

# How many unique countries played a game?
# note: some countries changed their names

length(unique(c(results$home_team, results$away_team)))

# How many cities hosted a game?

length(unique(results$city))

# How many countries hosted a game?

length(unique(results$country))

# Which tournaments hosted the greatest number of games?

results %>% group_by(tournament) %>% count() %>% arrange(-n)

results %>%
  group_by(home_team) %>%
  summarise(avg_pts = mean(home_points),
            games = n()) %>%
  arrange(-avg_pts) %>%
  filter(games >= 100)


# Data Visualizations -----------------------------------------------------

theme_set(theme_bw())

# Games played by year

ggplot(results, aes(x = year(date))) +
  geom_bar()

# Score differential bar chart

ggplot(results, aes(x = home_score - away_score)) +
  geom_bar() +
  scale_x_continuous(limits = c(-10,10))

