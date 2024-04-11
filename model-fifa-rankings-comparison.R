library(tidyverse)

fifa <- read_csv("FifaRankingsPreWC.csv")
preds <- read_csv("WorldCup2022Predictions.csv")
ranks_raw <- read_csv("ModelRankings.csv")

ranks <- ranks_raw %>%
  arrange(-net_effect) %>%
  mutate(ModelRank = row_number())

rank_merge <- inner_join(ranks, fifa, by = c("team" = "PredName")) %>%
  select(team, ModelRank, FifaRank)

model_eval <- preds %>%
  left_join(rank_merge, by = c("team" = "team")) %>%
  left_join(rank_merge, by = c("opponent" = "team"), suffix = c("Team","Opp")) %>%
  select(team, opponent, result, score, opp_score, contains("is"), contains("Rank")) %>%
  mutate(is_higher_fifa = as.numeric(FifaRankTeam < FifaRankOpp),
         is_higher_model = as.numeric(ModelRankTeam < ModelRankOpp))

table(model_eval$is_higher_fifa, model_eval$is_higher_model)

library(ggrepel)

ggplot(rank_merge, aes(x = ModelRank, y = FifaRank)) +
  geom_point() +
  coord_equal() +
  geom_abline(linetype = "dashed") +
  theme_classic() +
  geom_text_repel(aes(label = team), max.overlaps = 1, seed = 4133) +
  scale_x_continuous(breaks = seq(0,80,10)) +
  scale_y_continuous(breaks = seq(0,60,10)) +
  labs(x = "Model Ranking", y = "FIFA World Ranking",
       title = "Model Rankings vs. FIFA Rankings Entering 2022 World Cup")
