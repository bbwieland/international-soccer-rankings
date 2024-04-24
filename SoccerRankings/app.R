# Global Variables:
library(shiny)
library(shinythemes)
library(readr)
library(reactable)
library(reactablefmtr)
library(markdown)
library(dplyr)
library(devtools)
library(bivpois)
library(purrr)
library(janitor)
library(magrittr)
library(tidyverse)
library(lme4)

clean_rankings <- function(df) {
  # Purpose: pre-processes the `rankings` dataframe for clean output.
  output <- df %>%
    mutate(o_rk = dense_rank(-o_effect),
           d_rk = dense_rank(d_effect),
           n_rk = dense_rank(-net_effect)) %>%
    select(team, 
           n_val = net_effect, n_rk, 
           o_val = o_effect, o_rk, 
           d_val = d_effect, d_rk) %>%
    arrange(n_rk)
}

react_rankings <- function(df, rank_width = 50, rank_font = 14, val_width = 100, val_font = 18) {
  reactable(
    df,
    theme = table_theme(),
    pagination = FALSE,
    searchable = TRUE,
    highlight = TRUE,
    language = reactableLang(searchPlaceholder = "Filter by team...",
                             noData = "Enter a valid team name."),
    columns = list(
      o_rk = colDef(name = "",
                    width = rank_width,
                    style = cell_style(font_size = rank_font, vertical_align = "center"),
                    align = "center",
                    sortable = FALSE),
      d_rk = colDef(name = "",
                    width = rank_width,
                    style = cell_style(font_size = rank_font, vertical_align = "center"),
                    align = "center",
                    sortable = FALSE),
      n_rk = colDef(name = "",
                    width = rank_width,
                    style = cell_style(font_size = rank_font, vertical_align = "center"),
                    align = "center",
                    sortable = FALSE),
      o_val = colDef(name = "OFF RTG",
                     width = val_width,
                     cell = net_rating_format,
                     defaultSortOrder = "desc"),
      d_val = colDef(name = "DEF RTG",
                     width = val_width,
                     cell = net_rating_format,
                     defaultSortOrder = "asc"),
      n_val = colDef(name = "NET RTG",
                     width = val_width,
                     cell = net_rating_format,
                     defaultSortOrder = "desc"),
      team = colDef(name = "Team",
                    width = 300)
    )
  )
}

# Utility Functions -------------------------------------------------------

table_theme <- function() {
  font_size = 18
  font_color = "#222222"
  header_font_size = 14
  header_font_color = "#000000"
  cell_padding = 5
  centered_content = NULL
  
  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderWidth = "1px",
    borderColor = "#dddddd",
    stripedColor = "#dddddd",
    highlightColor = "#f0f0f0",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size,
                      borderBottom = "3px solid #222222"),
    headerStyle = list(
      borderWidth = "3px",
      paddingTop = "12px",
      verticalAlign = "bottom",
      textAlign = "bottom",
      background = "#ffffff",
      textTransform = "uppercase",
      borderColor = "#222222",
      color = header_font_color,
      `&:hover` = list(background = "#dddddd"),
      `&[aria-sort='ascending'], &[aria-sort='descending']` = list(background = "#5b5e5f",
                                                                   color = "#ffffff"),
      borderColor = "#333",
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      `&:not(:empty)` = list(
        paddingBottom = "3px",
        verticalAlign = "bottom",
        textAlign = "bottom",
        backgroundColor = "#ffffff",
        textTransform = "uppercase",
        fontSize = header_font_size,
        color = font_color
      )
    ),
    inputStyle = list(
      backgroundColor = "#ffffff",
      color = "#222222"
    ),
    rowSelectedStyle = list(backgroundColor = "#dddddd"),
    pageButtonStyle = list(textTransform = "uppercase",
                           fontSize = "14px"),
    paginationStyle = list(textTransform = "uppercase",
                           fontSize = "14px"),
    searchInputStyle = list(
      paddingLeft = "0.5rem",
      paddingTop = "0.5rem",
      paddingBottom = "0.5rem",
      width = "100%",
      border = "none",
      backgroundColor = "white",
      backgroundSize = "1rem",
      backgroundPosition = "left 0.5rem center",
      backgroundRepeat = "no-repeat",
      "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
      "&:hover::placeholder, &:focus::placeholder" = list(color = "#222222"),
      fontSize = font_size
    )
  )
}

net_rating_format <- function(x) {
  sprintf("%+.2f", x)
}

percent_format <- function(x) {
  sprintf("%1.1f%%", 100*x)
}

build_match_dataviz <- function(results_df, team1, team2, location) {
  
  title_string <- paste0("\n",team1, " vs. ", team2)
  
  p_w <- results_df[which(results_df$result == "W"), "lik_sum", drop = TRUE] %>% percent_format()
  p_d <- results_df[which(results_df$result == "D"), "lik_sum", drop = TRUE] %>% percent_format()
  p_l <- results_df[which(results_df$result == "L"), "lik_sum", drop = TRUE] %>% percent_format()
  
  subtitle_string <- paste0("\n","Match Location: ", location, "\n","Win: ", p_w, " | Draw: ", p_d, " | Loss: ", p_l)
  caption_string = "Created via Team JUDE's international soccer ratings algorithm & R Shiny application."
  
  ggplot(results_df, aes(fill = result, x = lik_sum, y = placeholder)) +
    geom_col(show.legend = FALSE, color = "white") +
    theme_void(base_family = "Chivo") +
    scale_fill_manual(values = c("L" = "#cc7cde", "D" = "#e0e0e0", "W" = "#6ceb70")) +
    labs(title = title_string,
         subtitle = subtitle_string,
         caption = caption_string) +
    theme(plot.title = element_text(face = "bold", size = 36, hjust = 0.5),
          plot.subtitle = element_text(face = "bold", size = 20, hjust = 0.5),
          plot.caption = element_text(size = 16, hjust = 0.5))
  
}

# import Data -------------------------------------------------------------

rankings <- read_csv("https://raw.githubusercontent.com/bbwieland/international-soccer-rankings/main/ModelRankings.csv") %>% clean_rankings()

off_model <- readRDS(gzcon(url("https://raw.github.com/bbwieland/international-soccer-rankings/main/OffModel.RDS")))
def_model <- readRDS(gzcon(url("https://raw.github.com/bbwieland/international-soccer-rankings/main/DefModel.RDS")))

# Prediction Function -----------------------------------------------------

clip_predictions <- function(x) {
  ifelse(x < 0.05, 0.05, x)
}

predict_match <- function(team1, team2, location) {
  # location = (1 = home, 0 = neutral, -1 = away)
  
  location_numeric <- case_when(
    location == "Home" ~ 1,
    location == "Neutral" ~ 0,
    location == "Away" ~ -1,
    TRUE ~ NA)
  
  prediction_df <- data.frame(team = team1, opponent = team2, location = location_numeric)
  team_goals <- predict(off_model, prediction_df, type = "response") %>% clip_predictions()
  opp_goals <- predict(def_model, prediction_df, type = "response") %>% clip_predictions()
  
  go_to = 10
  
  goals <- expand.grid(seq(0,go_to), seq(0,go_to))
  
  lik <- map2_vec(.x = goals$Var1, .y = goals$Var2, .f = ~ dbp(
    x1 = .x, x2 = .y, lambda = c(team_goals, opp_goals, 0), logged = FALSE
  ))
  
  likelihoods <- cbind(goals, lik)

  likelihoods <- likelihoods %>%
    mutate(result = case_when(
      Var1 > Var2 ~ "W",
      Var1 == Var2 ~ "D",
      Var1 < Var2 ~ "L",
      TRUE ~ NA
    )) %>%
    mutate(lik = ifelse(result == "D", lik * 1.1, lik))

  # rescaling to be valid PMF
  
  scale_factor <- 1 / sum(likelihoods$lik)
  
  likelihoods$lik <- likelihoods$lik * scale_factor
  
  match_probs <- likelihoods %>% 
    group_by(result) %>% 
    summarise(lik_sum = sum(lik)) %>%
    mutate(result = factor(result, levels = c("L","D","W")))
  
  return(match_probs)
}

# Necessary Variables -----------------------------------------------------

teams <- unique(rankings$team)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("JUDE International Soccer Ratings", windowTitle = "JUDE Ratings"),
  tabsetPanel(
    tabPanel("Rankings",
             reactableOutput("homepage")),
    tabPanel("Projections",
             sidebarLayout(
               sidebarPanel(
                 selectInput("team1", "Select the first team:", choices = teams, selected = "Argentina"),
                 selectInput("team2", "Select the second team:", choices = teams, selected = "France"),
                 selectInput("location", "Select the game location:", choices = c("Home","Neutral","Away"), selected = "Neutral")
               ),
               mainPanel(
                 #tableOutput("prediction"),
                 plotOutput("prediction_bar")
               )
             )),
    tabPanel("Methodology")
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  rankings_output <- react_rankings(rankings)
  output$homepage <- renderReactable(rankings_output)
  
  match_prediction <- reactive({predict_match(input$team1, input$team2, input$location)})
  # output$prediction <- renderTable(match_prediction())
  output$prediction_bar <- renderPlot(build_match_dataviz(match_prediction() %>% mutate(placeholder = ""),
                                                          input$team1, input$team2, input$location))
}

# Run the application 
shinyApp(ui = ui, server = server)
