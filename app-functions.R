library(shiny)
library(shinythemes)
library(readr)
library(reactable)
library(reactablefmtr)
library(markdown)
library(dplyr)

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

net_rating_format <- function(x)
  sprintf("%+.2f", x)