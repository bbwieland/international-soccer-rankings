# Global Variables:

filepath <- "https://github.com/bbwieland/international-soccer-rankings/"

source(paste0(filepath, "app-functions.R"))

rankings <- read_csv(paste0(filepath, "ModelRankings.csv")) %>% clean_rankings()

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("JUDE International Soccer Ratings", windowTitle = "JUDE Ratings"),
  tabsetPanel(
    tabPanel("Rankings",
             reactableOutput("homepage")),
    tabPanel("Projections"),
    tabPanel("Methodology")
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  rankings_output <- react_rankings(rankings)
  output$homepage <- renderReactable(rankings_output)
}

# Run the application 
shinyApp(ui = ui, server = server)
