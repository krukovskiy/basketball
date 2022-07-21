library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)

getGameSummary <- function(URL)
{  
# Get Web page
#URL = "https://www.basketball-reference.com/boxscores/pbp/202110190LAL.html"
page = read_html(URL)


# Get game date
gameDate = page %>%  html_nodes(".scorebox_meta div:nth-child(1)") %>% html_text()

# Get game place
gamePlace = page %>%  html_nodes(".scorebox_meta div:nth-child(2)") %>% html_text()

# Get teams
teams = page %>%  html_nodes("div:nth-child(1) div strong a") %>% html_text()

# Get scores
scores = page %>%  html_nodes("div:nth-child(1) .score") %>% html_text()

# Convert scores to numeric
scores = as.numeric(scores)

# Calculate winner
if (scores[1] > scores[2])
{
  winner1 = 1; winner2 = 0
} else
{
  winner1 = 0; winner2 = 1
}


# Create summary table
summaryStats <- data.frame(date = gameDate, 
                           place = gamePlace,
                           team1 = teams[1],
                           team2 = teams[2],
                           score1 = scores[1],
                           score2 = scores[2],
                           winner1 = winner1,
                           winner2 = winner2)
summaryStats
#gameLogs[[i]] <- list(gameLogs[[i]], summaryStats = summaryStats )
#gameLogs[[i]]
}