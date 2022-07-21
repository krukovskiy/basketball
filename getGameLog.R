library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)

# Create function of getting data

getGameLog <- function(URL)
{

  # Get data
  
  # URL = "https://www.basketball-reference.com/boxscores/pbp/202110190LAL.html"
  page = read_html(URL)
  
  # Parse html page
  rank_data_html = page %>%  html_nodes(".thead .center") %>% html_text()
  rank_cols <- c(rank_data_html[1], rank_data_html[2],rank_data_html[6], rank_data_html[4])
  time <- page %>%  html_nodes("td:nth-child(1)") %>% html_text()
  time
  team1 <- page %>%  html_nodes("#pbp td:nth-child(2)") %>% html_text()
  team2 <- page %>%  html_nodes("td.center:nth-child(2) , .center~ td+ td") %>% html_text()
  score <- page %>%  html_nodes("tr~ tr+ tr td.center , td~ td+ .center") %>% html_text()
  
  game_log <- data.frame(time, team1, team2, score)
  
  
  # add team names to the log
  colnames(game_log) <- rank_cols
  
  # separate score by 2
  game_log <- game_log %>% separate(Score, into = c("score1", "score2"), sep = "-", )
  
  # Make good time column
  default_time <- as.double(ms("12:00"))
  logged_time <- as.double(ms(game_log$Time))
  game_log$Time <- default_time - logged_time
  
  # Find index of the end of 1 quarter
  index_1q <- which(game_log[,2] == "End of 1st quarter")
  # Delete rows with index and index + 1
  game_log <- game_log[-c(index_1q, index_1q +1),]
  
  # Find index of the end of 2 quarter
  index_2q <- which(game_log[,2] == "End of 2nd quarter")
  # Delete rows with index and index + 1
  game_log <- game_log[-c(index_2q, index_2q +1),]
  
  # Changing time of the 2 quarter
  game_log[c((index_1q):nrow(game_log)), 1] <- game_log[c((index_1q):nrow(game_log)), 1] + 720
  
  # Find index of the end of 3rd quarter
  index_3q <- which(game_log[,2] == "End of 3rd quarter")
  # Delete rows with index and index + 1
  game_log <- game_log[-c(index_3q, index_3q +1),]
  # Changing time of the 3 quarter
  game_log[c((index_2q):nrow(game_log)), 1] <- game_log[c((index_2q):nrow(game_log)), 1] + 720
  
  # Find index of the end of 4rd quarter
  index_4q <- which(game_log[,2] == "End of 4th quarter")
  # Delete rows with index and index + 1
  game_log <- game_log[-c(index_4q, index_4q +1),]
  # Changing time of the 4 quarter
  game_log[c((index_3q):nrow(game_log)), 1] <- game_log[c((index_3q):nrow(game_log)), 1] + 720
  
  # Adjusting scoring columns
  game_log$score1 <- as.numeric(game_log$score1); game_log$score1[1] <- 0 
  game_log$score2 <- as.numeric(game_log$score2); game_log$score2[1] <- 0
  
  # Calculate points per minute distribution
  
  game_log <- game_log %>%  mutate(ppm1 = score1 / Time * 60)
  game_log <- game_log %>%  mutate(ppm2 = score2 / Time * 60)
}