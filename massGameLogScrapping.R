library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)
library('ggspectra')
library("ggpmisc")

# Read link file
URL <- read.csv("f.csv")

# Add game logs to the one big list
i <- 1
gameLogs <- c()
while(i <= nrow(URL)) {
  gameLog  <- getGameLog(URL[i,])
  gameSummary <- getGameSummary(URL[i,])
  gamePeaks <- countPeaks(gameLog)
  gameLogs[[i]] <- list(gameSummary = gameSummary, gamelog = gameLog, gamePeaks = gamePeaks)
  i <- i + 1
}


# Save all data
saveRDS(gameLogs, file="NBA2122.RData")




# Read Rdata
gameLog <- readRDS("NBA2122.RData")



