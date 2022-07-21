library(XML)
library(dplyr)
library(tidyr)
library(EloRating)
library(stringr)
library(hms)
library(lubridate)


# The main goal of this project is to find the win predictors

# Predcessors
content <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(content) <- c("time", "team1", "scores1", "team2", "scores2", "boxscore", "OTs", "Append", "Notes", "Date")

# Get data
#https://www.basketball-reference.com/leagues/NBA_2021_games-december.html
#https://www.basketball-reference.com/leagues/NBA_2021_games-january.html
URL <- "https://www.basketball-reference.com/leagues/NBA_2021_games-december.html"
thepage = readLines(URL)
thepage3 = htmlTreeParse(thepage, useInternalNodes = T)
data <- xpathSApply(thepage3, "//tr//td", xmlValue)
head(data)
# Parse dates
dates <- xpathSApply(thepage3, "//tr//th//a", xmlValue)
Sys.setlocale("LC_TIME", "C")
new_dates <- as.Date(dates, "%a, %b %d, %Y")
# Create preprocessed content data frame and add date column
preprocessed_content <- as.data.frame(matrix(data, ncol = 9, byrow = TRUE))
preprocessed_content$date <- new_dates

# Create content frame
content <- rbind(content, preprocessed_content)

