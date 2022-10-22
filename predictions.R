# Here we develop prediction models
library(rpart)
library(tidyr)
library(dplyr)
library(plyr)
library(stringi)
library(stringr)
# Starting input
gameLog <- readRDS("NBA2122.RData")
team_name = "Phoenix Suns"

# Call function getGameLogFromFile.R
teamLog = getGameLogFromFile(gameLog, team_name)

# Now we gonna scrap all events of the dataset

# Change accented values
teamLog$team_home = stri_trans_general(teamLog$team_home, "Latin-ASCII")  
teamLog$team_away = stri_trans_general(teamLog$team_away, "Latin-ASCII")  

# Unite all events to one column
teamLog$summaryLog <- paste(teamLog$team_home,teamLog$team_away, sep= "")
teamLog$summaryLog = str_trim(teamLog$summaryLog, "left")

# Find player name
player_name = str_extract(teamLog$summaryLog, "^[A-Z]\\. ([a-zA-Z-]+)|[^by ]*[A-Z-]\\. [a-zA-Z-]+")

# Away Extract team names
away_team = data.frame(str_match(teamLog$team_away, "^([a-zA-Z]+) full timeout|[a-zA-Z-]+ [a-zA-Z-]+ by ([a-zA-Z]+)$|[Violation|Turnover] by ([a-zA-Z]+)"))
# Replace team names
t = !is.na(away_team$X3)
tt = which(t == TRUE)
away_team$X3[tt] = teamLog$team1[tt]
# Replace team names
t = !is.na(away_team$X2)
tt = which(t == TRUE)
away_team$X2[tt] = teamLog$team1[tt]
# Replace team names
t = (away_team$X4 == "Team")
tt = which(t == TRUE)
away_team$X4[tt] = teamLog$team1[tt]
tt = which(t == FALSE)
away_team$X4[tt] = NA
away_team = away_team %>% mutate(finalized = coalesce(X2, X3, X4))

# Home Extract team names
home_team = data.frame(str_match(teamLog$team_home, "^([a-zA-Z]+) full timeout|[a-zA-Z-]+ [a-zA-Z-]+ by ([a-zA-Z]+)$|[Violation|Turnover] by ([a-zA-Z]+)"))
# Replace team names
t = !is.na(home_team$X3)
tt = which(t == TRUE)
home_team$X3[tt] = teamLog$team2[tt]
# Replace team names
t = !is.na(home_team$X2)
tt = which(t == TRUE)
home_team$X2[tt] = teamLog$team2[tt]
# Replace team names
t = (home_team$X4 == "Team")
tt = which(t == TRUE)
home_team$X4[tt] = teamLog$team2[tt]
tt = which(t == FALSE)
home_team$X4[tt] = NA
home_team = home_team %>% mutate(finalized = coalesce(X2, X3, X4))
t3 = data.frame(cbind(player = player_name, home = home_team$finalized, away = away_team$finalized))
t3 = t3 %>% mutate(pname = coalesce(player, home, away))
teamLog = cbind(teamLog, pname = t3$pname)

# Change colnames teamlog
colnames(teamLog)[4] = "curscore1"
colnames(teamLog)[5] = "curscore2"

# Extract player actions
action = data.frame(str_match(teamLog$summaryLog, "^[A-Z]\\. [a-zA-Z-]+ ([a-zA-Z-]+)|([a-zA-Z-]+ [a-zA-Z-]+) [by]|([a-zA-Z-]+) [by]|(timeout)"))

# Unite actions to one columns
action = action %>% mutate(action = coalesce(X2,X3,X4,X5))
teamLog = cbind(teamLog, action = action$action)

# Extract points
points = str_match(teamLog$summaryLog, "^[A-Z]\\. [a-zA-Z-]+ [a-zA-Z-]+ ([a-zA-Z123-]+ [a-zA-Z123-]+)")[,2]
teamLog = cbind(teamLog, points = points)

# Calculate extended peaks for the selected team
extPeaks = getExtendedPeaks(gameLog, team_name)

# Encoding has crucial effect on the results
extPeaks$team_home = stri_trans_general(extPeaks$team_home, "Latin-ASCII")  
extPeaks$team_away = stri_trans_general(extPeaks$team_away, "Latin-ASCII")  

# For extPeaks unite all events to one column
extPeaks$summaryLog <- paste(extPeaks$team_home,extPeaks$team_away, sep= "")
extPeaks$summaryLog = str_trim(extPeaks$summaryLog, "left")
# Change accented values


# Merge team log and peaks
# Subset extPeaks
sextPeaks = extPeaks[, c("Time", "date", "summaryLog", "peak_time", "peak_ppm", "team", "trend_duration", "trend_strength", "trendType")]

# Generate combined log
combLog = merge(sextPeaks, teamLog, by.x = c("Time", "date", "summaryLog"), by.y = c("Time", "date", "summaryLog"), all.y = TRUE)

# Adjusting values
combLog$pname = as.factor(combLog$pname)
combLog$trendType[is.na(combLog$trendType)] <- "NEUTRAL"
combLog$trendType = as.factor(combLog$trendType)

# Now we construct predictive models
tt2 <- rpart(trendType ~ pname, data = combLog)
