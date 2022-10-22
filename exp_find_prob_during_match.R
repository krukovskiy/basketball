library(stringr)
library(dplyr)
library(ggplot2)

# Let's try to evaluate match events probability

# Load data 

# Load game logs from a file
gameLog <- readRDS("NBA2122.RData")

# Input data of the game
matchDate = "April 22, 2022"
team_name = "Phoenix Suns"


######## GET teams by name (only when away)
indexes = NULL
i = 1
while (i < length(gameLog))
{
  ii <- gameLog[[i]][["gameSummary"]]$team1 == team_name & grepl(matchDate, gameLog[[i]][["gameSummary"]]$date)
  indexes <- append(indexes, ii)
  i = i+1
}

# Detect ID of games
gameIds <- which(indexes == TRUE)

# Initialize variable
teamLog = data.frame()

## Get particular game log
j = 1
while(j < (length(gameIds) + 1))
{
  
  ## Loop for for each peak of the game
  i = 1
  while (i < nrow(gameLog[[gameIds[j]]]$gamelog))
  {
    
    # Add summary and gamelog
    t1 = cbind(gameLog[[gameIds[j]]]$gamelog[i,], gameLog[[gameIds[j]]]$gameSummary)
    colnames(t1)[2] = "team_away"
    colnames(t1)[3] = "team_home"
    teamLog <- rbind(teamLog, t1)
    t1 = NULL
    i = i+1
  }
  j = j+1
}

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

# Extract player actions
action = data.frame(str_match(teamLog$summaryLog, "^[A-Z]\\. [a-zA-Z-]+ ([a-zA-Z-]+)|([a-zA-Z-]+ [a-zA-Z-]+) [by]|([a-zA-Z-]+) [by]|(timeout)"))

# Unite actions to one columns
action = action %>% mutate(action = coalesce(X2,X3,X4,X5))
teamLog = cbind(teamLog, action = action$action)

# Extract points
points = str_match(teamLog$summaryLog, "^[A-Z]\\. [a-zA-Z-]+ [a-zA-Z-]+ ([a-zA-Z123-]+ [a-zA-Z123-]+)")[,2]
teamLog = cbind(teamLog, points = points)

## Combine rows
teamLog[is.na(teamLog)] <- ""
t = data.frame(event = paste(teamLog$pname, teamLog$action, teamLog$points, sep=" "))
teamLog = cbind(teamLog, t)

# Add columns for probabilities
teamLog$prob_up = 0
teamLog$prob_down = 0
# Now we assign probabilities to a particular game (log)
# We make a loop where we find events in our prepared file of probabilities (phoenix_results.rds)
all_stats = readRDS("phoenix_results.rds")

# Add columns to team log

i = 1
while (i < nrow(teamLog))
{
  # find probability of the event in the overall dataframe (if exists)
  if (length(all_stats[grepl(teamLog$event[i], all_stats$event),]$prob_up) > 0)
  {
    teamLog$prob_up[i] = all_stats[grepl(teamLog$event[i], all_stats$event),]$prob_up
    teamLog$prob_down[i] = all_stats[grepl(teamLog$event[i], all_stats$event),]$prob_down
  }
  i = i + 1
}


# Let's count current probability during a match
teamLog$curProb = 0
i = 6
while (i < nrow(teamLog))
{
  tt = teamLog[c((i-5):i),]
  teamLog$curProb[i] = sum(tt$prob_up) - sum(tt$prob_down)
  i=i+1
}



smt_prob <- loess.smooth(teamLog$Time, teamLog$curProb, span=0.2,
                    degree = 2, evaluation = 100)
plot(smt_prob, xlim = c(0,3000))


