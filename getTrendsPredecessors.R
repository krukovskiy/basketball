library("lubridate")
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
# Explorations


# Load game logs from a file
gameLog <- readRDS("NBA2122.RData")

team_name = "Phoenix Suns"

getTrendsPredecessors = function(gameLog, team_name)
{
# Calculate extended peaks for a certain team
extPeaks = getExtendedPeaks(gameLog, team_name)
# Change accented values
extPeaks$team_home = stri_trans_general(extPeaks$team_home, "Latin-ASCII")  
extPeaks$team_away = stri_trans_general(extPeaks$team_away, "Latin-ASCII")  

# Explorations
# Count occurrences. Subset when Phoenix is home
test = subset(extPeaks, extPeaks$team2 == team_name & extPeaks$trendType == "UP")

# Merge away and home logs to one column
test$summaryLog <- paste(test$team_home,test$team_away, sep= "")
test$summaryLog = str_trim(test$summaryLog, "left")

# Count the number of occurrences
counts <- ddply(test, .(test$team_home, test$trendType), nrow)
counts = counts[-c(1,2,3),]
names(counts) <- c("Event", "Type", "Freq")
counts = counts %>% arrange(desc(Freq))


# Extraction players
# Extract player names
player_name = str_extract(test$summaryLog, "^[A-Z]\\. ([a-zA-Z-]+)|[^by ]*[A-Z-]\\. [a-zA-Z-]+")

# Away Extract team names
away_team = data.frame(str_match(test$team_away, "^([a-zA-Z]+) full timeout|[a-zA-Z-]+ [a-zA-Z-]+ by ([a-zA-Z]+)$|[Violation|Turnover] by ([a-zA-Z]+)"))
# Replace team names
t = !is.na(away_team$X3)
tt = which(t == TRUE)
away_team$X3[tt] = test$team1[tt]
# Replace team names
t = !is.na(away_team$X2)
tt = which(t == TRUE)
away_team$X2[tt] = test$team1[tt]
# Replace team names
t = (away_team$X4 == "Team")
tt = which(t == TRUE)
away_team$X4[tt] = test$team1[tt]
tt = which(t == FALSE)
away_team$X4[tt] = NA
away_team = away_team %>% mutate(finalized = coalesce(X2, X3, X4))

# Home Extract team names
home_team = data.frame(str_match(test$team_home, "^([a-zA-Z]+) full timeout|[a-zA-Z-]+ [a-zA-Z-]+ by ([a-zA-Z]+)$|[Violation|Turnover] by ([a-zA-Z]+)"))
# Replace team names
t = !is.na(home_team$X3)
tt = which(t == TRUE)
home_team$X3[tt] = test$team2[tt]
# Replace team names
t = !is.na(home_team$X2)
tt = which(t == TRUE)
home_team$X2[tt] = test$team2[tt]
# Replace team names
t = (home_team$X4 == "Team")
tt = which(t == TRUE)
home_team$X4[tt] = test$team2[tt]
tt = which(t == FALSE)
home_team$X4[tt] = NA
home_team = home_team %>% mutate(finalized = coalesce(X2, X3, X4))
t3 = data.frame(cbind(player = player_name, home = home_team$finalized, away = away_team$finalized))
t3 = t3 %>% mutate(pname = coalesce(player, home, away))
test = cbind(test, pname = t3$pname)

# Extract player actions
action = data.frame(str_match(test$summaryLog, "^[A-Z]\\. [a-zA-Z-]+ ([a-zA-Z-]+)|([a-zA-Z-]+ [a-zA-Z-]+) [by]|([a-zA-Z-]+) [by]|(timeout)"))

# Unite actions to one columns
action = action %>% mutate(action = coalesce(X2,X3,X4,X5))
test = cbind(test, action = action$action)

# Extract points
points = str_match(test$summaryLog, "^[A-Z]\\. [a-zA-Z-]+ [a-zA-Z-]+ ([a-zA-Z123-]+ [a-zA-Z123-]+)")[,2]
test = cbind(test, points = points)


# Statistics statP_UP

statP_UP <- ddply(test, .(test$pname, test$action, test$points), nrow)
names(statP_UP) <- c("Player", "Action", "Points", "Freq")
statP_UP = statP_UP %>% arrange(desc(Freq))


##########################################
# DOWN
##########################################

# Explorations
# Count occurrences. Subset when Phoenix is home
test = subset(extPeaks, extPeaks$team2 == team_name & extPeaks$trendType == "DOWN")

# Merge away and home logs to one column
test$summaryLog <- paste(test$team_home,test$team_away, sep= "")
test$summaryLog = str_trim(test$summaryLog, "left")

# Count the number of occurrences
counts <- ddply(test, .(test$team_home, test$trendType), nrow)
counts = counts[-c(1,2,3),]
names(counts) <- c("Event", "Type", "Freq")
counts = counts %>% arrange(desc(Freq))


# Extraction players
# Extract player names
player_name = str_extract(test$summaryLog, "^[A-Z]\\. ([a-zA-Z-]+)|[^by ]*[A-Z-]\\. [a-zA-Z-]+")

# Away Extract team names
away_team = data.frame(str_match(test$team_away, "^([a-zA-Z]+) full timeout|[a-zA-Z-]+ [a-zA-Z-]+ by ([a-zA-Z]+)$|[Violation|Turnover] by ([a-zA-Z]+)"))
# Replace team names
t = !is.na(away_team$X3)
tt = which(t == TRUE)
away_team$X3[tt] = test$team1[tt]
# Replace team names
t = !is.na(away_team$X2)
tt = which(t == TRUE)
away_team$X2[tt] = test$team1[tt]
# Replace team names
t = (away_team$X4 == "Team")
tt = which(t == TRUE)
away_team$X4[tt] = test$team1[tt]
tt = which(t == FALSE)
away_team$X4[tt] = NA
away_team = away_team %>% mutate(finalized = coalesce(X2, X3, X4))


# Home Extract team names
home_team = data.frame(str_match(test$team_home, "^([a-zA-Z]+) full timeout|[a-zA-Z-]+ [a-zA-Z-]+ by ([a-zA-Z]+)$|[Violation|Turnover] by ([a-zA-Z]+)"))
# Replace team names
t = !is.na(home_team$X3)
tt = which(t == TRUE)
home_team$X3[tt] = test$team2[tt]
# Replace team names
t = !is.na(home_team$X2)
tt = which(t == TRUE)
home_team$X2[tt] = test$team2[tt]
# Replace team names
t = (home_team$X4 == "Team")
tt = which(t == TRUE)
home_team$X4[tt] = test$team2[tt]
tt = which(t == FALSE)
home_team$X4[tt] = NA
home_team = home_team %>% mutate(finalized = coalesce(X2, X3, X4))

t3 = data.frame(cbind(player = player_name, home = home_team$finalized, away = away_team$finalized))
t3 = t3 %>% mutate(pname = coalesce(player, home, away))
test = cbind(test, pname = t3$pname)

# Extract player actions
action = data.frame(str_match(test$summaryLog, "^[A-Z]\\. [a-zA-Z-]+ ([a-zA-Z-]+)|([a-zA-Z-]+ [a-zA-Z-]+) [by]|([a-zA-Z-]+) [by]|(timeout)"))

# Unite actions to one columns
action = action %>% mutate(action = coalesce(X2,X3,X4,X5))
test = cbind(test, action = action$action)

# Extract points
points = str_match(test$summaryLog, "^[A-Z]\\. [a-zA-Z-]+ [a-zA-Z-]+ ([a-zA-Z123-]+ [a-zA-Z123-]+)")[,2]
test = cbind(test, points = points)


# Statistics

statP_DOWN <- ddply(test, .(test$pname, test$action, test$points), nrow)
names(statP_DOWN) <- c("Player", "Action", "Points", "Freq")
statP_DOWN = statP_DOWN %>% arrange(desc(Freq))



#############################################
# Play with collected data
#############################################

# Combine stats from 3 columns
# Remove rows when all of them are NA
statP_UP = statP_UP[!is.na(statP_UP$Player) | !is.na(statP_UP$Action) | !is.na(statP_UP$Points), ]
statP_DOWN = statP_DOWN[!is.na(statP_DOWN$Player) | !is.na(statP_DOWN$Action) | !is.na(statP_DOWN$Points), ]

# Adjust NA
statP_UP <- sapply(statP_UP, as.character)
statP_UP[is.na(statP_UP)] <- " "
statP_UP = as.data.frame(statP_UP)

statP_DOWN <- sapply(statP_DOWN, as.character)
statP_DOWN[is.na(statP_DOWN)] <- " "
statP_DOWN = as.data.frame(statP_DOWN)


stat_comp_up = data.frame(event = paste(statP_UP$Player, statP_UP$Action, statP_UP$Points, sep=" "))
stat_comp_up = cbind(stat_comp_up, Freq = statP_UP$Freq, Type = "UP")

stat_comp_down = data.frame(event = paste(statP_DOWN$Player[!is.na(statP_DOWN$Player)], statP_DOWN$Action[!is.na(statP_DOWN$Action)], statP_DOWN$Points[!is.na(statP_DOWN$Points)], sep=" "))
stat_comp_down = cbind(stat_comp_down, Freq = statP_DOWN$Freq, Type = "DOWN")

stat_comp_full = rbind(stat_comp_up, stat_comp_down)
stat_comp_full$Freq = as.numeric(stat_comp_full$Freq)

stat_comp_full2 = merge(stat_comp_down, stat_comp_up, by = "event")
colnames(stat_comp_full2) = c("event", "freq_up", "up", "freq_down", "down")
stat_comp_full
}

# Plot comparison graph
ggplot(stat_comp_full,aes(x=event, y = ifelse(Type == "UP", Freq, -Freq), fill=Type))+
  geom_bar(stat="identity", position="identity") +
  coord_flip() +
  labs(title = "Scoring trends predecessors",
       fill = "Trend Type",
       x = "Event",
       y = "Frequency") +
  scale_fill_brewer(palette = "Dark2")


ggplot(stat_comp_full,                         # Draw barplot with grouping & stacking
       aes(x = event,
           y = Freq,
           fill = Type)) + 
  geom_bar(stat = "identity",
           position = "stack") 

