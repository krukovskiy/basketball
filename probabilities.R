library(tidyr)
library(dplyr)

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

# Extract player actions
action = data.frame(str_match(teamLog$summaryLog, "^[A-Z]\\. [a-zA-Z-]+ ([a-zA-Z-]+)|([a-zA-Z-]+ [a-zA-Z-]+) [by]|([a-zA-Z-]+) [by]|(timeout)"))

# Unite actions to one columns
action = action %>% mutate(action = coalesce(X2,X3,X4,X5))
teamLog = cbind(teamLog, action = action$action)

# Extract points
points = str_match(teamLog$summaryLog, "^[A-Z]\\. [a-zA-Z-]+ [a-zA-Z-]+ ([a-zA-Z123-]+ [a-zA-Z123-]+)")[,2]
teamLog = cbind(teamLog, points = points)

# Get stat of all the season
statP_all <- ddply(teamLog, .(teamLog$pname, teamLog$action, teamLog$points), nrow)
names(statP_all) <- c("Player", "Action", "Points", "Freq")
statP_all = statP_all %>% arrange(desc(Freq))

# Remove rows when all of them are NA
statP_all = statP_all[!is.na(statP_all$Player) | !is.na(statP_all$Action) | !is.na(statP_all$Points), ]

# Adjust NA
statP_all <- sapply(statP_all, as.character)
statP_all[is.na(statP_all)] <- " "
statP_all = as.data.frame(statP_all)

# Add Type NEUTRAL
stat_comp_neu = data.frame(event = paste(statP_all$Player, statP_all$Action, statP_all$Points, sep=" "))
stat_comp_neu = cbind(stat_comp_neu, Freq = statP_all$Freq, Type = "NEUTRAL")

# Get stat comp full
stat_comp_full = getTrendsPredecessors(gameLog, team_name)

# Bind to peaks table
stat_comp_full = rbind(stat_comp_full, stat_comp_neu)

# Add factor to a type
stat_comp_full$Type = as.factor(stat_comp_full$Type) 


stat_comp_up2 = subset(stat_comp_full, Type=='UP')
stat_comp_down2 = subset(stat_comp_full, Type=='DOWN')
stat_comp_neu = merge(stat_comp_up2, stat_comp_neu, by = "event")    
stat_comp_all = merge(stat_comp_neu, stat_comp_down2, by = "event")    
colnames(stat_comp_all) <- c("event", "freq_up", "type_up", "freq_neu", "type_neu", "freq_down", "type_down")
stat_comp_all$freq_neu = as.numeric(stat_comp_all$freq_neu)
stat_comp_all$freq_up = as.numeric(stat_comp_all$freq_up)
stat_comp_all$freq_down = as.numeric(stat_comp_all$freq_down)

# Count probability of events by dividing frequency on sum of frequencies
prob_up = stat_comp_all$freq_up  /  (stat_comp_all$freq_neu + stat_comp_all$freq_up + stat_comp_all$freq_down)
prob_down = stat_comp_all$freq_down  /  (stat_comp_all$freq_neu + stat_comp_all$freq_up + stat_comp_all$freq_down)
stat_comp_all = cbind(stat_comp_all, prob_up = prob_up, prob_down = prob_down)

# save result data
saveRDS(stat_comp_all, file = "phoenix_results.rds")
tt = readRDS("phoenix_results.rds")
# Some exporations
# Plot comparison graph
stat_to_plot = data.frame(event = stat_comp_all$event, type = "UP", freq = stat_comp_all$freq_up)
stat_to_plot = (rbind(stat_to_plot, data.frame(event = stat_comp_all$event, type = "DOWN", freq = stat_comp_all$freq_down)))
stat_to_plot = (rbind(stat_to_plot, data.frame(event = stat_comp_all$event, type = "NEUTRAL", freq = stat_comp_all$freq_neu)))

# Good graph
g = ggplot() +
   geom_bar(stat_to_plot,         # Draw barplot with grouping & stacking
          mapping = aes(x = reorder(event, -freq),
                     y = freq,
                     fill = type), stat = "identity",
                   position = "stack") +
    geom_line(stat_comp_all, mapping = aes(x = event, y = prob_up*2000, group = 1), color = "#009E73", size = 1)+
    geom_line(stat_comp_all, mapping = aes(x = event, y = -prob_down*2000, group = 1), color = "#D55E00", size = 1)+
    scale_y_continuous("Frequency", sec.axis = sec_axis(~ (. /20), name = "probability to go up or down, %"))+
    theme_bw(base_size = 9) +
    theme(axis.text.x=element_text(angle=90,hjust=1))
        