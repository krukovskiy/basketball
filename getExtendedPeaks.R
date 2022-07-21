library('ggspectra')
library("ggpmisc")
library(dplyr)
library(tidyr)


###########################################
# This works with one team that we select #
###########################################


# Make a function to calculate log 

getExtendedPeaks = function(gameLog, team_name)
{
# Initialize variable
extPeaks = data.frame()
  
# Filter teams by name (only when away)
indexes = NULL

i = 1
while (i < length(gameLog))
{
  ii <- gameLog[[i]][["gameSummary"]]$team1 == team_name  
  indexes <- append(indexes, ii)
  i = i+1
}

# Detect ID of games
gameIds <- which(indexes == TRUE)

## Loop for each game in the log
j = 1
while(j < length(gameIds))
{

      ## Loop for for each peak of the game
      i = 1
      while (i < nrow(gameLog[[gameIds[j]]]$gamePeaks))
      {
      # Select peak time
      peakTime  = gameLog[[gameIds[j]]]$gamePeaks[i,1]
      
      # Get last minute moments before peak
      moments = gameLog[[gameIds[j]]]$gamelog  %>% filter(Time > peakTime & Time <= peakTime + 30)
      
      # Change columns names of teams to bind correctly
      colnames(moments)[2] = "team_away"
      colnames(moments)[3] = "team_home"
      
      # Add moments to peaks
      if (nrow(moments) > 0)
      {t1 = cbind(gameLog[[gameIds[j]]]$gamePeaks[i,],moments, gameLog[[gameIds[j]]]$gameSummary)
      
      # Add type peaks
      t1$trendType <- ifelse(t1$trend_strength > 0, "UP", "DOWN")
      extPeaks <- rbind(extPeaks, t1)
      }
      t1 = NULL
      i = i+1
      }
      j = j+1
}

# Filter teams by name (only when home)
indexes = NULL

i = 1
while (i < length(gameLog))
{
  ii <- gameLog[[i]][["gameSummary"]]$team2 == team_name 
  indexes <- append(indexes, ii)
  i = i+1
}

# Detect ID of games
gameIds <- which(indexes == TRUE)

## Loop for each game in the log
j = 1
while(j < length(gameIds))
{
  
  ## Loop for for each peak of the game
  i = 1
  while (i < nrow(gameLog[[gameIds[j]]]$gamePeaks))
  {
    # Select peak time
    peakTime  = gameLog[[gameIds[j]]]$gamePeaks[i,1]
    
    # Get last minute moments before peak
    moments = gameLog[[gameIds[j]]]$gamelog  %>% filter(Time > peakTime & Time <= peakTime + 30)
    
    # Change columns names of teams to bind correctly
    colnames(moments)[2] = "team_away"
    colnames(moments)[3] = "team_home"
    
    if (nrow(moments) > 0)
    {t1 = cbind(gameLog[[gameIds[j]]]$gamePeaks[i,],moments, gameLog[[gameIds[j]]]$gameSummary)
    
    # Add type peaks
    t1$trendType <- ifelse(t1$trend_strength > 0, "UP", "DOWN")
    extPeaks <- rbind(extPeaks, t1)
    }
    t1 = NULL
    i = i+1
  }
  j = j+1
}

# Subset only Phoenix events
extPeaks = subset(extPeaks, team == "Phoenix" )
extPeaks
}

