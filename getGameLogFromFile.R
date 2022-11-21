
# Load game logs from a file.
gameLog <- readRDS("NBA2122.RData")

team_name = "Phoenix Suns"
getGameLogFromFile = function(gameLog, team_name)
{
  
  
  # Initialize variable
  teamLog = data.frame()
  
  ######## GET teams by name (only when away)
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
###########################################################################################
######## GET teams by name (only when home)
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
  
  teamLog
}