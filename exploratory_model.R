library("lubridate")
library(plyr)
library(ggplot2)
library(dplyr)
# Explorations


# Load game logs from a file
gameLog <- readRDS("NBA2122.RData")




# Filter teams by name (only when away)
indexes = NULL

i = 1
while (i < length(gameLog))
{
  ii <- gameLog[[i]][["gameSummary"]]$team1 == "Phoenix Suns"  
  indexes <- append(indexes, ii)
  i = i+1
}

# Detect ID of games
gameIds <- which(indexes == TRUE)

# Create dataframe with all peaks in the season
sPeaks = NULL
i = 1
 while (i < length(gameIds))
 {
   t1 <- cbind(gameLog[[gameIds[i]]]$gamePeaks, 
               gameLog[[gameIds[i]]]$gameSummary,
               gameId = gameIds[i])
   sPeaks <- rbind(sPeaks, t1)
   
   i = i+1
   t1 = NULL
 }


######################################################################
# Filter teams by name (only when home)

indexes = NULL

i = 1
while (i < length(gameLog))
{
  ii <- gameLog[[i]][["gameSummary"]]$team2 == "Phoenix Suns"  
  indexes <- append(indexes, ii)
  i = i+1
}

# Detect ID of games
gameIds <- which(indexes == TRUE)


# Cbind dataframe with all peaks in the season
i = 1
while (i < length(gameIds))
{
  t2 <- cbind(gameLog[[gameIds[i]]]$gamePeaks, 
              gameLog[[gameIds[i]]]$gameSummary,
              gameId = gameIds[i])
  sPeaks <- rbind(sPeaks, t2)
  
  i = i+1
  t2 = NULL
}

# Convert date time
Sys.setlocale("LC_TIME", "English")
sPeaks$date <- as.Date(sPeaks$date,"%I:%M %p, %B %d, %Y" )

###################################################################
# Explorations with Phoenix

## Subset only Phx
team1 <- subset(sPeaks, team=='Phoenix')

# Set win flag

sWinner <- ifelse ((team1$team1 == "Phoenix Suns" & team1$winner1 == 1)|(team1$team2 == "Phoenix Suns" & team1$winner2 == 1),
        1, 0)
# Add win flag to the data frame team1
team1 <- cbind(team1, sWinner)




# Create win table
teamPeaks <- team1 %>% distinct(date, sWinner) %>%  arrange(date)
teamPeaks <- cbind(teamPeaks, peakCount = count(team1$date)$freq)


# Create general linear model
logPhx = glm(sWinner ~ peakCount, data = teamPeaks, family="binomial")
summary(logPhx)
# Plot model
plot(teamPeaks$peakCount,logPhx$fitted,pch=19,col="steelblue",xlab="Key moments per game",ylab ="Win probability")

# Plot base graph with confidence interval
p3 <- ggplot(data = teamPeaks, aes(x = peakCount, y = sWinner)) 
p3 <- p3 + geom_point(color = "steelblue", size = 4, alpha = 0.1)+ geom_smooth(method = "glm")
p3 + labs( x = "Key moments per game", y = "Win probability")


p3

# Explorations
team1na <- na.omit(team1)
fit <- glm(sWinner ~ place + peak_ppm + score1 + score2, data = team1na)
summary(fit)

count(team1$date)

# Plot peaks strength of time
p4 <- ggplot(data = team1, aes(x = peak_time, y = trend_strength), xlab = "match time", ylab = "peak ppm")
p4 + geom_point(color = 1 +team1$sWinner, alpha = .5)
