library('ggspectra')
#library("ggpmisc")
library(dplyr)
library(tidyr)


#Prepare game log
i <- 1 # game index

# Looping data set
countPeaks <- function(gameLog)
{
  
  
  game_log_p <- gameLog %>% filter(Time > 120)
  
  
  # Smooth raw data to a graph
  smt1 <- loess.smooth(game_log_p$Time, game_log_p$ppm1, span=0.2,
              degree = 2, evaluation = 100)
  
  
  # Find peaks ppm by time
  peak_time1 <- smt1$x[ggpmisc:::find_peaks(smt1$y)]
  peak_ppm1 <- smt1$y[ggpmisc:::find_peaks(smt1$y)]
  
  
  
  # Find valleys ppm by time
  valley_time1 <- smt1$x[ggpmisc:::find_peaks(-smt1$y)]
  valley_ppm1 <- smt1$y[ggpmisc:::find_peaks(-smt1$y)]
  
  # Plot graph
  smt1_frame <- data.frame(smt1$x, smt1$y)
  #ggplot(data = smt1_frame, aes(x = smt1$x, y = smt1$y)) + geom_line() + stat_peaks(col = "red") + stat_valleys(col = "green")
  
  
  # Create peaks and valleys data frame
  
  peak_frame <- data.frame(peak_time = c(peak_time1, valley_time1), peak_ppm = c(peak_ppm1, valley_ppm1))
  
  
  # Add finish time
  peak_frame <- rbind(peak_frame, c(
    game_log_p$Time[nrow(game_log_p)],
    game_log_p$ppm1[nrow(game_log_p)],
    NA))
    
  # Add start time (2 min after start)
  peak_frame <- rbind(peak_frame, c(
    game_log_p$Time[1],
    game_log_p$ppm1[1],
    NA))
  
  # Add team1 name
  peak_frame <- cbind(peak_frame, team = names(game_log_p)[2])
  
  # Order extremumes by time
  peak_frame <- peak_frame[order(peak_frame$peak_time),]
  
  
  # Calculate peak strength
  
  trend_strength <- c(diff(peak_frame$peak_ppm), NA)
  trend_duration <- c(diff(peak_frame$peak_time), NA)
  
  # Add trends to peak table
  peak_frame <- cbind(peak_frame, trend_duration, trend_strength)
  
  ################################################################################################################
  
  ################################################################################################################
  # Do the same with the second team
  smt2 <- loess.smooth(game_log_p$Time, game_log_p$ppm2, span=0.2,
                       degree = 2, evaluation = 100)
  
  peak_time2 <- smt2$x[ggpmisc:::find_peaks(smt2$y)]
  peak_ppm2 <- smt2$y[ggpmisc:::find_peaks(smt2$y)]
  
  
  
  # Find valleys ppm by time
  valley_time2 <- smt2$x[ggpmisc:::find_peaks(-smt2$y)]
  valley_ppm2 <- smt2$y[ggpmisc:::find_peaks(-smt2$y)]
  
  # Plot graph
  smt2_frame <- data.frame(smt2$x, smt2$y)
  
  # Create peaks and valleys data frame
  
  peak_frame2 <- data.frame(peak_time = c(peak_time2, valley_time2), peak_ppm = c(peak_ppm2, valley_ppm2))
  
  
  # Add finish time
  peak_frame2 <- rbind(peak_frame2, c(
    game_log_p$Time[nrow(game_log_p)],
    game_log_p$ppm2[nrow(game_log_p)],
    NA))
  
  # Add start time (2 min after start)
  peak_frame2 <- rbind(peak_frame2, c(
    game_log_p$Time[1],
    game_log_p$ppm2[1],
    NA))
  
  # Add team2 name
  peak_frame2 <- cbind(peak_frame2, team = names(game_log_p)[3])
  
  # Order extremumes by time
  peak_frame2 <- peak_frame2[order(peak_frame2$peak_time),]
  
  # Calculate peak strength
  
  trend_strength2 <- c(diff(peak_frame2$peak_ppm), NA)
  trend_duration2 <- c(diff(peak_frame2$peak_time), NA)
  
  # Add trends to peak table
  peak_frame2 <- cbind(peak_frame2, trend_duration = trend_duration2, trend_strength = trend_strength2)
  
   
  # Split team peaks
  
  gamePeaks <- rbind(peak_frame, peak_frame2)
  
  gamePeaks
  # Add stats to the game log
  
  #gameLogs[[i]] <- list(gameLogs[[i]], gamePeaks = gamePeaks )
  #gameLogs[[i]]
}



