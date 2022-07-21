library('ggspectra')
library("ggpmisc")
library(gridExtra)
library(ggplot2)
library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)
#Links
#https://www.basketball-reference.com/boxscores/pbp/202205150PHO.html
#https://www.basketball-reference.com/boxscores/pbp/202110190LAL.html

tURL <- "https://www.basketball-reference.com/boxscores/pbp/202205150PHO.html"

tgameLog  <- getGameLog(tURL)
tgameSummary <- getGameSummary(tURL)
tgamePeaks <- countPeaks(tgameLog)



tt <- merge(tgameLog, tgamePeaks, by.x ="Time", by.y =  "peak_time", all.x=TRUE, all.y=TRUE)
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

smt2 <- loess.smooth(tgameLog$Time, tgameLog$ppm2, span=0.2,
                     degree = 2, evaluation = 100)

peak_time2 <- smt2$x[ggpmisc:::find_peaks(smt2$y)]
peak_ppm2 <- smt2$y[ggpmisc:::find_peaks(smt2$y)]



# Find valleys ppm by time
valley_time2 <- smt2$x[ggpmisc:::find_peaks(-smt2$y)]
valley_ppm2 <- smt2$y[ggpmisc:::find_peaks(-smt2$y)]

# Plot graph
smt2_frame <- data.frame(smt2$x, smt2$y)


grid.arrange(plot1, plot3, plot2, plot4, ncol=2, nrow = 2)
plot1 <- ggplot(data = smt2_frame, aes(x = smt2$x, y = smt2$y)) + geom_line() + stat_peaks(col = "red") + stat_valleys(col = "green") + ylim(1,3)+ stat_smooth()
plot3 <- ggplot(data = smt1_frame, aes(x = smt1$x, y = smt1$y)) + geom_line()  + stat_peaks(col = "red")  + stat_valleys(col = "green")  + ylim(1,3)  + stat_smooth()
plot2 <- ggplot(data = tgameLog, aes(x = Time, y = ppm2)) + geom_line() + ylim(.5, 4)
plot4 <- ggplot(data = tgameLog, aes(x = Time, y = ppm1)) + geom_line()+ ylim(.5, 4)

names(tgameSummary)[5] <- "tscores1"
names(tgameSummary)[6] <- "tscores2"
tt <- cbind(tt, tgameSummary)
tLm = glm(winner1 ~ score1, data = tt, family="binomial")
summary(tLm)
