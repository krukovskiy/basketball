library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)
# The main goal of this project is to find the win predictors

# Test links
# https://www.basketball-reference.com/boxscores/pbp/202110190MIL.html
# https://www.basketball-reference.com/boxscores/pbp/202110190LAL.html
# https://www.basketball-reference.com/boxscores/pbp/202204260PHO.html
# https://www.basketball-reference.com/boxscores/pbp/202204240NOP.html


# Get data
URL = "https://www.basketball-reference.com/boxscores/pbp/202110190LAL.html"
page = read_html(URL)


rank_data_html = page %>%  html_nodes(".thead .center") %>% html_text()
rank_cols <- c(rank_data_html[1], rank_data_html[2],rank_data_html[6], rank_data_html[4])

time <- page %>%  html_nodes("td:nth-child(1)") %>% html_text()
time

team1 <- page %>%  html_nodes("#pbp td:nth-child(2)") %>% html_text()
team2 <- page %>%  html_nodes("td.center:nth-child(2) , .center~ td+ td") %>% html_text()
score <- page %>%  html_nodes("tr~ tr+ tr td.center , td~ td+ .center") %>% html_text()

game_log <- data.frame(time, team1, team2, score)


# add team names to the log
colnames(game_log) <- rank_cols

# separate score by 2
game_log <- game_log %>% separate(Score, into = c("score1", "score2"), sep = "-", )

# Make good time column
default_time <- as.double(ms("12:00"))
logged_time <- as.double(ms(game_log$Time))
game_log$Time <- default_time - logged_time

# Find index of the end of 1 quarter
index_1q <- which(game_log[,2] == "End of 1st quarter")
# Delete rows with index and index + 1
game_log <- game_log[-c(index_1q, index_1q +1),]

# Find index of the end of 2 quarter
index_2q <- which(game_log[,2] == "End of 2nd quarter")
# Delete rows with index and index + 1
game_log <- game_log[-c(index_2q, index_2q +1),]

# Changing time of the 2 quarter
game_log[c((index_1q):nrow(game_log)), 1] <- game_log[c((index_1q):nrow(game_log)), 1] + 720

# Find index of the end of 3rd quarter
index_3q <- which(game_log[,2] == "End of 3rd quarter")
# Delete rows with index and index + 1
game_log <- game_log[-c(index_3q, index_3q +1),]
# Changing time of the 3 quarter
game_log[c((index_2q):nrow(game_log)), 1] <- game_log[c((index_2q):nrow(game_log)), 1] + 720

# Find index of the end of 4rd quarter
index_4q <- which(game_log[,2] == "End of 4th quarter")
# Delete rows with index and index + 1
game_log <- game_log[-c(index_4q, index_4q +1),]
# Changing time of the 4 quarter
game_log[c((index_3q):nrow(game_log)), 1] <- game_log[c((index_3q):nrow(game_log)), 1] + 720

# Adjusting scoring columns
game_log$score1 <- as.numeric(game_log$score1); game_log$score1[1] <- 0 
game_log$score2 <- as.numeric(game_log$score2); game_log$score2[1] <- 0

# Calculate points per minute distribution

game_log <- game_log %>%  mutate(ppm1 = score1 / Time * 60)
game_log <- game_log %>%  mutate(ppm2 = score2 / Time * 60)



# Explorations
plot(game_log$Time, game_log$score1)
points(game_log$Time, game_log$score1, col = "lightblue")
points(game_log$Time, game_log$score2, col = "magenta")

plot(game_log$Time, game_log$ppm1)
points(game_log$Time, game_log$ppm1, col = "lightblue")
points(game_log$Time, game_log$ppm2, col = "magenta")

par(mfrow = c(2,1))
plot(subset(game_log, Time > 540, select = c(Time, ppm1)))
points(game_log$Time, game_log$ppm1, col = "lightblue")
points(game_log$Time, game_log$ppm2, col = "magenta")

summary(subset(game_log, Time > 720, select = c(Time, ppm1)))

#Prepare game log
game_log_p <- game_log %>% filter(Time > 120)


# Smooth raw data to a graph
smt <- loess.smooth(game_log_p$Time, game_log_p$ppm1, span=0.2,
                    degree = 2, evaluation = 100)


plot(smt)


# Find peaks ppm by time
peak_time <- smt$x[ggpmisc:::find_peaks(smt$y)]
peak_ppm <- smt$y[ggpmisc:::find_peaks(smt$y)]

# Find valleys ppm by time
valley_time <- smt$x[ggpmisc:::find_peaks(-smt$y)]
valley_ppm <- smt$y[ggpmisc:::find_peaks(-smt$y)]

# Plot graph
smt_frame <- data.frame(smt$x, smt$y)
ggplot(data = smt_frame, aes(x = smt$x, y = smt$y)) + geom_line() + stat_peaks(col = "red") + stat_valleys(col = "green")


# Create peaks and valleys data frame

peak_frame <- data.frame(peak_time = c(peak_time, valley_time), peak_ppm = c(peak_ppm, valley_ppm))


# Add finish time
peak_frame <- rbind(peak_frame, c(
  game_log_p$Time[nrow(game_log_p)],
  game_log_p$ppm1[nrow(game_log_p)]))

# Add start time (2 min after start)
peak_frame <- rbind(peak_frame, c(
  game_log_p$Time[1],
  game_log_p$ppm1[1]))

# Order extremumes by time
peak_frame <- peak_frame[order(peak_frame$peak_time),]


# Calculate peak strength

trend_strength <- c(diff(peak_frame$peak_ppm), NA)
trend_duration <- c (diff(peak_frame$peak_time), NA)

# Add trends to peak table
peak_frame <- cbind(peak_frame, trend_duration, trend_strength)


# Explorations

head(game_log[game_log$Time > peak_frame$peak_time[3], 2:3], n = 10)


