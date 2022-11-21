library(rvest)
library(jsonlite)
# Parse html page

page = read_html("https://www.nba.com/game/0022200210/play-by-play")
t = page %>% html_nodes("script") %>% html_text()

