library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)
library(glue)

season <- c("2015-16", 
            "2016-17", 
            "2017-18", 
            "2018-19", 
            "2019-20", 
            "2020-21", 
            "2021-22")

dfseason <- function(season) {
  headers <- c( # les llamo headers porque es mas corto ;)
    "Accept" = "application/json, text/plain, */*",
    "Accept-Encoding" = "gzip, deflate, br",
    "Accept-Language" = "es-ES,es;q=0.9,en;q=0.8",
    "Connection" = "keep-alive",
    "Host" = "stats.nba.com",
    "If-Modified-Since" = "Tue, 28 Jun 2022 16:52:53 GMT",
    "Origin" = "https://www.nba.com",
    "Referer" = "https://www.nba.com/",
    "sec-ch-ua" = "Not A;Brand';v='99', 'Chromium';v='103', 'Google Chrome';v='103'",
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = "macOS",
    "Sec-Fetch-Dest" = "empty",
    "Sec-Fetch-Mode" = "cors",
    "Sec-Fetch-Site" = "same-site",
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36",
    "x-nba-stats-origin" = "stats",
    "x-nba-stats-token" = "true"
  )
  link <- paste0("https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&LeagueID=00&PlayerOrTeam=T&Season=", season, "&SeasonType=Regular%20Season&Sorter=DATE") # LINK
  res <- httr::GET(url = link, add_headers(.headers = headers))
  json_res <- jsonlite::fromJSON(content(res, "text"))
  
  link1 <- paste0("https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&LeagueID=00&PlayerOrTeam=T&Season=", season, "&SeasonType=Playoffs&Sorter=DATE") # LINK PARA PLAYOFF
  res1 <- httr::GET(url = link1, add_headers(.headers = headers))
  json_res1 <- jsonlite::fromJSON(content(res1, "text"))
  
  # TABLA REGULAR
  df <- data.frame(json_res$resultSets$rowSet)
  colnames(df) <- json_res$resultSets$headers[[1]]
  df <- janitor::clean_names(df)
  df$year <- season
  df$type <- "regular"
  
  # TABLA PLAYOFF
  df1 <- data.frame(json_res1$resultSets$rowSet)
  colnames(df1) <- json_res1$resultSets$headers[[1]]
  df1 <- janitor::clean_names(df1)
  df1$year <- season
  df1$type <- "playoff"
  
  df2 <- rbind(df1, df) #juntamos las tablas
  
  return(df2)
}

df_games <- map_df(season, dfseason) #mapeamos la funcion
write.csv(df_games, "df_games.csv")
