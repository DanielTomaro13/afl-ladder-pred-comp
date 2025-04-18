library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(tibble)
###################################################################
url <- 'https://www.sportsbet.com.au/betting/australian-rules/afl'
headers <- c(
  'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36',
  'Accept-Language' = 'en-US,en;q=0.9'
)

response <- GET(url, add_headers(headers))
page <- content(response, 'text') %>% read_html()

all_spans <- page %>%
  html_nodes('span') %>%
  html_text()

team_names <- page %>%
  html_nodes('div.participantText_fivg86r') %>%
  html_text()

teams_df <- tibble(
  home_team = team_names[seq(1, length(team_names), by = 2)],
  away_team = team_names[seq(2, length(team_names), by = 2)]
)

game_times_idx <- str_which(all_spans, "^[A-Z][a-z]+,\\s\\d+\\s[A-Z][a-z]+\\s\\d{2}:\\d{2}$")

games <- list()
for (i in seq_along(game_times_idx)) {
  start <- game_times_idx[i]
  end <- if (i < length(game_times_idx)) game_times_idx[i + 1] - 1 else length(all_spans)
  
  game_data <- all_spans[start:end]
  
  game <- tibble(
    datetime = game_data[1],
    markets = game_data[2],
    odds_home = game_data[3],
    odds_home_dup = game_data[4],
    odds_away = game_data[5],
    odds_away_dup = game_data[6],
    line_home_raw = game_data[7],
    line_home = game_data[8],
    line_home_price = game_data[9],
    line_away_raw = game_data[10],
    line_away = game_data[11],
    line_away_price = game_data[12],
    total_over_raw = game_data[13],
    total_over = game_data[14],
    total_over_price = game_data[15],
    total_under_raw = game_data[16],
    total_under = game_data[17],
    total_under_price = game_data[18]
  )
  
  games[[i]] <- game
}

odds_df <- bind_rows(games)

odds_df <- odds_df %>%
  mutate(
    odds_home = as.numeric(odds_home),
    odds_away = as.numeric(odds_away),
    line_home_margin = as.numeric(str_extract(line_home, "-?\\d+\\.?\\d*")),
    line_away_margin = as.numeric(str_extract(line_away, "-?\\d+\\.?\\d*")),
    total_points = as.numeric(str_extract(total_over, "\\d+\\.?\\d*")),
    datetime_clean = lubridate::parse_date_time(datetime, orders = "A, d b H:M")
  )

odds <- bind_cols(teams_df, odds_df)
###################################################################
if (!dir.exists("sportsbet_odds")) dir.create("sportsbet_odds")

file_name <- paste0("sportsbet_odds/odds_", Sys.Date(), ".csv")
write.csv(odds, file_name, row.names = FALSE)
###################################################################