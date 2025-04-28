#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
library(xgboost)
library(Matrix)
library(elo)
library(data.table)
#####################################################
results <- fetch_results_afltables(2003:2025)
colnames(results)
restructure_afl_data <- function(afl_data) {
  afl_home <- data.frame(
    Date = afl_data$Date,
    Season = afl_data$Season,
    Round = afl_data$Round.Number,
    Game_Id = paste0(afl_data$Date, "_", afl_data$Home.Team, "_vs_", afl_data$Away.Team),
    Team = afl_data$Home.Team,
    Opponent = afl_data$Away.Team,
    Result = ifelse(afl_data$Home.Points > afl_data$Away.Points, "W", "L"),
    Points_For = afl_data$Home.Points,
    Points_Against = afl_data$Away.Points,
    Spread = afl_data$Home.Points - afl_data$Away.Points,
    Played = TRUE,
    Home = TRUE,
    Game_ID = afl_data$Game,
    ELO = NA,
    Opp_ELO = NA
  )
  afl_away <- data.frame(
    Date = afl_data$Date,
    Season = afl_data$Season,
    Round = afl_data$Round.Number,
    Game_Id = paste0(afl_data$Date, "_", afl_data$Home.Team, "_vs_", afl_data$Away.Team),
    Team = afl_data$Away.Team,
    Opponent = afl_data$Home.Team,
    Result = ifelse(afl_data$Away.Points > afl_data$Home.Points, "W", "L"),
    Points_For = afl_data$Away.Points,
    Points_Against = afl_data$Home.Points,
    Spread = afl_data$Away.Points - afl_data$Home.Points,
    Played = TRUE,
    Home = FALSE,
    Game_ID = afl_data$Game,
    ELO = NA,
    Opp_ELO = NA
  )
  return(bind_rows(afl_home, afl_away))
}
results <- restructure_afl_data(results)

results <- results %>%
  filter(!(Team %in% c("Fitzroy", "University") | Opponent %in% c("Fitzroy", "University")))

colnames(results)
colSums(is.na(results)) # ELO will be NA
#####################################################
results <- results %>% mutate(
  ELO = 0,
  Opp_ELO = 0,
  Result = ifelse(Result == "W", 1, Result),
  Result = ifelse(Result == "L", 0, Result),
  Result = ifelse(Result == "T", 0.5, Result),
  Result = as.numeric(Result)
)
#####################################################
results <- results %>%
  mutate(Result_Binary = ifelse(Result == 1, 1, 0))

results <- results %>% arrange(Date, Season, Round, Game_ID)

teams_elo <- unique(c(results$Team))

teams_elo <- data.frame(
  Team = teams_elo,
  ELO = 1500,
  stringsAsFactors = FALSE
)

teams_elo <- teams_elo[order(teams_elo$Team), ]

for(i in 1:nrow(results)){
  if(i %% 2 != 0){ 
    # i = 1
    print(i)
    
    Team_A <- results$Team[i]
    Team_B <- results$Team[i+1]
    
    Result_A <- results$Result[i]
    Result_B <- results$Result[i+1]
    
    ELO_A <- as.numeric(teams_elo[teams_elo$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(teams_elo[teams_elo$Team == Team_B, "ELO"])
    
    results$ELO[i] <- ELO_A
    results$Opp_ELO[i] <- ELO_B
    
    results$ELO[i+1] <- ELO_B
    results$Opp_ELO[i+1] <- ELO_A
    
    R_A <- 10^(ELO_A/400)
    R_B <- 10^(ELO_B/400)
    
    E_A <- R_A/(R_A + R_B)
    E_B <- R_B/(R_A + R_B)
    
    Elo_Updated_A <- ELO_A + 20 * (Result_A - E_A) 
    Elo_Updated_B <- ELO_B + 20 * (Result_B - E_B)
    
    teams_elo[teams_elo$Team == Team_A, "ELO"] <- Elo_Updated_A
    teams_elo[teams_elo$Team == Team_B, "ELO"] <- Elo_Updated_B
    
  }
}

results <- results %>%
  mutate(Elo_Difference = ELO - Opp_ELO)

colnames(results)

results <- results %>% select(
  Date, Season, Round, Game_Id, Game_ID, Team, Opponent, Points_For, Points_Against, Result_Binary, Spread, Home, ELO, Opp_ELO,
  Elo_Difference
)
#####################################################
# Add form
results <- results %>%
  arrange(Team, Date) %>%
  group_by(Team) %>%
  mutate(
    form_last_5 = coalesce(lag(slide_dbl(Result_Binary, ~mean(.x, na.rm = TRUE), .before = 4, .complete = TRUE)), 0)
  ) %>%
  ungroup()
#####################################################
# Add Weather - There will be an issue with team name mapping
weather <- fetch_results_afl(2003:2025)
weather <- weather %>% select(match.date, round.roundNumber, match.homeTeam.name, match.awayTeam.name,
                              weather.description, weather.tempInCelsius, weather.weatherType)

weather_flags <- weather %>%
  mutate(
    Date = as.Date(match.date),
    is_wet_weather = as.integer(grepl("RAIN|WINDY", weather.weatherType, ignore.case = TRUE))
  ) %>%
  rename(
    Home.Team = match.homeTeam.name,
    Away.Team = match.awayTeam.name
  ) %>%
  select(Date, Home.Team, Away.Team, is_wet_weather)

weather_flags_long <- weather_flags %>%
  pivot_longer(cols = c(Home.Team, Away.Team),
               names_to = "HomeAway",
               values_to = "Team") %>%
  select(Date, Team, is_wet_weather)

results <- results %>%
  left_join(weather_flags_long, by = c("Date", "Team")) %>%
  mutate(is_wet_weather = replace_na(is_wet_weather, 0))
#####################################################
# Team stats
stats <- fetch_player_stats_afltables(2003:2025)
colnames(stats)
colSums(is.na(stats))
unique_teams <- stats %>% arrange(Home.team)
unique(unique_teams$Home.team)
unique(results$Team)

team_name_map <- c(
  "Western Bulldogs" = "Footscray",
  "Greater Western Sydney" = "GWS",
  "Kangaroos" = "North Melbourne")

results <- results %>%
  mutate(
    Team = recode(Team, !!!team_name_map),
    Opponent = recode(Opponent, !!!team_name_map)
  )
stats <- stats %>%
  mutate(
    Team = recode(Team, !!!team_name_map),
    Home.team = recode(Home.team, !!!team_name_map),
    Away.team = recode(Away.team, !!!team_name_map)
  )

keep_na_cols <- c("Home.team", "Away.team", "Brownlow.Votes", "Bounces", "Time.on.Ground", "Player", "ID")

stats <- stats %>%
  mutate(
    Brownlow.Votes = replace_na(Brownlow.Votes, 0),
    Bounces = replace_na(Bounces, 0),
    Time.on.Ground = replace_na(Time.on.Ground, 0)
  )

cols_to_keep <- names(stats)[colSums(is.na(stats)) == 0 | names(stats) %in% keep_na_cols]

stats <- stats %>%
  select(all_of(cols_to_keep))

stats <- stats %>%
  select(
    Date, Season, Round, 
    Home.team, Away.team, 
    Player, ID, Team, 
    Kicks, Marks, Handballs, Disposals, Goals, Behinds,
    Tackles, Rebounds, Inside.50s, Clearances, Clangers, Brownlow.Votes,
    Contested.Possessions, Uncontested.Possessions, Contested.Marks,
    Marks.Inside.50, One.Percenters, Goal.Assists, Time.on.Ground,
    Age, Career.Games, Coach
  ) %>%
  arrange(Date, Season, Round, Team) %>%
  mutate(
    Game_Id = paste0(Date, "_", Home.team, "_vs_", Away.team)
  )

colSums(is.na(stats))

team_avg_stats <- stats %>%
  arrange(Team, Date) %>%
  group_by(Team) %>%
  mutate(
    avg_Kicks = lag(cummean(Kicks)),
    avg_Marks = lag(cummean(Marks)),
    avg_Handballs = lag(cummean(Handballs)),
    avg_Disposals = lag(cummean(Disposals)),
    avg_Goals = lag(cummean(Goals)),
    avg_Behinds = lag(cummean(Behinds)),
    avg_Tackles = lag(cummean(Tackles)),
    avg_Rebounds = lag(cummean(Rebounds)),
    avg_Inside.50s = lag(cummean(Inside.50s)),
    avg_Clearances = lag(cummean(Clearances)),
    avg_Clangers = lag(cummean(Clangers)),
    avg_Brownlow.Votes = lag(cummean(Brownlow.Votes)),
    avg_Contested.Possessions = lag(cummean(Contested.Possessions)),
    avg_Uncontested.Possessions = lag(cummean(Uncontested.Possessions)),
    avg_Contested.Marks = lag(cummean(Contested.Marks)),
    avg_Marks.Inside.50 = lag(cummean(Marks.Inside.50)),
    avg_One.Percenters = lag(cummean(One.Percenters)),
    avg_Goal.Assists = lag(cummean(Goal.Assists)),
    avg_Time.on.Ground = lag(cummean(Time.on.Ground)),
    avg_Age = lag(cummean(Age)),
    avg_Career_Games = lag(cummean(Career.Games)),
    
    roll3_Kicks = lag(slide_dbl(Kicks, mean, .before = 2, .complete = TRUE)),
    roll3_Marks = lag(slide_dbl(Marks, mean, .before = 2, .complete = TRUE)),
    roll3_Handballs = lag(slide_dbl(Handballs, mean, .before = 2, .complete = TRUE)),
    roll3_Disposals = lag(slide_dbl(Disposals, mean, .before = 2, .complete = TRUE)),
    roll3_Goals = lag(slide_dbl(Goals, mean, .before = 2, .complete = TRUE)),
    roll3_Behinds = lag(slide_dbl(Behinds, mean, .before = 2, .complete = TRUE)),
    roll3_Tackles = lag(slide_dbl(Tackles, mean, .before = 2, .complete = TRUE)),
    roll3_Rebounds = lag(slide_dbl(Rebounds, mean, .before = 2, .complete = TRUE)),
    roll3_Inside.50s = lag(slide_dbl(Inside.50s, mean, .before = 2, .complete = TRUE)),
    roll3_Clearances = lag(slide_dbl(Clearances, mean, .before = 2, .complete = TRUE)),
    roll3_Clangers = lag(slide_dbl(Clangers, mean, .before = 2, .complete = TRUE)),
    roll3_Brownlow.Votes = lag(slide_dbl(Brownlow.Votes, mean, .before = 2, .complete = TRUE)),
    roll3_Contested.Possessions = lag(slide_dbl(Contested.Possessions, mean, .before = 2, .complete = TRUE)),
    roll3_Uncontested.Possessions = lag(slide_dbl(Uncontested.Possessions, mean, .before = 2, .complete = TRUE)),
    roll3_Contested.Marks = lag(slide_dbl(Contested.Marks, mean, .before = 2, .complete = TRUE)),
    roll3_Marks.Inside.50 = lag(slide_dbl(Marks.Inside.50, mean, .before = 2, .complete = TRUE)),
    roll3_One.Percenters = lag(slide_dbl(One.Percenters, mean, .before = 2, .complete = TRUE)),
    roll3_Goal.Assists = lag(slide_dbl(Goal.Assists, mean, .before = 2, .complete = TRUE)),
    roll3_Time.on.Ground = lag(slide_dbl(Time.on.Ground, mean, .before = 2, .complete = TRUE)),
    roll3_Age = lag(slide_dbl(Age, mean, .before = 2, .complete = TRUE)),
    roll3_Career_Games = lag(slide_dbl(Career.Games, mean, .before = 2, .complete = TRUE))
  ) %>%
  ungroup() %>%
  group_by(Date, Season, Round, Game_Id, Team) %>%
  summarise(
    across(starts_with("avg_"), mean, na.rm = FALSE),
    across(starts_with("roll3_"), mean, na.rm = FALSE),
    .groups = "drop"
  )
colSums(is.na(team_avg_stats))

results <- results %>%
  mutate(Round = as.character(Round)) %>%
  left_join(
    team_avg_stats %>% select(Date, Season, Round, Game_Id, Team, starts_with("avg_"), starts_with("roll3_")),
    by = c("Date", "Season", "Round", "Game_Id", "Team")
  ) %>% arrange(Date, Season, Round, Game_Id)

colnames(results)
colSums(is.na(results))
results <- results %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))
#####################################################
# is_premiership_coach
premiership_coaches <- c(
  "Beveridge, Luke",
  "Blight, Malcolm",
  "Clarkson, Alastair",
  "Goodwin, Simon",
  "Hardwick, Damien",
  "Jeans, Allan",
  "Longmire, John",
  "Malthouse, Michael",
  "Malthouse, Mick",
  "Matthews, Leigh",
  "McRae, Craig",
  "Pagan, Denis",
  "Parkin, David",
  "Roos, Paul",
  "Scott, Chris",
  "Sheedy, Kevin",
  "Thompson, Mark",
  "Walls, Robert",
  "Williams, Mark"
)

coach <- stats %>%
  select(Date, Season, Round, Team, Coach) %>%
  distinct() %>%  # So we don't duplicate teams
  mutate(is_premiership_coach = ifelse(Coach %in% premiership_coaches, 1, 0))

results <- results %>%
  left_join(coach, by = c("Date", "Season", "Round", "Team"))
#####################################################
# Time between previous game
results <- results %>%
  arrange(Team, Date) %>%
  group_by(Team) %>%
  mutate(
    rest_days = as.numeric(Date - lag(Date)),
    rest_days = replace_na(rest_days, 0),            
    rest_days = pmin(rest_days, 14),                 # Cap it
    is_short_turnaround = ifelse(rest_days < 6, 1, 0)
  ) %>%
  ungroup()
#####################################################
results <- na.omit(results)

train_data <- results %>% filter(Season >= 2003, Season <= 2024)
test_data <- results %>% filter(Season == 2025)

poisson_model_train <- glm(
  Points_For ~ 
    Elo_Difference + 
    Home +
    roll3_Kicks + 
    roll3_Goals +
    roll3_Behinds +
    roll3_Inside.50s +
    roll3_Clearances +
    roll3_Brownlow.Votes +
    roll3_Goal.Assists +
    roll3_Time.on.Ground +
    is_short_turnaround +
    rest_days +
    form_last_5 +
    avg_Contested.Marks +
    roll3_Marks +
    roll3_Rebounds,
  family = poisson(link = "log"),
  data = train_data
)

test_data <- test_data %>%
  mutate(
    Poisson_Pred_Points = predict(poisson_model_train, newdata = ., type = "response")
  )

opponent_pred_points <- test_data %>%
  select(Game_Id, Team, Poisson_Pred_Points) %>%
  rename(Opponent = Team, Opponent_Poisson_Pred_Points = Poisson_Pred_Points)

test_data <- test_data %>%
  left_join(opponent_pred_points, by = c("Game_Id", "Opponent")) %>%
  mutate(
    Poisson_Pred_Margin = Poisson_Pred_Points - Opponent_Poisson_Pred_Points,
    Poisson_Pred_Result = ifelse(Poisson_Pred_Margin > 0, 1, 0),
    Poisson_Correct = ifelse(Poisson_Pred_Result == Result_Binary, 1, 0)
  )

poisson_accuracy_2025 <- test_data %>%
  summarise(
    Accuracy = mean(Poisson_Correct, na.rm = TRUE) * 100,
    MAE = mean(abs(Points_For - Poisson_Pred_Points), na.rm = TRUE)
  )

poisson_accuracy_2025

poisson_data <- test_data %>% select(
  Round, Game_Id, Team, Opponent, Poisson_Pred_Points, Poisson_Pred_Margin, Poisson_Pred_Result
) %>% arrange(Game_Id)
#####################################################
# Actual Ladder
ladder_actual <- results %>%
  filter(Season == 2025) %>%
  group_by(Team) %>%
  summarise(
    Actual_Wins = sum(Result_Binary == 1),
    Actual_Points = Actual_Wins * 4,
    Points_For = sum(Points_For, na.rm = TRUE),
    Points_Against = sum(Points_Against, na.rm = TRUE),
    .groups = "drop"
  ) %>%  mutate(
    Percentage = round((Points_For / Points_Against) * 100, 1)
  ) %>%
  arrange(desc(Actual_Points)) %>%
  mutate(Rank_Actual = row_number())

ladder_poisson <- poisson_data %>%
  group_by(Team) %>%
  summarise(
    Poisson_Wins = sum(Poisson_Pred_Result == 1, na.rm = TRUE),
    Poisson_Points = Poisson_Wins * 4,
    Poisson_Points_For = sum(Poisson_Pred_Points, na.rm = TRUE),
    Poisson_Points_Against = sum(Poisson_Pred_Points - Poisson_Pred_Margin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Poisson_Percentage = round((Poisson_Points_For / Poisson_Points_Against) * 100, 1)
  ) %>%
  arrange(desc(Poisson_Points), desc(Poisson_Percentage)) %>%
  mutate(Rank_Poisson = row_number())

ladder_comparison <- ladder_actual %>%
  left_join(ladder_poisson, by = "Team")

ladder_comparison <- ladder_comparison %>%
  mutate(
    Diff_Poisson = abs(Rank_Poisson - Rank_Actual)
  )

rank_diff_summary <- ladder_comparison %>%
  summarise(
    Mean_Diff_Poisson = mean(Diff_Poisson, na.rm = TRUE),
  )

rank_diff_summary

ranks <- ladder_comparison %>% select(Team, Rank_Actual, Rank_Poisson) %>% mutate(
  rank_diff = Rank_Actual - Rank_Poisson
)
#####################################################
# 2025 Future Predictions
fixture_2025 <- fetch_fixture_footywire(2025)
colnames(fixture_2025)

fixture_home <- fixture_2025 %>%
  mutate(
    Date = as.Date(Date),
    Season = 2025,
    Game_Id = paste0(Date, "_", Home.Team, "_vs_", Away.Team),
    Team = Home.Team,
    Opponent = Away.Team,
    Home = TRUE
  ) %>%
  select(Date, Season, Game_Id, Round, Team, Opponent, Venue, Home)

fixture_away <- fixture_2025 %>%
  mutate(
    Date = as.Date(Date),
    Season = 2025,
    Game_Id = paste0(Date, "_", Home.Team, "_vs_", Away.Team),
    Team = Away.Team,
    Opponent = Home.Team,
    Home = FALSE
  ) %>%
  select(Date, Season, Game_Id, Round, Team, Opponent, Venue, Home)

#####################################################
fixture_2025_long <- bind_rows(fixture_home, fixture_away) %>% 
  arrange(Date, Season, Round, Game_Id) %>% filter(Round >= 8)

fixture_2025_long <- fixture_2025_long %>%
  mutate(
    Team = ifelse(Team == "Western Bulldogs", "Footscray", Team),
    Opponent = ifelse(Opponent == "Western Bulldogs", "Footscray", Opponent),
    Team = ifelse(Team == "Greater Western Sydney", "GWS", Team),
    Opponent = ifelse(Opponent == "Greater Western Sydney", "GWS", Opponent)
  )
#####################################################
recent_team_stats <- results %>%
  filter(Season == 2025, Round < 8) %>%
  group_by(Team) %>%
  slice_max(Date, n = 1, with_ties = FALSE) %>%
  select(
    Team,
    ELO,
    roll3_Kicks, roll3_Goals, roll3_Behinds,
    roll3_Inside.50s, roll3_Clearances,
    roll3_Brownlow.Votes, roll3_Goal.Assists,
    roll3_Time.on.Ground,
    is_short_turnaround, rest_days,
    form_last_5,
    avg_Contested.Marks,
    roll3_Marks,
    roll3_Rebounds
  )


fixture_2025_long <- fixture_2025_long %>%
  left_join(recent_team_stats, by = "Team") %>%
  
  left_join(
    recent_team_stats %>%
      select(Team, Opponent_ELO = ELO),
    by = c("Opponent" = "Team")
  ) %>%
  
  mutate(
    Elo_Difference = ELO - Opponent_ELO
  )
#####################################################
fixture_2025_long <- fixture_2025_long %>%
  mutate(
    Poisson_Pred_Points = predict(poisson_model_train, newdata = ., type = "response")
  )

opponent_pred_points <- fixture_2025_long %>%
  select(Game_Id, Team, Poisson_Pred_Points) %>%
  rename(Opponent = Team, Opponent_Poisson_Pred_Points = Poisson_Pred_Points)

fixture_2025_long <- fixture_2025_long %>%
  left_join(opponent_pred_points, by = c("Game_Id", "Opponent")) %>%
  mutate(
    Poisson_Pred_Margin = Poisson_Pred_Points - Opponent_Poisson_Pred_Points,
    Poisson_Pred_Result = ifelse(Poisson_Pred_Margin > 0, 1, 0)
  )

fixture_2025_predictions <- fixture_2025_long %>%
  filter(Home == TRUE) %>%
  select(
    Date, Round, Venue, Game_Id,
    Team, Opponent,
    Poisson_Pred_Points, Opponent_Poisson_Pred_Points,
    Poisson_Pred_Margin, Poisson_Pred_Result
  ) %>%
  arrange(Date)
#####################################################