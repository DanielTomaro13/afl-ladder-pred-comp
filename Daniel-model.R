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

team_name_map <- c(
  "Western Bulldogs" = "Footscray",
  "GWS Giants" = "Greater Western Sydney",
  "Sydney Swans" = "Sydney",
  "Brisbane Lions" = "Brisbane",
  "Gold Coast Suns" = "Gold Coast",
  "Port Adelaide Power" = "Port Adelaide",
  "North Melbourne Kangaroos" = "North Melbourne",
  "Kangaroos" = "North Melbourne",
  "West Coast Eagles" = "West Coast",
  "Geelong Cats" = "Geelong",
  "Hawthorn Hawks" = "Hawthorn",
  "Richmond Tigers" = "Richmond",
  "Carlton Blues" = "Carlton",
  "St Kilda Saints" = "St Kilda",
  "Melbourne Demons" = "Melbourne",
  "Essendon Bombers" = "Essendon",
  "Adelaide Crows" = "Adelaide",
  "Fremantle Dockers" = "Fremantle",
  "Collingwood Magpies" = "Collingwood"
)
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

keep_na_cols <- c("Brownlow.Votes", "Bounces", "Time.on.Ground", "Player", "ID")
stats <- stats %>%
  mutate(
    Brownlow.Votes = replace_na(Brownlow.Votes, 0),
    Bounces = replace_na(Bounces, 0),
    Time.on.Ground = replace_na(Time.on.Ground, 0)
  )
cols_to_keep <- names(stats)[colSums(is.na(stats)) == 0 | names(stats) %in% keep_na_cols]
stats <- stats %>% select(all_of(cols_to_keep))
colSums(is.na(stats))
colnames(stats)

stats <- stats %>% select(
  Date, Season, Round, Player, ID, Team, Kicks, Marks, Handballs, Disposals, Goals, Behinds,
  Tackles, Rebounds, Inside.50s, Clearances, Clangers, Brownlow.Votes, Contested.Possessions, Uncontested.Possessions, Contested.Marks,
  Marks.Inside.50, One.Percenters, Goal.Assists, Time.on.Ground, Age, Career.Games, Coach
) %>% arrange(Date, Season, Round, Team)
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
  group_by(Date, Season, Round, Team) %>%
  summarise(across(starts_with("avg_"), mean, na.rm = TRUE),
            across(starts_with("roll3_"), mean, na.rm = TRUE),
            .groups = "drop")

results <- results %>%
  mutate(Round = as.character(Round)) %>%
  left_join(team_avg_stats, by = c("Date", "Season", "Round", "Team"))


results <- na.omit(results)
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
# Logisitc Regression
logit_model <- glm(
  Result_Binary ~ 
    Elo_Difference + 
    Home +
    
    # Team performance metrics - both avg and rolling 3
    avg_Inside.50s + roll3_Inside.50s +
    avg_Clearances + roll3_Clearances +
    avg_Contested.Possessions + roll3_Contested.Possessions +
    avg_Uncontested.Possessions + roll3_Uncontested.Possessions +
    
    # Team quality indicators
    is_premiership_coach +
    avg_Career_Games + roll3_Career_Games +
    form_last_5 +
    
    # New features
    rest_days + is_short_turnaround,
  
  family = binomial,
  data = results
)

summary(logit_model)


results <- results %>%
  mutate(
    Logit_Prob = predict(logit_model, newdata = results, type = "response"),
    Logit_Forecast = ifelse(Logit_Prob > 0.5, 1, 0),
    Logit_Correct = ifelse(Logit_Forecast == Result_Binary, 1, 0)
  )

logit_accuracy_by_season <- results %>%
  filter(!is.na(Logit_Correct)) %>%
  group_by(Season) %>%
  summarise(
    Accuracy = mean(Logit_Correct) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Season))

logit_accuracy_by_season

table(results$Result_Binary, results$Logit_Forecast)
#####################################################
# Linear Regression 
spread_lm <- lm(
  Spread ~ 
    Elo_Difference + 
    Home +
    # Both avg and roll3 performance metrics
    avg_Inside.50s + roll3_Inside.50s +
    avg_Clearances + roll3_Clearances +
    avg_Contested.Possessions + roll3_Contested.Possessions +
    avg_Goal.Assists + roll3_Goal.Assists +
    # Team quality indicators
    is_premiership_coach +
    avg_Career_Games + roll3_Career_Games +
    form_last_5 + rest_days + is_short_turnaround,
  data = results
)
summary(spread_lm)


results <- results %>%
  mutate(
    Margin = Points_For - Points_Against,
    Margin_Pred = predict(spread_lm, newdata = results),
    Margin_Pred_Result = ifelse(Margin_Pred > 0, 1, 0),
    Margin_Correct = ifelse(Margin_Pred_Result == Result_Binary, 1, 0)
  )

season_accuracy_mae <- results %>%
  group_by(Season) %>%
  summarise(
    Accuracy = mean(Margin_Correct, na.rm = TRUE) * 100,
    MAE = mean(abs(Margin - Margin_Pred), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Season)) 

season_accuracy_mae
#####################################################
# Poisson Distribution

poisson_model <- glm(
  Points_For ~ 
    Elo_Difference + 
    Home +
    avg_Inside.50s + roll3_Inside.50s +
    avg_Marks.Inside.50 + roll3_Marks.Inside.50 +
    avg_Goal.Assists + roll3_Goal.Assists +
    avg_Clearances + roll3_Clearances +
    is_premiership_coach +
    form_last_5 + rest_days + is_short_turnaround,
  family = poisson(link = "log"),
  data = results
)

summary(poisson_model)


results <- results %>%
  mutate(
    Poisson_Pred_Points = predict(poisson_model, type = "response"),
    Poisson_Pred_Margin = Poisson_Pred_Points - Points_Against,
    Poisson_Pred_Result = ifelse(Poisson_Pred_Margin > 0, 1, 0),
    Poisson_Correct = ifelse(Poisson_Pred_Result == Result_Binary, 1, 0)
  )

poisson_accuracy_by_season <- results %>%
  group_by(Season) %>%
  summarise(
    Accuracy = mean(Poisson_Correct, na.rm = TRUE) * 100,
    MAE = mean(abs(Points_For - Poisson_Pred_Points), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Season)) 

poisson_accuracy_by_season

poisson_data <- results %>% filter(Season == 2025) %>% select(
  Date, Round, Game_Id, Team, Opponent, Poisson_Pred_Points, Poisson_Pred_Margin, Poisson_Pred_Result, Poisson_Correct
) %>% arrange(Date, Round, Game_Id)
#####################################################
# XG Boost

xgb_data <- results %>%
  filter(!is.na(Result_Binary)) %>%
  select(
    # Target
    Result_Binary,
    
    # Core predictors
    Elo_Difference, 
    Home,
    
    # avg_ metrics
    avg_Inside.50s, avg_Clearances, avg_Contested.Possessions,
    avg_Marks.Inside.50, avg_Goal.Assists,
    avg_Career_Games,
    
    # roll3_ metrics
    roll3_Inside.50s, roll3_Clearances, roll3_Contested.Possessions,
    roll3_Marks.Inside.50, roll3_Goal.Assists,
    roll3_Career_Games,
    
    # Team quality indicators
    is_premiership_coach,
    form_last_5,
    rest_days,
    is_short_turnaround
  )


X <- as.matrix(xgb_data %>% select(-Result_Binary))
y <- xgb_data$Result_Binary

dtrain <- xgb.DMatrix(data = X, label = y)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 5,                # Increased from 3 for more complex relationships
  eta = 0.05,                   # Reduced from 0.1 for better generalization
  subsample = 0.8,              # Added subsample for robustness
  colsample_bytree = 0.8,       # Added column sampling
  min_child_weight = 3          # Added to prevent overfitting
)

cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 500,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = 0
)

optimal_nrounds <- which.min(cv_results$evaluation_log$test_logloss_mean)

xgb_model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = optimal_nrounds,
  verbose = 0
)

results$XGB_Win_Prob <- predict(
  xgb_model, 
  as.matrix(results %>% select(
    Elo_Difference, 
    Home,
    
    # avg_ predictors
    avg_Inside.50s, avg_Clearances, avg_Contested.Possessions,
    avg_Marks.Inside.50, avg_Goal.Assists, avg_Career_Games,
    
    # roll3_ predictors
    roll3_Inside.50s, roll3_Clearances, roll3_Contested.Possessions,
    roll3_Marks.Inside.50, roll3_Goal.Assists, roll3_Career_Games,
    
    # team quality indicators
    is_premiership_coach,
    form_last_5,
    rest_days,
    is_short_turnaround
  ))
)

results$XGB_Forecast <- ifelse(results$XGB_Win_Prob > 0.5, 1, 0)
results$XGB_Correct <- ifelse(results$XGB_Forecast == results$Result_Binary, 1, 0)

xgb_accuracy <- results %>%
  group_by(Season) %>%
  summarise(
    Accuracy = mean(XGB_Correct, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Season))

xgb.importance(model = xgb_model)

xgb_accuracy_by_season <- results %>%
  group_by(Season) %>%
  summarise(
    Accuracy = mean(XGB_Correct, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Season))

xgb_accuracy_by_season
#####################################################
# Ladder

# Actual Ladder
ladder_actual <- results %>%
  filter(Season == 2025) %>%
  group_by(Team) %>%
  summarise(
    Actual_Wins = sum(Result_Binary == 1),
    Actual_Points = Actual_Wins * 4,
    .groups = "drop"
  ) %>%
  arrange(desc(Actual_Points)) %>%
  mutate(Rank_Actual = row_number())

# Logistic Regression
ladder_logit <- results %>%
  filter(Season == 2025) %>%
  group_by(Team) %>%
  summarise(
    Logit_Wins = sum(Logit_Forecast == 1, na.rm = TRUE),
    Logit_Points = Logit_Wins * 4,
    .groups = "drop"
  ) %>%
  arrange(desc(Logit_Points)) %>%
  mutate(Rank_Logit = row_number())

# XGBoost
ladder_xgb <- results %>%
  filter(Season == 2025) %>%
  group_by(Team) %>%
  summarise(
    XGB_Wins = sum(XGB_Forecast == 1, na.rm = TRUE),
    XGB_Points = XGB_Wins * 4,
    .groups = "drop"
  ) %>%
  arrange(desc(XGB_Points)) %>%
  mutate(Rank_XGB = row_number())

# Poisson
ladder_poisson <- results %>%
  filter(Season == 2025) %>%
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

poisson_data <- results %>%
  select(
    Season,
    Round,
    Date,
    Team,
    Opponent,
    Home,
    Points_For,
    Points_Against,
    Poisson_Pred_Points,
    Poisson_Pred_Margin,
    Poisson_Pred_Result,
    Result_Binary) %>% filter(Season == 2025) %>% arrange(Date, Round, Team)

# Linear Margin
ladder_linear <- results %>%
  filter(Season == 2025) %>%
  group_by(Team) %>%
  summarise(
    Linear_Wins = sum(Margin_Pred_Result == 1, na.rm = TRUE),
    Linear_Points = Linear_Wins * 4,
    .groups = "drop"
  ) %>%
  arrange(desc(Linear_Points)) %>%
  mutate(Rank_Linear = row_number())

ladder_comparison <- ladder_actual %>%
  left_join(ladder_logit, by = "Team") %>%
  left_join(ladder_xgb, by = "Team") %>%
  left_join(ladder_poisson, by = "Team") %>%
  left_join(ladder_linear, by = "Team")

ladder_comparison <- ladder_comparison %>%
  mutate(
    Diff_Logit = abs(Rank_Logit - Rank_Actual),
    Diff_XGB = abs(Rank_XGB - Rank_Actual),
    Diff_Poisson = abs(Rank_Poisson - Rank_Actual),
    Diff_Linear = abs(Rank_Linear - Rank_Actual)
  )

rank_diff_summary <- ladder_comparison %>%
  summarise(
    Mean_Diff_Logit = mean(Diff_Logit, na.rm = TRUE),
    Mean_Diff_XGB = mean(Diff_XGB, na.rm = TRUE),
    Mean_Diff_Poisson = mean(Diff_Poisson, na.rm = TRUE),
    Mean_Diff_Linear = mean(Diff_Linear, na.rm = TRUE)
  )

rank_diff_summary
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

fixture_long <- bind_rows(fixture_home, fixture_away) %>%
  arrange(Date, Season, Round, Game_Id)
#####################################################

#####################################################
# Linear
fixture_2025 <- fixture_2025 %>%
  mutate(
    Margin_Pred = predict(spread_lm, newdata = fixture_2025),
    Margin_Pred_Result = ifelse(Margin_Pred > 0, 1, 0)
  )
#####################################################
# Poisson
fixture_2025 <- fixture_2025 %>%
  mutate(Poisson_Pred_Points = predict(poisson_model, newdata = ., type = "response"))

opponent_scores <- fixture_2025 %>%
  select(Game_Id, Team, Opponent, Opponent_Pred_Points = Poisson_Pred_Points)

fixture_2025 <- fixture_2025 %>%
  left_join(opponent_scores, by = c("Game_Id", "Team" = "Opponent", "Opponent" = "Team")) %>%
  mutate(
    Poisson_Pred_Margin = Poisson_Pred_Points - Opponent_Pred_Points,
    Poisson_Pred_Result = ifelse(Poisson_Pred_Margin > 0, 1, 0)
  )
#####################################################
# XG
predict_data <- fixture_2025 %>%
  select(
    Elo_Difference,
    Home,
    
    # avg_ metrics
    avg_Inside.50s,
    avg_Clearances,
    avg_Contested.Possessions,
    avg_Marks.Inside.50,
    avg_Goal.Assists,
    avg_Career_Games,
    
    # roll3_ metrics
    roll3_Inside.50s,
    roll3_Clearances,
    roll3_Contested.Possessions,
    roll3_Marks.Inside.50,
    roll3_Goal.Assists,
    roll3_Career_Games,
    
    # Team quality indicators
    is_premiership_coach,
    form_last_5,
    rest_days,
    is_short_turnaround
  )


fixture_2025 <- fixture_2025 %>%
  mutate(
    XGB_Win_Prob = predict(xgb_model, as.matrix(predict_data)),
    XGB_Forecast = ifelse(XGB_Win_Prob > 0.5, 1, 0)
  ) %>%
  group_by(Game_Id) %>%
  mutate(
    XGB_Winner = ifelse(XGB_Win_Prob == max(XGB_Win_Prob), 1, 0)
  ) %>%
  ungroup()

XGB_pred <- fixture_2025 %>% select(Round, Team, Opponent, XGB_Win_Prob, XGB_Winner)
#####################################################
ladder_2025_logit <- fixture_2025 %>%
  filter(Home == TRUE) %>%
  group_by(Team) %>%
  summarise(
    Logit_Wins = sum(Logit_Forecast, na.rm = TRUE),
    Logit_Points = Logit_Wins * 4,
    .groups = "drop"
  ) %>%
  arrange(desc(Logit_Points)) %>%
  mutate(Rank_Logit = row_number())

ladder_2025_linear <- fixture_2025 %>%
  filter(Home == TRUE) %>%
  group_by(Team) %>%
  summarise(
    Linear_Wins = sum(Margin_Pred_Result, na.rm = TRUE),
    Linear_Points = Linear_Wins * 4,
    .groups = "drop"
  ) %>%
  arrange(desc(Linear_Points)) %>%
  mutate(Rank_Linear = row_number())

ladder_2025_poisson <- fixture_2025 %>%
  filter(Home == TRUE) %>%
  group_by(Team) %>%
  summarise(
    Poisson_Wins = sum(Poisson_Pred_Result, na.rm = TRUE),
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

ladder_2025_xgb <- fixture_2025 %>%
  filter(Home == TRUE) %>%
  group_by(Team) %>%
  summarise(
    XGB_Wins = sum(XGB_Forecast, na.rm = TRUE),
    XGB_Points = XGB_Wins * 4,
    .groups = "drop"
  ) %>%
  arrange(desc(XGB_Points)) %>%
  mutate(Rank_XGB = row_number())

ladder_2025 <- ladder_2025_logit %>%
  left_join(ladder_2025_linear, by = "Team") %>%
  left_join(ladder_2025_poisson, by = "Team") %>%
  left_join(ladder_2025_xgb, by = "Team")
#####################################################