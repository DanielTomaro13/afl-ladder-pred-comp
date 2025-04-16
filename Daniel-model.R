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
#####################################################
results <- fetch_results_afltables(1897:2025)
colnames(results)
restructure_afl_data <- function(afl_data) {
  afl_home <- data.frame(
    Date = afl_data$Date,
    Season = afl_data$Season,
    Round = afl_data$Round.Number,
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

results <- results %>%
  mutate(
    game_id = paste0(
      Season, "_R", Round, "_",
      pmin(Team, Opponent), "_vs_", pmax(Team, Opponent)
    )
  ) %>% arrange(Round, game_id)

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
#####################################################
# Add form
results <- results %>%
  arrange(Team, Date) %>%
  group_by(Team) %>%
  mutate(
    form_last_5 = coalesce(lag(slide_dbl(Result, ~mean(.x, na.rm = TRUE), .before = 4, .complete = TRUE)), 0)
  ) %>%
  ungroup()
#####################################################
# Logisitc Regression
logit_model <- glm(
  Result_Binary ~ Elo_Difference + Home + form_last_5, 
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
#####################################################
# Linear Regression 

spread_lm <- lm(Spread ~ Elo_Difference + Home + form_last_5, data = results)

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
  Points_For ~ Elo_Difference + Home + form_last_5,
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
#####################################################
# XG Boost

xgb_data <- results %>%
  filter(!is.na(Result_Binary)) %>%
  select(Result_Binary, Elo_Difference, Home, form_last_5)

# Split into features (X) and target (y)
X <- as.matrix(xgb_data %>% select(-Result_Binary))
y <- xgb_data$Result_Binary

# Convert to DMatrix (xgboost's format)
dtrain <- xgb.DMatrix(data = X, label = y)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 3,
  eta = 0.1
)

xgb_model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = 100,
  verbose = 0
)

results$XGB_Win_Prob <- predict(xgb_model, as.matrix(results %>% select(Elo_Difference, Home, form_last_5)))
results$XGB_Forecast <- ifelse(results$XGB_Win_Prob > 0.5, 1, 0)
results$XGB_Correct <- ifelse(results$XGB_Forecast == results$Result_Binary, 1, 0)

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
    Actual_Wins = sum(Result == 1),
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