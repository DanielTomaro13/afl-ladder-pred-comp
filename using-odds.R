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
odds <- fetch_betting_odds_footywire() %>%
  select(
    Date, Season, Round, Home.Team, Away.Team, Home.Score, Away.Score,
    Home.Margin, Away.Margin, Home.Win.Odds, Away.Win.Odds,
    Home.Line.Odds, Away.Line.Odds
  ) %>%
  rename(
    Home_Team = Home.Team,
    Away_Team = Away.Team,
    Home_Line = Home.Line.Odds,
    Away_Line = Away.Line.Odds
  ) %>%
  mutate(
    Home.Win.Prob = 1 / Home.Win.Odds,
    Away.Win.Prob = 1 / Away.Win.Odds,
    Home.Line.Prob = 1 / 1.90,
    Away.Line.Prob = 1 / 1.90
  ) %>%
  mutate(
    Total.Win.Prob = Home.Win.Prob + Away.Win.Prob,
    Home.Win.Prob = Home.Win.Prob / Total.Win.Prob,
    Away.Win.Prob = Away.Win.Prob / Total.Win.Prob
  ) %>%
  select(-Total.Win.Prob)

restructure_odds_data <- function(odds_df) {
  home_odds <- odds_df %>%
    transmute(
      Date, Season, Round,
      Team = Home_Team,
      Opponent = Away_Team,
      Win_Prob = Home.Win.Prob,
      Margin = Home_Line,
      Home = TRUE
    )
  
  away_odds <- odds_df %>%
    transmute(
      Date, Season, Round,
      Team = Away_Team,
      Opponent = Home_Team,
      Win_Prob = Away.Win.Prob,
      Margin = Away_Line,
      Home = FALSE
    )
  
  bind_rows(home_odds, away_odds)
}

odds <- restructure_odds_data(odds) %>%
  arrange(Date, Season, Round)
#####################################################
results <- fetch_results_afltables(2010:2020)
restructure_afl_data <- function(afl_data) {
  # Home team perspective
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
  
  # Away team perspective
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
colnames(results)

results <- results %>% mutate(
  ELO = 0,
  Opp_ELO = 0,
  Result = ifelse(Result == "W", 1, Result),
  Result = ifelse(Result == "L", 0, Result),
  Result = ifelse(Result == "T", 0.5, Result),
  Result = as.numeric(Result)
)

results <- results %>% arrange(
  Date,Season,Round,Team
)
head(results) # Verify

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
    
    ## Get Current ELO ##
    
    ELO_A <- as.numeric(teams_elo[teams_elo$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(teams_elo[teams_elo$Team == Team_B, "ELO"])
    
    ## Load current ELO into the main data set 
    
    results$ELO[i] <- ELO_A
    results$Opp_ELO[i] <- ELO_B
    
    results$ELO[i+1] <- ELO_B
    results$Opp_ELO[i+1] <- ELO_A
    
    ## Update ELOs
    
    R_A <- 10^(ELO_A/400)
    R_B <- 10^(ELO_B/400)
    
    E_A <- R_A/(R_A + R_B)
    E_B <- R_B/(R_A + R_B)
    
    Elo_Updated_A <- ELO_A + 20 * (Result_A - E_A) # Our K value here is 20, the K value controls how much ELO moves after each game, higher more dramatically and lower less. 
    Elo_Updated_B <- ELO_B + 20 * (Result_B - E_B)
    
    ## Update Team ELOs
    
    teams_elo[teams_elo$Team == Team_A, "ELO"] <- Elo_Updated_A
    teams_elo[teams_elo$Team == Team_B, "ELO"] <- Elo_Updated_B
    
  }
}

results <- results %>%
  mutate(Elo_Difference = ELO - Opp_ELO)

results <- results %>%
  mutate(Result_Binary = Result)  
#####################################################
# Adding Odds
results <- results %>%
  left_join(
    odds %>% select(Date, Season, Round, Team, Opponent, Win_Prob, Margin),
    by = c("Date", "Season", "Round", "Team", "Opponent")
  )
#####################################################
win_prob_glm_1 <- glm(
  Result_Binary ~ Elo_Difference + Home, 
  family = binomial,
  data = results
)
summary(win_prob_glm_1)

results$Win_Prob_Pred <- predict(win_prob_glm_1, type = "response")

results <- results %>%
  mutate(GLM_Forecast = ifelse(Win_Prob_Pred > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == Result_Binary, 1, 0))

glm_accuracy <- mean(results$GLM_Correct, na.rm = TRUE)
glm_accuracy * 100

# Bookie Accuracy
results <- results %>%
  mutate(
    Bookie_Forecast = ifelse(Win_Prob > 0.5, 1, 0),
    Bookie_Correct = ifelse(Bookie_Forecast == Result_Binary, 1, 0)
  )
bookie_accuracy <- mean(results$Bookie_Correct, na.rm = TRUE)
bookie_accuracy * 100
#####################################################
results <- results %>%
  mutate(Bookie_Error = abs(Win_Prob - Result_Binary))

residual_model <- lm(Bookie_Error ~ Elo_Difference + Home, data = results)
summary(residual_model)
#####################################################
win_prob_glm_weighted <- glm(
  Result_Binary ~ Elo_Difference + Home,
  family = binomial,
  data = results,
  weights = 1 - abs(0.5 - Win_Prob)  # Less weight for close games
)

results <- results %>%
  mutate(GLM_Weighted_Forecast = ifelse(Weighted_Win_Prob_Pred > 0.5, 1, 0),
         GLM_Weighted_Correct = ifelse(GLM_Weighted_Forecast == Result_Binary, 1, 0))

glm_weighted_accuracy <- mean(results$GLM_Weighted_Correct, na.rm = TRUE)
glm_weighted_accuracy * 100
#####################################################