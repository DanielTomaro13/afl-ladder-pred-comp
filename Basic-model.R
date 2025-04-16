## Step by Step AFL model

# Load libaries
library(tidyr)
library(dplyr) # For data manipulation
library(ggplot2) # Optional - for graphical visualisation
library(fitzRoy) # For AFL data

# Fetch data 
# Which source will we use?
# I have chosen to go with AFL tables due to its speed and history
results <- fetch_results_afltables(2024)
colSums(is.na(results))
str(results)
# Function to restructure data so each game has two entries one for home and one for away
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
  
  # Combine and return
  return(bind_rows(afl_home, afl_away))
}

results <- restructure_afl_data(results)
# Check the result
colnames(results)
# So we can see we have added ELO, boolean values for if the row is related to the home or away team and a boolean result column in the form of W and L
# Let us make everything numeric and get rid of NA for ELO

results <- results %>% mutate(
  ELO = 0,
  Opp_ELO = 0,
  Result = ifelse(Result == "W", 1, Result),
  Result = ifelse(Result == "L", 0, Result),
  Result = ifelse(Result == "T", 0.5, Result),
  Result = as.numeric(Result)
)

# Order the data correctly so games with home and away perspective are after each other for correct ELO calculation
results <- results %>% arrange(
  Date,Season,Round,Team
)
head(results) # Verify

# Creating a teams data frame with ELO -- We will have to deal with team mergers at some point here because of the extensive data but it shouldn't matter too much
# Extract unique teams from the combined dataset
teams_elo <- unique(c(results$Team))

# Create a dataframe with teams and assign everyone an ELO of 1500 as a base ELO
# We use 1500 as it is the middle of typical ELO ranges and assumes all teams start equally skilled - When teams join, GCS, GWS we may give them a lower ELO to reflect under performance
teams_elo <- data.frame(
  Team = teams_elo,
  ELO = 1500,
  stringsAsFactors = FALSE
)

# Sort by team name alphabetically
teams_elo <- teams_elo[order(teams_elo$Team), ]

# The creation of ELO
# Load in the library
library(elo)

# Creating the ELO logic
# %%2 != 0 means when we divide by 2 does not equal 0 so we are doing it for every second number, we can see in the console
# We do this because we created an extra row for each game to see it in the home and away perspective

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

# Naive wins based off ELO, basically we are not taking much into account here, very simple win or loss - if ELO is higher then W if lower then L for our prediction
results <- results %>% mutate(
  ELO = as.numeric(ELO),
  Opp_ELO = as.numeric(Opp_ELO),
  Elo_Difference = ELO - Opp_ELO,
  Elo_Forecast_Pred = ifelse(ELO > Opp_ELO, 1, 0),
  Elo_Forecast_Result = ifelse(Elo_Forecast_Pred == Result, 1, 0),
)

## Naive Win Rate Calculation
win_rate <- sum(results$Elo_Forecast_Result, na.rm = TRUE) / 
  sum(!is.na(results$Elo_Forecast_Result))
win_rate * 100

## Adding ELO Difference to the data set

results <- results %>%
  mutate(Elo_Difference = ELO - Opp_ELO)

results <- results %>%
  mutate(Result_Binary = Result)  

# Run Logistic Regression Model 
# We use logistic regression as it is a classification model, there are two outcomes, win or lose therefore it will output a probability between 0-1. 

win_prob_glm_1 <- glm(
  Result_Binary ~ Elo_Difference + Home, # We take home team into account here, very basic model this is where you can add stuff to make it more advanced
  family = binomial,
  data = results
  # %>% filter(Season >= 1990 & Season <= 2024) # We can filter to use different parts of the data such as from 1990 when the VFL became the AFL
)

# Output model summary
summary(win_prob_glm_1)


## Test Model

# Predict probabilities
results$Win_Prob_Pred <- predict(win_prob_glm_1, type = "response")

# Accuracy of the model
results <- results %>%
  mutate(GLM_Forecast = ifelse(Win_Prob_Pred > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == Result_Binary, 1, 0))

glm_accuracy <- mean(results$GLM_Correct, na.rm = TRUE)
glm_accuracy * 100

## Filter for 2025

results_2025 <- results %>% filter(Season == 2025)

# Accuracy of the model
results_2025 <- results_2025 %>%
  mutate(GLM_Forecast = ifelse(Win_Prob_Pred > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == Result_Binary, 1, 0))

glm_accuracy_2025 <- mean(results_2025$GLM_Correct, na.rm = TRUE)
glm_accuracy_2025 * 100

# Ladder creation
# ACTUAL ladder
ladder_actual <- results_2025 %>%
  group_by(Team) %>%
  summarise(
    Games = n(),
    Wins_Actual = sum(Result == 1),
    Losses_Actual = sum(Result == 0),
    Draws_Actual = sum(Result == 0.5),
    WinPct_Actual = round(mean(Result) * 100, 1),
    Points_Actual = Wins_Actual * 4 + Draws_Actual * 2,
    Points_For = sum(Points_For),
    Points_Against = sum(Points_Against),
    Percentage_Actual = round((Points_For / Points_Against) * 100, 1)
  )

# PREDICTED ladder
ladder_predicted <- results_2025 %>%
  group_by(Team) %>%
  summarise(
    Wins_Pred = sum(GLM_Forecast == 1),
    Losses_Pred = sum(GLM_Forecast == 0),
    Draws_Pred = sum(Win_Prob_Pred > 0.45 & Win_Prob_Pred < 0.55),  # optional
    WinPct_Pred = round(mean(GLM_Forecast) * 100, 1),
    Points_Pred = Wins_Pred * 4 + Draws_Pred * 2,
    Percentage_Pred = round((sum(Points_For) / sum(Points_Against)) * 100, 1)
  )

# Merge both ladders
ladder_comparison <- ladder_actual %>%
  left_join(ladder_predicted, by = "Team") %>%
  arrange(desc(Points_Actual))

ladder_comparison <- ladder_comparison %>%
  mutate(Point_Diff = Points_Pred - Points_Actual,
         Rank_Actual = rank(-Points_Actual),
         Rank_Pred = rank(-Points_Pred),
         Rank_Diff = abs(Rank_Pred - Rank_Actual))

mean(ladder_comparison$Rank_Diff)
ladder_comparison












