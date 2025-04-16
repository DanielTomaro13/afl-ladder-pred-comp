#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
#####################################################
odds <- fetch_betting_odds_footywire(start_season = '2010', end_season = lubridate::year(Sys.Date()))
colnames(odds)

odds$Predicted.Winner <- ifelse(odds$Home.Win.Odds < odds$Away.Win.Odds,
                                odds$Home.Team, odds$Away.Team)
odds$Actual.Winner <- ifelse(odds$Home.Score > odds$Away.Score,
                             odds$Home.Team, odds$Away.Team)

odds$Correct.Prediction <- odds$Predicted.Winner == odds$Actual.Winner

overall_accuracy <- mean(odds$Correct.Prediction, na.rm = TRUE)
print(paste("Accuracy of betting odds in predicting winners:", round(accuracy * 100, 2), "%"))
#####################################################
accuracy_by_season <- odds %>%
  group_by(Season) %>%
  summarise(
    Total.Games = n(),
    Correct = sum(Correct.Prediction, na.rm = TRUE),
    Accuracy = mean(Correct.Prediction, na.rm = TRUE) * 100
  )

ggplot(accuracy_by_season, aes(x = Season, y = Accuracy)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Accuracy)), vjust = -0.5, size = 3.5) +
  labs(title = "Accuracy of Betting Odds in Predicting AFL Winners by Season",
       x = "Season", y = "Accuracy (%)") +
  ylim(0, 100) +
  theme_minimal()
#####################################################
odds <- odds %>%
  arrange(Date) %>%
  mutate(
    Game.Number = row_number(),
    Cumulative.Correct = cumsum(Correct.Prediction),
    Cumulative.Accuracy = 100 * Cumulative.Correct / Game.Number
  )

ggplot(odds, aes(x = Game.Number, y = Cumulative.Accuracy)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Cumulative Accuracy of Betting Odds Over Time",
       x = "Game Number",
       y = "Cumulative Accuracy (%)") +
  theme_minimal()
