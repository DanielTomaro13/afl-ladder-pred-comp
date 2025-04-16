# AFL-ladder-comp

## 🏉 AFL ELO Ladder Prediction Challenge

Welcome to the **AFL Ladder Prediction Challenge**! This template provides a simple framework to build and test your own AFL prediction model using **ELO ratings** and **logistic regression**. The goal is to use past results and ELO dynamics to forecast the final ladder for the 2025 AFL season.

## 🔧 What's in the Template?

This R script walks you through:
- Loading data via the `fitzRoy` package
- Building an ELO model to track team strength
- Using logistic regression to forecast results
- Creating a predicted vs actual ladder
- Comparing your model’s predictions against real-world results

## 🧠 Learning Content

https://www.youtube.com/watch?v=HllkSUXRyQc&t=5s - Video on NCAAF predicting using R studio and ELO, great walkthrough and the inspiration for the challenge 

- 📚 **What is ELO?**  
  ELO is a rating system originally developed for chess. It’s now used in many sports to rank players and teams based on results and the strength of opponents.

- 📈 **Logistic Regression**  
  A statistical model for binary outcomes — like predicting win/loss from features like ELO difference, home ground, etc.

- 📦 **fitzRoy Docs**  
  https://fitzRoy.readthedocs.io/ — the go-to place to learn how to pull AFL data using R.

- 🧪 **K Value in ELO**  
  The `K` controls how responsive the ELO scores are after each game. Try tweaking this to see how your model changes!

- 🧮 **Metrics to Compare Predictions**  
  `mean(ladder_comparison$Rank_Diff)` gives the average ranking error — lower is better!

Feel free to add links, articles, or personal notes as you experiment!


---

Want me to add a scoring system or extra leaderboard code in here too?
