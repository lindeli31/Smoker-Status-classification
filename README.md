# Smoker Status Prediction: Comparative Study of Classification Methods

This repository contains the R code and final report for a statistical analysis aimed at predicting smoker status using biosignals.

## Description
The project compares three classification methods:
- Penalized Logistic Regression
- Linear and Quadratic Discriminant Analysis (LDA/QDA)
- Decision Trees and Random Forests

All analyses were performed using R, with results documented in the final report.

## Files
- `EsplorativeAnalysis.R`: Exploratory data analysis and preprocessing.
- `LDA_QDA.R`: Implementation of LDA and QDA models.
- `Tree_RandomForest.R`: Tree-based models (classification tree and random forest).
- `stepwise_penalty_regression.R`: Stepwise penalized logistic regression.
- `train_dataset.csv`: Training dataset (subset from Kaggle).
- `Report.pdf`: Final report summarizing methodology, results, and conclusions.

## Data Source
Dataset available on Kaggle: [Smoker Status Prediction Using Biosignals](https://www.kaggle.com/datasets/gauravduttakiit/smoker-status-prediction-using-biosignals)

## How to Use
1. Install R and required packages (`caret`, `MASS`, `rpart`, etc.)
2. Run each `.R` script in order.
3. View the results and compare with the final report.

## License
This project is for educational purposes only. The original dataset is under its own license.
