# Pharmaceutical-Sales-Forecasting---Time-Series-Analysis
This repository contains the Pharmaceutical Sales Forecasting project, which uses time series analysis techniques to predict sales trends for a specific drug category (M01AB).


# Project Overview
This project aims to forecast monthly pharmaceutical sales using historical transactional data from a pharmacy's Point-of-Sale system. The dataset spans six years (2014-2019) and focuses on drugs under the M01AB category (Anti-inflammatory and Antirheumatic products). The objective is to develop a predictive model to optimize inventory management, reduce wastage, and enhance decision-making.

# Dataset Source
The dataset was obtained from Kaggle:
🔗 Pharma Sales Data - Kaggle

# Methodology
The project follows a structured time series forecasting approach consisting of the following steps:

1️⃣ Data Preprocessing
Loaded the dataset and converted relevant columns to appropriate data types.
Handled missing data using imputation techniques.
Partitioned the dataset into training (2014-2018) and validation (2019) sets.
2️⃣ Exploratory Data Analysis
Visualized trends, seasonality, and cyclic patterns in the sales data.
Identified seasonal peaks (higher sales in winter months).
Performed autocorrelation analysis to detect statistical dependencies.
3️⃣ Forecasting Models
Several forecasting models were implemented and evaluated using Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE):

Linear Regression with Trend and Seasonality + Moving Average for Residuals
🏆 Best-performing model (RMSE: 9.803, MAPE: 5.403)
Linear Regression with Trend and Seasonality + AR(1) Model for Residuals
Seasonal ARIMA (1,1,1)(1,1,1)
Auto ARIMA
4️⃣ Model Evaluation
The best model was identified as the Two-Level Model using:
Linear Regression for trend and seasonality
Moving Average for residuals
Forecasts were generated for future 12 months.

# How to Use the Code
Install Required Packages
Run the following command in R to install necessary libraries:

install.packages(c("dplyr", "readr", "forecast", "zoo"))

Run the R Script

Load the dataset (salesmonthly.csv).
Execute the Time Series Analytics_Pharmaceutical-Sales-Forecasting.R script.
Adjust file paths as needed.
Interpret Results

Check forecast accuracy metrics (RMSE, MAPE).
Visualize sales trends and predictions.


# Key Findings
Sales exhibit strong seasonality, peaking in winter and declining in summer.
Two-level regression models (with trend & seasonality adjustments) outperform traditional ARIMA methods.
Optimal forecasting models can significantly improve inventory management.













