## 673_Time Series Analytics - Final Project
## USE FORECAST LIBRARY.

library(dplyr)
library(readr)
library(forecast)
library(zoo)

###################.  DataFrame Creation and Pre-processing     .####################

# Set working directory for locating files.
setwd("C:/Users/ishra/Downloads/BAN673")


# # Create data frame.
df <- read.csv("salesmonthly.csv")  # Adjust path as needed

# See the first 6 records of the file.
print(head(df))

# Check the structure of the data frame
str(df)

# Pre-processing step: Convert the M01AB column to numeric
df$M01AB <- as.numeric(as.character(df$M01AB))

# Convert the 'datum' column to Date type
df$datum <- as.Date(df$datum, format="%d-%m-%Y")

# Checking if the conversion is successful
summary(df$datum)

# Pre-processing step:Imputation- Subset to get only January entries
january_data <- df[format(df$datum, "%m") == "01",]
january_data

# Calculate the average for January 
average_january <- mean(january_data$M01AB[january_data$M01AB != 0], na.rm = TRUE)
print(average_january)  # Ensure this is a reasonable number and not NA or 0

# Impute the missing value
df$M01AB[df$datum == as.Date("2017-01-31")] <- average_january

# View the updated data
print(head(df))

# Pre-processing step:Convert M01AB column from float to integer
df$M01AB <- as.integer(df$M01AB)

# Check the result of the conversion
print(head(df))

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# Arguments start and end are pairs: (season number, period number).
sales.ts <- ts(df$M01AB, 
                 start = c(2014, 1), end = c(2019, 9), freq = 12)
sales.ts

###################. Check for predictability     .####################

# Use plot() function to create a data plot of the historical data.
plot(sales.ts, 
     xlab = "Time", 
     ylab = "Sales in Millions ", 
     xaxt = "n",
     ylim = c(100,220) ,
     bty = "l",
     xlim = c(2014, 2019),
     main = "Monthly Sales of Pharma Drug- M01AB (2014-2019)", 
     lwd = 2,
     col = "blue")

axis(1, at = seq(2014, 2019, by = 1), labels = seq(2014, 2019, by = 1))

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 10 
nTrain <- length(sales.ts) - nValid
train.ts <- window(sales.ts, start = c(2014, 1), end = c(2014, nTrain))
train.ts
valid.ts <- window(sales.ts, start = c(2014, nTrain + 1), 
                   end = c(2014, nTrain + nValid))
valid.ts

# Use Arima() function to fit AR(1) model for Walmart Revenue
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
M01AB_sales.ar1<- Arima(sales.ts, order = c(1,0,0))
summary(M01AB_sales.ar1)

# Checking the predictability of the pharma M01AB sales data
# Approach 1 for predictability

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.5807
s.e. <- 0.0975
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

# Create first difference of Pharma sales data using diff() function.
diff.sales <- diff(sales.ts, lag = 1)
diff.sales

# Approach 2 for predictability

# Use Acf() function to identify autocorrelation for first differenced
# pharma sales data and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.sales, lag.max = 12, 
    main = "Autocorrelation of first differencing for pharma sales")

###################.  Model: Trailing MA     .####################

# Develop 3 trailing MAs with the window width of 4, 6, and 12 for the training partition
# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_6 <- rollmean(train.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

# Create forecast for the validation data for the window widths 
# of k = 4, 5, and 12. 
ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_6.pred <- forecast(ma.trailing_6, h = nValid, level = 0)
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_6.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)

###################.Model: Linear Trend & Seasonality + trailing MA for residuals .####################

# Fit a regression model with linear trend and seasonality for
# training partition. 
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

# Create regression forecast with trend and seasonality for 
# validation period.
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred

# Identify and display regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
trend.seas.res <- trend.seas$residuals
trend.seas.res

# Apply trailing MA for residuals with window width k = 3
# for training partition.
ma.trail.res <- rollmean(trend.seas.res, k = 3, align = "right")
ma.trail.res

# Regression residuals in validation period.
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

# Create a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(valid.df) <- c("Monthly Sales", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)

# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(sales.ts ~ trend  + season)
summary(tot.trend.seas)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 3, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                                tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

# Plot regression residuals data and trailing MA based on residuals.
# Plot original  time series data and regression model.
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(100, 300), 
     bty = "l", xlim = c(2014, 2020), lwd =1, xaxt = "n",
     main = "Pharma Data for M01AB and Regression with Trend and Seasonality") 
axis(1, at = seq(2014, 2020.25, 1), labels = format(seq(2014, 2020.25, 1)))
lines(tot.trend.seas$fitted, col = "blue", lwd = 2)
lines(tot.trend.seas.pred$mean, col = "blue", lty =5, lwd = 2)
legend(2014,260, legend = c("Pharma M01AB Sales", "Regression",
                             "Regression Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2019.70, 2019.70), c(0, 300))
text(2016,300, "Data Set")
text(2020, 300, "Future")
arrows(2014, 220, 2019.70, 220, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.70, 220, 2020.9,220 , code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(tot.trend.seas.pred$fitted, sales.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)

###################.Mode 2: HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING  .####################

## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATIC
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for sales data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(sales.ts, model = "ZZZ")
HW.ZZZ # Model appears to be (M, Ad, M), with alpha = 0.5334, beta = 0.0014,
# gamma = 0.1441, and phi = 0.9698.

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# Identify performance measures for HW forecast.
round(accuracy(HW.ZZZ.pred$fitted, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)

###################.Mode 3: All Regression models  .#############################

## FIT REGRESSION MODEL WITH LINEAR TREND: MODEL 1. 
## FORECAST AND PLOT DATA, AND MEASURE ACURACY.

# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
train.lin <- tslm(train.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

## FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND: MODEL 2. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 3. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)


# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: MODEL 4. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL 5. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic trend and seasonal model.
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred

# Use accuracy() function to identify common accuracy measures
# for the developed forecast in the validation period.
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.season.pred$mean, valid.ts),3)

## FIT REGRESSION MODELS WITH LINEAR TREND, SEASONALITY AND LINEAR TREND 
## AND SEASONALITY FOR ENTIRE DATASET FOE 3 BEST PERFORMING MODELS.  
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend model- Model 1
lin.trend <- tslm(sales.ts ~ trend)

# See summary of linear trend equation and associated parameters.
summary(lin.trend)

# Apply forecast() function to make predictions for ts with 
# linear trend  data in 12 future periods.
lin.trend.pred <- forecast(lin.trend, h = 12, level = 0)

# Use tslm() function to create regression model with linear trend and seasonality- Model 2
lin.season <- tslm(sales.ts ~ trend + season)

# See summary of linear trend and seasonality equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in 12 future periods.
lin.season.pred <- forecast(lin.season, h = 12, level = 0)
lin.season.pred

# Use tslm() function to create seasonal model- Model 3
lin.onlyseason <- tslm(sales.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(lin.onlyseason)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in 12 future periods.
lin.onlyseason.pred <- forecast(lin.onlyseason, h = 12, level = 0)

# Use accuracy() function to identify common accuracy measures
# for naive model, seasonal naive, and 3 best performing models in entire dataset 
round(accuracy(lin.trend.pred$fitted, sales.ts),3)
round(accuracy(lin.season.pred$fitted, sales.ts),3)
round(accuracy(lin.onlyseason.pred$fitted, sales.ts),3)
round(accuracy((naive(sales.ts))$fitted, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)

# Use Acf() function to identify autocorrelation for the model residuals 
# (training), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(train.lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Pharma sales Training Residuals")

###################.Model: Linear regression with trend and seasonality + AR(1) model for residuals  .###################

## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## PLOT DATA AND IDENTIFY ACCURACY MEASURES.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Pharma M01AB sales Training Residuals of Residuals")

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Revenue", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# Use accuracy() function to identify common accuracy measures
round(accuracy(valid.two.level.pred, valid.ts),3)

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY 
## FOR ENTIRE DATASET. FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonality model.
lin.season <- tslm(sales.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 12 periods.  
lin.season.pred <- forecast(lin.season, h = 12, level = 0)
lin.season.pred

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 periods.
residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for AR(1) model’s residuals for Entire Data Set")

# Identify forecast for the future 12 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
lin.season.ar1.pred <- lin.season.pred$mean + residual.ar1.pred$mean
lin.season.ar1.pred

# Create a data table with linear trend and seasonal forecast 
# for 12 future periods,
# AR(1) model for residuals for 12 future periods, and combined 
# two-level forecast for 12 future periods. 
table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar1.pred$mean, lin.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df

# Plot historical data, predictions for historical data, and forecast 
# for 12 future periods.
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", 
     ylim = c(100, 350), xaxt = "n",
     bty = "l", xlim = c(2014, 2020), lwd = 2,
     main = "Two-Level Forecast: Regression with Trend and Seasonlity + AR(1)
     for Residuals") 
axis(1, at = seq(2014, 2020, 1), labels = format(seq(2014, 2020, 1)))
lines(lin.season$fitted + residual.ar1$fitted, col = "blue", lwd = 2)
lines(lin.season.ar1.pred, col = "blue", lty = 5, lwd = 2)
legend(2014,350, legend = c("Sales Series for Training and Valiadaton Periods", 
                             "Two-Level Forecast for Training and Valiadtion Periods", 
                             "Two-Level Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(2018.90, 2018.90), c(0, 300))
lines(c(2019.70, 2019.70), c(0, 300))
text(2016, 260, "Training")
text(2019.3, 260, "Validation")
text(2019.95, 260, "Future")
arrows(2014, 250, 2018.9, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2018.9, 250, 2019.7, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.7, 250, 2020.2, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)

###################.Model: Seasonal ARIMA & Auto ARIMA .###################

## FIT ARIMA(1,1,1)(1,1,1) MODEL.
# Use Arima() function to fit ARIMA(1,1,1)(1,1,1) model for 
# trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
train.arima.seas <- Arima(train.ts, order = c(1,1,1), 
                          seasonal = c(1,1,1)) 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

## FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Use accuracy() function to identify common accuracy measures 
# for validation period forecast:

# (1) ARIMA(1,1,1)(1,1,1) model; and 
# (2) Auto ARIMA model.
round(accuracy(train.arima.seas.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

## FIT SEASONAL ARIMA AND AUTO ARIMA MODELS FOR ENTIRE DATA SET. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use arima() function to fit seasonal ARIMA(1,1,1)(1,1,1) model 
# for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
arima.seas <- Arima(sales.ts, order = c(1,1,1), 
                    seasonal = c(1,1,1)) 
summary(arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 12 periods. 
arima.seas.pred <- forecast(arima.seas, h = 12, level = 0)
arima.seas.pred

# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 12 future periods.
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", 
     ylim = c(100, 350), xaxt = "n",
     bty = "l", xlim = c(2014, 2020), lwd = 2,
     main = "Seasonal ARIMA(1,1,1)(1,1,1)[12] Model for Entire Data Set") 
axis(1, at = seq(2014, 2020, 1), labels = format(seq(2014, 2020, 1)))
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2014,350, legend = c("Sales Series", 
                            "Seasonal ARIMA Forecast", 
                            "Seasonal ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(2019.70, 2019.70), c(0, 300))
text(2016,300, "Data Set")
text(2020, 300, "Future")
arrows(2014, 250, 2019.70, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.70, 250, 2020.2,250 , code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(sales.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 12 future periods.
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", 
     ylim = c(100, 350), xaxt = "n", 
     bty = "l", xlim = c(2014, 2020), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(2014, 2020, 1), labels = format(seq(2014, 2020, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2014,350, legend = c("M01AB Sales Series", 
                             "Auto ARIMA Forecast", 
                             "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(2019.70, 2019.70), c(0, 300))
text(2016,300, "Data Set")
text(2020, 300, "Future")
arrows(2014, 250, 2019.70, 250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.70, 250, 2020.1,250 , code = 3, length = 0.1,
       lwd = 1, angle = 30)

###################.    Accuracy Measures    .###################

# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:
# (1) Linear regression with trend and seasonality + trailing MA for residuals
# (2) linear trend and seasonality model only
# (3) two-level model (linear trend and seasonality model + AR(1) model for residuals),
# (4) Seasonal ARIMA (1,1,1)(1,1,1) Model,
# (5) Auto ARIMA Model,
# (6) naive forecast, and
# (7) Seasonal naive forecast

round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, sales.ts), 3)
round(accuracy(lin.season$fitted, sales.ts), 3)
round(accuracy(lin.season$fitted + residual.ar1$fitted, sales.ts), 3)
round(accuracy(arima.seas.pred$fitted, sales.ts), 3)
round(accuracy(auto.arima.pred$fitted, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)
round(accuracy((naive(sales.ts))$fitted, sales.ts), 3)


