rm(list = ls())
graphics.off()

#  Install and load packages
install.packages("tseries")  
install.packages("xts") 
install.packages("forecast") 
install.packages("tsbox")    
install.packages("seasonal")
install.packages("mFilter")
install.packages("bruceR", dep=TRUE)
install.packages("fanplot")
install.packages("dplyr")
install.packages("readxl")
install.packages("vars")
install.packages("MASS")
install.packages("bruceR")

library(tseries)  
library(xts)
library(forecast) 
library(tsbox)    
library(seasonal)
library(mFilter)
library(dplyr)
library(readxl)
library(vars)
library(MASS)
library(bruceR)


# Load transposed data 

rawData <- read_excel("retail_sales_transp_March.xlsx")

# Convert the data frame to a xts object and use the first column as date

data <- as.xts(x = rawData[, -1], order.by = as.Date(rawData$Date))

# take the first difference of the log to have growth rates 

data$df_Inflation <- diff(log(data$Inflation), lag = 1, differences = 1)
data$df_Retail_sales <- diff(log(data$Retail_sales), lag = 1, differences = 1)
data$df12_Retail_sales <- diff(log(data$Retail_sales), lag = 12, differences = 1)
data$df_CHFEUR <- diff(log(data$CHFEUR), lag = 1, differences = 1)
data$df_Unemploy <- diff(log(data$Unemploy), lag = 1, differences = 1)
data$df_Brent_COIL <- diff(log(data$Brent_COIL), lag = 1, differences = 1)
data$df_Retail_EU <- diff(log(data$Retail_sales_EU), lag = 1, differences = 1)

# delete first row because of differencing 

data <- tail(data, -1)

# Visualize data

plot.xts(cbind(data$Retail_sales),  main = "Retail Sales Index (not seasonally adjusted)")
plot.xts(cbind(data$df_Retail_sales),  main = "Retail Sales Growth rate (not seasonally adjusted)")
plot.xts(cbind(data$Inflation),  main = "Inflation")
plot.xts(cbind(data$Brent_COIL),  main = "Brent Crude Oil")
plot.xts(cbind(data$CHFEUR),  main = "CHF/EUR")

### Seasonally adjust df Retail sales with monthly dummies 

monthlyDummies <- forecast::seasonaldummy(ts_ts(data$df_Retail_sales)) 
modelretailseason <- lm(data$df_Retail_sales ~ monthlyDummies)


plot.xts(as.xts(modelretailseason$fitted.values),  main = "Swiss Retail Sales: fitted with seasonal dummy")
plot.xts(as.xts(modelretailseason$residuals),  main = "Swiss Retail Sales: deviations from seasonal dummy")

data$Retail_sales_cycle  <- modelretailseason$residuals

## Doing the same with Retail Sales EU

modelretailseason <- lm(data$df_Retail_EU ~ monthlyDummies)

plot.xts(as.xts(modelretailseason$fitted.values),  main = "EU Retail Sales: fitted with seasonal dummy")
plot.xts(as.xts(modelretailseason$residuals),  main = "EU Retail Sales: deviations from seasonal dummy")

data$Retail_EU_cycle <- modelretailseason$residuals

# Explore the CCF Plot for different candidates for the exogenous variable 

ccf(as.ts(data$Retail_sales_cycle["/2019-12-31"]),as.ts(data$Retail_EU_cycle["/2019-12-31"]))
ccf(as.ts(data$Retail_sales_cycle["/2019-12-31"]),as.ts(data$df_CHFEUR["/2019-12-31"]))
ccf(as.ts(data$Retail_sales_cycle["/2019-12-31"]),as.ts(data$df_Inflation["/2019-12-31"]))
ccf(as.ts(data$Retail_sales_cycle["/2019-12-31"]),as.ts(data$df_Brent_COIL["/2019-12-31"]))
ccf(as.ts(data$Retail_sales_cycle["/2019-12-31"]),as.ts(data$df_Unemploy["/2019-12-31"]))

# Correlation only found for Retail EU (seasonally adjusted) - nothing for CHFEUR or Inflation

## Include pandemic dummies to refine the model and exlude the correct pandemic months 
## Only until December 2021 because shops opened up earlier

pandemicDummies <- as.xts(ts(matrix(0, nrow = 320, ncol = 24), start = c(2000, 2), frequency = 12))

## Suggesting a pandemic period from 01/01/2020 to 31/12/21 - earlier normalization for retail 

pandemicDummies["2020-01-01", 1] <- 1
pandemicDummies["2020-02-01", 2] <- 1
pandemicDummies["2020-03-01", 3] <- 1
pandemicDummies["2020-04-01", 4] <- 1
pandemicDummies["2020-05-01", 5] <- 1
pandemicDummies["2020-06-01", 6] <- 1
pandemicDummies["2020-07-01", 7] <- 1
pandemicDummies["2020-08-01", 8] <- 1
pandemicDummies["2020-09-01", 9] <- 1
pandemicDummies["2020-10-01", 10] <- 1
pandemicDummies["2020-11-01", 11] <- 1
pandemicDummies["2020-12-01", 12] <- 1
pandemicDummies["2021-01-01", 13] <- 1
pandemicDummies["2021-02-01", 14] <- 1
pandemicDummies["2021-03-01", 15] <- 1
pandemicDummies["2021-04-01", 16] <- 1
pandemicDummies["2021-05-01", 17] <- 1
pandemicDummies["2021-06-01", 18] <- 1
pandemicDummies["2021-07-01", 19] <- 1
pandemicDummies["2021-08-01", 20] <- 1
pandemicDummies["2021-09-01", 21] <- 1
pandemicDummies["2021-10-01", 22] <- 1
pandemicDummies["2021-11-01", 23] <- 1
pandemicDummies["2021-12-01", 24] <- 1


## Check the different exogenous variables with the model (for later comparability no seasonal model - 
## because we had som non stationarity for some tau with the SARIMA model) 
## Use Monthly dummies (seasonal adjusted time-series)

forecast::auto.arima(data$Retail_sales_cycle,
                     ic="bic",
                     xreg = pandemicDummies["2000-02-01/2024-03-01"], 
                     max.d = 0, 
                     trace = TRUE,
                     stepwise = FALSE)

forecast::auto.arima(data$Retail_sales_cycle,
                     ic="bic",
                     xreg = cbind(pandemicDummies["2000-02-01/2024-03-01"], 
                                  data$Retail_EU_cycle),
                     max.d = 0, 
                     trace = TRUE,
                     stepwise = FALSE)

forecast::auto.arima(data$Retail_sales_cycle,
                     ic="bic",
                     xreg = cbind(pandemicDummies["2000-02-01/2024-03-01"], 
                                  data$df_CHFEUR),
                     max.d = 0, 
                     trace = TRUE,
                     stepwise = FALSE)

forecast::auto.arima(data$Retail_sales_cycle,
                     ic="bic",
                     xreg = cbind(pandemicDummies["2000-02-01/2024-03-01"], 
                                  data$Retail_EU_cycle,
                                  data$df_CHFEUR),
                     max.d = 0, 
                     trace = TRUE,
                     stepwise = FALSE)

## Double check results with SARIMA

forecast::auto.arima(ts_ts(data$df_Retail_sales),
                     ic="bic",
                     xreg = pandemicDummies["2000-02-01/2024-03-01"],
                     max.d = 0, 
                     trace = TRUE,
                     stepwise = FALSE)

forecast::auto.arima(ts_ts(data$df_Retail_sales),
                     ic="bic",
                     xreg = cbind(pandemicDummies["2000-02-01/2024-03-01"], 
                                  data$df_Retail_EU),
                     max.d = 0, 
                     trace = TRUE,
                     stepwise = FALSE)

forecast::auto.arima(ts_ts(data$df_Retail_sales),
                     ic="bic",
                     xreg = cbind(pandemicDummies["2000-02-01/2024-03-01"], 
                                  data$df_Retail_EU,
                                  lag(data$df_Retail_EU)
                     ),
                     max.d = 0, 
                     trace = TRUE,
                     stepwise = FALSE)

forecast::auto.arima(ts_ts(data$df_Retail_sales),
                     ic="bic",
                     xreg = cbind(pandemicDummies["2000-02-01/2024-03-01"], 
                                  data$df_CHFEUR),
                     max.d = 0, 
                     trace = TRUE,
                     stepwise = FALSE)

forecast::auto.arima(ts_ts(data$df_Retail_sales),
                     ic="bic",
                     xreg = cbind(pandemicDummies["2000-02-01/2024-03-01"], 
                                  data$df_Retail_EU,
                                  data$df_CHFEUR),
                     max.d = 0, 
                     trace = TRUE,
                     stepwise = FALSE)


## Model with Exogenous variable Retail EU best performing without any lag 

## The model we choose - only Retail Sales EU as exogenous 

modelretailsales <- forecast::auto.arima(ts_ts(data$df_Retail_sales),
                                         ic="bic",
                                         xreg = cbind(pandemicDummies["2000-02-01/2024-03-01"], 
                                                      data$df_Retail_EU
                                         ),
                                         max.d = 0, 
                                         trace = TRUE,
                                         stepwise = FALSE)

### It chose the ARIMA(0,0,1)(2,1,0)[12] 

### Forecast for Retail EU 

modelretaileu <- forecast::auto.arima(ts_ts(data$df_Retail_EU), 
                                      xreg = pandemicDummies["2000-02-01/2024-03-01"], 
                                      ic = "bic",
                                      trace = TRUE,
                                      stepwise = FALSE
)

modelretaileu$bic  

forecasteu <- forecast::forecast(modelretaileu, 
                                 xreg = pandemicDummies["2024-04-01/2025-03-01"])

plot(forecasteu)


## Forecast CHFEUR 

modelchfeur <- forecast::auto.arima(data$df_CHFEUR, 
                                    xreg = pandemicDummies["2000-02-01/2024-03-01"], 
                                    ic = "bic",
                                    trace = TRUE,
                                    stepwise = FALSE)

### Forecast Retail Sales Switzerland 

forecastretail <- forecast::forecast(modelretailsales, 
                                     xreg = cbind(pandemicDummies["2024-04-01/2025-03-01"],
                                                  forecasteu$mean))

plot(forecastretail)

### Reverse the differencing and log to get a point forecast 

cumulative_forecasted_diffs <- cumsum(forecastretail$mean[1:12])

### Add on to the last observed value 

forecasted_series_final <- cumulative_forecasted_diffs + log(tail(data$Retail_sales, 1)[[1]])

### Convert back to the original scale

forecasted_series_original_scale <- exp(forecasted_series_final)

print(forecasted_series_original_scale)


## Check VAR Model 

dta <-data[,c("Retail_sales_cycle","Retail_EU_cycle","df_CHFEUR")]
VARselect(dta, lag.max =4, type = "const", exogen=pandemicDummies["2000-02-01/2024-03-01"])

varmodel <- VAR(dta, p = 4, type = "const")
granger_causality(varmodel, var.x = "Retail_sales_cycle", var.y = c("Retail_EU_cycle","df_CHFEUR"))

forecast <- predict(varmodel, n.ahead = 12)
print(forecast)




# Evaluation 

## Now for evaluation only use the data before the pandemic 

dataeval <- data[as.Date(index(data)) <= as.Date("2020-01-01"), ]

tEvaluationPeriods <- 70
tPeriods = length(dataeval[,"Retail_sales"])
forecastHorizon = 12
nModels = 6 # we compare these models: univariate m/m, y/y, multivariate with EU Retail, CHFEUR, naive, average
forecastRolling <- matrix(,tEvaluationPeriods,nModels)
for (tauEvaluationPeriod in 1:tEvaluationPeriods){
  
  lastPeriod <- tPeriods-tEvaluationPeriods+tauEvaluationPeriod-forecastHorizon 
  
  # Univariate model, m/m 
  modelRetail_temp <- forecast::Arima(dataeval$Retail_sales_cycle[1:lastPeriod],
                                      order = c(1, 0, 1))
  forecastRetail_temp <- forecast::forecast(modelRetail_temp, h = forecastHorizon)
  forecastRolling[tauEvaluationPeriod,1] <- sum(forecastRetail_temp$mean[1:forecastHorizon]) # y/y forecast is sum of m/m (with logs)
  
  # Univariate model, y/y
  modelRetail_temp = forecast::Arima(dataeval$df12_Retail_sales[12:lastPeriod],
                                     order = c(1, 0, 1))
  forecastRetail_temp = forecast::forecast(modelRetail_temp, h = forecastHorizon)
  forecastRolling[tauEvaluationPeriod,2] <- forecastRetail_temp$mean[forecastHorizon]
  
  # Multivariate model
  modelRetailExo_temp = forecast::Arima(dataeval$Retail_sales_cycle[1:lastPeriod],
                                        order = c(0, 0, 1),
                                        xreg = dataeval$Retail_EU_cycle[1:lastPeriod])
  modelRetaileu_temp <- forecast::Arima(ts_ts(dataeval$Retail_sales_cycle[1:lastPeriod]), 
                                        order = c(2, 0, 3))
  forecasteu_temp <- forecast::forecast(modelRetaileu_temp, 
                                        h = forecastHorizon)  
  forecastRetail_temp <- forecast::forecast(modelRetailExo_temp,
                                            xreg = forecasteu_temp$mean,
                                            h = forecastHorizon)  
  forecastRolling[tauEvaluationPeriod,3] <- sum(forecastRetail_temp$mean[1:forecastHorizon])    
  
  # Multivariate with Retail EU and CHFEUR 
  
  modelRetailExo_temp = forecast::Arima(dataeval$Retail_sales_cycle[1:lastPeriod],
                                        order = c(0, 0, 1),
                                        xreg = cbind(dataeval$Retail_EU_cycle[1:lastPeriod],
                                                     dataeval$df_CHFEUR[1:lastPeriod]))
  modelRetaileu_temp <- forecast::Arima(dataeval$Retail_sales_cycle[1:lastPeriod], 
                                        order = c(2, 0, 3))
  modelCHFEUR_temp <- forecast::Arima(dataeval$df_CHFEUR[1:lastPeriod], 
                                      order = c(0, 0, 1))
  forecasteu_temp <- forecast::forecast(modelRetaileu_temp, 
                                        h = forecastHorizon)  
  forecastCHFEUR_temp <- forecast::forecast(modelCHFEUR_temp, h = forecastHorizon)
  forecastRetail_temp <- forecast::forecast(modelRetailExo_temp,
                                            xreg = cbind(forecasteu_temp$mean, 
                                                         forecastCHFEUR_temp$mean),
                                            h = forecastHorizon)  
  forecastRolling[tauEvaluationPeriod,4] <- sum(forecastRetail_temp$mean[1:forecastHorizon]) 
  
  # "Naive", no change forecast
  
  forecastRolling[tauEvaluationPeriod,5] <- dataeval$df12_Retail_sales[lastPeriod]
  
  # average - just take the average of different models 
  forecastRolling[tauEvaluationPeriod,6] <- mean(forecastRolling[tauEvaluationPeriod,1:5])
  
}

matplot(1:70, cbind(forecastRolling,dataeval$df12_Retail_sales[171:240])) 

## Calculate the forecast errors 

actualData <- dataeval$df12_Retail_sales[(length(dataeval$df12_Retail_sales)-tEvaluationPeriods+1):length(dataeval$df12_Retail_sales)]
actualDataMatrix <- kronecker(matrix(1,1,6),actualData)
forecastErrors <- actualDataMatrix - forecastRolling

## Calculate the loss function (quadratic error and absolute error)

quadraticLoss <- forecastErrors^2 
absoluteLoss <- abs(forecastErrors)

## Calculate the mean loss for each model 

colMeans(quadraticLoss[1:70,])*100
colMeans(absoluteLoss[1:70,])*100

## best model is the average of the models - worst is the naive model 

## Check for significance difference between the best and the worst 

regressionDMW <- lm((quadraticLoss[1:70,5]-quadraticLoss[1:70,6]) ~ 1)
summary(regressionDMW)

## t-value corrected for autocorrelation 

tHAC <- regressionDMW$coefficients/(NeweyWest(regressionDMW,lag = 12))^0.5
print(tHAC)

## The difference is significant with the corrected t-value 

# Density forecast multivariate 

modelretailsales <- forecast::auto.arima(ts_ts(data$df_Retail_sales),
                                         ic="bic",
                                         xreg = cbind(pandemicDummies["2000-02-01/2024-03-01"], 
                                                      data$df_Retail_EU
                                         ),
                                         max.d = 0, 
                                         trace = TRUE,
                                         stepwise = FALSE)

modelretaileu <- forecast::auto.arima(ts_ts(data$df_Retail_EU), 
                                      xreg = pandemicDummies["2000-02-01/2024-03-01"], 
                                      ic = "bic",
                                      trace = TRUE,
                                      stepwise = FALSE
)

## Random draw normal distribution of error term 

tHorizons <- 12
nDraws <- 100
forecastRetailMatrix <- rbind(kronecker(matrix(1,1,nDraws),data.matrix(data$Retail_sales_cycle)),matrix(NA,tHorizons,nDraws))
forecastEUMatrix <- rbind(kronecker(matrix(1,1,nDraws),data.matrix(data$Retail_EU_cycle)),matrix(NA,tHorizons,nDraws))

pandemicDummiesMatrix <- kronecker(matrix(1,1),pandemicDummies)

T <- length(data$Retail_sales_cycle)
for (tauHorizon in 1:tHorizons) {
  
  for (iDraw in 1:nDraws) {
    
    modelEU_temp <- Arima(forecastEUMatrix[1:T+tauHorizon-1,iDraw],model = modelretaileu, xreg = pandemicDummiesMatrix[1:T+tauHorizon-1,]) 
    
    meanForecastEU <- forecast::forecast(modelEU_temp,h=1,xreg = matrix(pandemicDummiesMatrix[(T+tauHorizon),],1,24))$mean 
    
    forecastEUMatrix[T+tauHorizon,iDraw] <- meanForecastEU + rnorm(1, mean=0, sd=modelretaileu$sigma2^0.5)
    
    
    modelRetail_temp <- forecast::Arima(forecastRetailMatrix[1:T+tauHorizon-1,iDraw],model = modelretailsales, xreg = cbind(forecastEUMatrix[1:T+tauHorizon-1,iDraw],pandemicDummiesMatrix[1:T+tauHorizon-1,]))
    
    meanForecastRetail <- forecast::forecast(modelRetail_temp,h=1,xreg = cbind(forecastEUMatrix[T+tauHorizon,iDraw],matrix(pandemicDummiesMatrix[(T+tauHorizon),],1,24)))$mean
    forecastRetailMatrix[T+tauHorizon,iDraw] <- meanForecastRetail + rnorm(1, mean=0, sd=modelretailsales$sigma2^0.5)
    
  }
} 

plot(as.ts(data.matrix(data$Retail_sales_cycle)), type = "l", lwd = 2, las = 1, ylab = "",xlim = c(1,T+tHorizons))
fan(data = t(forecastRetailMatrix[(T+1):(T+tHorizons),]), type = "interval", start = (T+1))

## Bootstrapping 

forecastRetailMatrixboot <- rbind(kronecker(matrix(1,1,nDraws),data.matrix(data$Retail_sales_cycle)),matrix(NA,tHorizons,nDraws))
forecastEUMatrixboot <- rbind(kronecker(matrix(1,1,nDraws),data.matrix(data$Retail_EU_cycle)),matrix(NA,tHorizons,nDraws))

modelretaileu$residuals <- modelretaileu$residuals - mean(modelretaileu$residuals)
modelretailsales$residuals <- modelretailsales$residuals - mean(modelretailsales$residuals)

T <- length(data$Retail_sales_cycle)
for (tauHorizon in 1:tHorizons) {
  
  for (iDraw in 1:nDraws) {
    
    modelEU_temp <- Arima(forecastEUMatrix[1:T+tauHorizon-1,iDraw],model = modelretaileu, xreg = pandemicDummiesMatrix[1:T+tauHorizon-1,]) 
    
    meanForecastEU <- forecast::forecast(modelEU_temp,h=1,xreg = matrix(pandemicDummiesMatrix[(T+tauHorizon),],1,24))$mean
    
    forecastEUMatrixboot[T+tauHorizon,iDraw] <- meanForecastEU + modelretaileu$residuals[ceiling(runif(1,0,length(modelretaileu$residuals)))]
    
    
    modelRetail_temp <- forecast::Arima(forecastRetailMatrixboot[1:T+tauHorizon-1,iDraw],model = modelretailsales,xreg =     cbind(forecastEUMatrixboot[1:T+tauHorizon-1,iDraw],pandemicDummiesMatrix[1:T+tauHorizon-1,]))
    
    meanForecastRetail <- forecast::forecast(modelRetail_temp,h=1,xreg = cbind(forecastEUMatrixboot[T+tauHorizon,iDraw],matrix(pandemicDummiesMatrix[(T+tauHorizon),],1,24)))$mean
    forecastRetailMatrixboot[T+tauHorizon,iDraw] <- meanForecastRetail + modelretailsales$residuals[ceiling(runif(1,0,length(modelretailsales$residuals)))]
  }
  
} 

plot(as.ts(data.matrix(data$Retail_sales_cycle)), type = "l", lwd = 2, las = 1, ylab = "",xlim = c(1,T+tHorizons))
fan(data = t(forecastRetailMatrixboot[(T+1):(T+tHorizons),]), type = "interval", start = T+1)


