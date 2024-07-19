#Step1: Install packages and import the dataset

install.packages("tseries")
install.packages("forecast")

library(tseries) 
library(forecast) 

bank_case <- as.ts(scan("C:\\Users\\hakhamanesh\\Downloads\\bank_case.txt"))
print(bank_case)

#Step 2: Model Estimation

par(mfrow=c(1,3))
plot(bank_case, main = "Time Series of Loans")
acf(bank_case, main = "ACF of Loans", lag.max = 10, ylim = c(-1,1))
pacf(bank_case, main = "PACF of Loans", lag.max = 10, ylim = c(-1,1))

#Step 3: Test for Stationarity

adf.test(bank_case)
bank_case_d1 <- diff(bank_case)
adf.test(bank_case_d1)
bank_case_d2 <- diff(bank_case_d1)
adf.test(bank_case_d2)
par(mfrow=c(1,3))
plot(bank_case_d2, main = "SOD Time Series of Loans")
acf(bank_case_d2, main = "ACF of SOD Loans", lag.max = 10, ylim = c(-1,1))
pacf(bank_case_d2, main = "PACF of SOD Loans", lag.max = 10, ylim=c(-1,1))

#Step 4: Parameter Estimation

bank_fit <- arima(x=bank_case, order = c(0,2,1))
bank_fit

#Step 5: Fitted Values

fitted(bank_fit)

#Step 6: Residual Analysis

par(mfrow = c(1,3))
plot(bank_fit$residuals, ylab = "Residuals")
acf(bank_fit$residuals, ylim = c(-1,1))
pacf(bank_fit$residuals, ylim = c(-1,1))

checkresiduals(bank_fit)

#Step 7: Forecasting

bank_pred <- forecast(bank_fit, h=24)
bank_pred
par(mfrow = c(1,1))
plot(bank_pred)

