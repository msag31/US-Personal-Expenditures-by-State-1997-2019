library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
USDG.ts <- ts(df$United.States.Durable.Goods, start = c(1997,1), end=c(2019,1), freq = 1)
USDG.ts
max(USDG.ts)
min(USDG.ts)

plot(USDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(700000,3200000), main = "Value")

nTrain <- length(USDG.ts)-5
nValid <- length(USDG.ts)-nTrain
nValid
nTrain
train.ts <- window(USDG.ts, start = c(1997, 1), end = c(1997, nTrain))
valid.ts <- window(USDG.ts, start = c(1997, nTrain), 
                   end = c(2005, nTrain + nValid))
tail(valid.ts)
tail(train.ts)

#Regression mode with quadratic trend
train.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(train.quad)
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

plot(train.quad.pred$mean, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(700000,1700000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Quadtratic Trend for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)) )
lines(train.quad.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

round(accuracy(train.quad.pred$mean, valid.ts), 3)

legend(1997,3200000, legend = c("Time Series", 
                               "Forecast for Training Period",
                               "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 3200000))
lines(c(2014.5, 2014.5), c(0, 3200000))
lines(c(2019.25,2019.25), c(0,3200000))
text(2005.25, 1650000, "Training")
text(2017, 1650000, "Validation")
text(2022, 1650000, "Future")
arrows(1997.5, 3200000, 2014.25, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 3200000, 2019, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.5, 3200000, 2024, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Regression model with linear trend
train.lin <- tslm(train.ts ~ trend)
summary(train.lin)
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

plot(train.lin.pred$mean, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(700000,1650000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Linear Trend for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

legend(1997,3200000, legend = c("Time Series", 
                                "Forecast for Training Period",
                                "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 3200000))
lines(c(2014.5, 2014.5), c(0, 3200000))
lines(c(2019.25,2019.25), c(0,3200000))
text(2005.25, 1650000, "Training")
text(2017, 1650000, "Validation")
text(2022, 1650000, "Future")
arrows(1997.5, 3200000, 2014.25, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 3200000, 2019, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.5, 3200000, 2024, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(train.lin.pred$mean, valid.ts), 3)

#holts model
train.ZZZ <- ets(train.ts, model = "ZZZ")
train.ZZZ
train.ZZZ.pred <- forecast(train.ZZZ, h = nValid, level = 0)
train.ZZZ.pred


plot(train.ZZZ.pred$mean, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(700000,1650000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Holt's Model for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

legend(1997,3200000, legend = c("Time Series", 
                                "Forecast for Training Period",
                                "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 3200000))
lines(c(2014.5, 2014.5), c(0, 3200000))
lines(c(2019.25,2019.25), c(0,3200000))
text(2005.25, 1650000, "Training")
text(2017, 1650000, "Validation")
text(2022, 1650000, "Future")
arrows(1997.5, 3200000, 2014.25, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 3200000, 2019, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020, 3200000, 2024, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#auto arima
train.auto.arima<-auto.arima(train.ts)
summary(train.auto.arima)
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Expenses (in Millions)", ylim = c(700000,1650000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(1992, 2025, 1), labels = format(seq(1992, 2025, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)

legend(1997,3200000, legend = c("Time Series", 
                                "Forecast for Training Period",
                                "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 3200000))
lines(c(2014.5, 2014.5), c(0, 3200000))
lines(c(2019.25,2019.25), c(0,3200000))
text(2005.25, 1650000, "Training")
text(2017, 1650000, "Validation")
text(2022, 1650000, "Future")
arrows(1997.5, 3200000, 2014.25, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 3200000, 2019, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020, 3200000, 2024, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Regression mode with quadratic trend
round(accuracy(train.quad.pred$mean, valid.ts), 3)

#Regression model with linear trend
round(accuracy(train.lin.pred$mean, valid.ts), 3)

#holts model
round(accuracy(train.ZZZ.pred$mean, valid.ts), 3)
######################
#auto arima!!!!!!!!
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)



#Non-durable Goods
setwd("~//Desktop/CSUEB/Capstone")
USNDG.ts <- ts(df$United.States.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
USNDG.ts
min(USNDG.ts)
max(USNDG.ts)

plot(USNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(1200000,3000000), main = "Value")

nTrain <- length(USNDG.ts)-5
nValid <- length(USNDG.ts)-nTrain
nValid
nTrain
train.ts <- window(USNDG.ts, start = c(1997, 1), end = c(1997, nTrain))
valid.ts <- window(USNDG.ts, start = c(1997, nTrain), 
                   end = c(2005, nTrain + nValid))
tail(valid.ts)
tail(train.ts)

#Regression mode with quadratic trend
train.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(train.quad)
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

plot(train.quad.pred$mean, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(1200000,3500000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Quadtratic Trend for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)) )
lines(train.quad.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

legend(1997,3100000, legend = c("Time Series", 
                                "Forecast for Training Period",
                                "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 3200000))
lines(c(2014.5, 2014.5), c(0, 3200000))
lines(c(2019.25,2019.25), c(0,3200000))
text(2005.25, 3300000, "Training")
text(2017, 3300000, "Validation")
text(2022, 3300000, "Future")
arrows(1997.5, 3200000, 2014.25, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 3200000, 2019, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.5, 3200000, 2024, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(train.quad.pred$mean, valid.ts), 3)

#Regression model with linear trend
train.lin <- tslm(train.ts ~ trend)
summary(train.lin)
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)

plot(train.lin.pred$mean, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(1200000,3500000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Linear Trend for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

round(accuracy(train.lin.pred$mean, valid.ts), 3)

legend(1997,3100000, legend = c("Time Series", 
                                "Forecast for Training Period",
                                "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 3200000))
lines(c(2014.5, 2014.5), c(0, 3200000))
lines(c(2019.25,2019.25), c(0,3200000))
text(2005.25, 3300000, "Training")
text(2017, 3300000, "Validation")
text(2022, 3300000, "Future")
arrows(1997.5, 3200000, 2014.25, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 3200000, 2019, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.5, 3200000, 2024, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#holts model
train.ZZZ <- ets(train.ts, model = "ZZZ")
train.ZZZ
train.ZZZ.pred <- forecast(train.ZZZ, h = nValid, level = 0)
train.ZZZ.pred


plot(train.ZZZ.pred$mean, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(1200000,3500000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Holt's Model for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)) )
lines(train.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

legend(1997,3100000, legend = c("Time Series", 
                                "Forecast for Training Period",
                                "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 3200000))
lines(c(2014.5, 2014.5), c(0, 3200000))
lines(c(2019.25,2019.25), c(0,3200000))
text(2005.25, 3300000, "Training")
text(2017, 3300000, "Validation")
text(2022, 3300000, "Future")
arrows(1997.5, 3200000, 2014.25, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 3200000, 2019, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.5, 3200000, 2024, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#auto arima
train.auto.arima<-auto.arima(train.ts)
summary(train.auto.arima)
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Expenses (in Millions)", ylim = c(1200000,3500000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(1992, 2025, 1), labels = format(seq(1992, 2025, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)

legend(1997,3100000, legend = c("Time Series", 
                                "Forecast for Training Period",
                                "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 3200000))
lines(c(2014.5, 2014.5), c(0, 3200000))
lines(c(2019.25,2019.25), c(0,3200000))
text(2005.25, 3300000, "Training")
text(2017, 3300000, "Validation")
text(2022, 3300000, "Future")
arrows(1997.5, 3200000, 2014.25, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 3200000, 2019, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.5, 3200000, 2024, 3200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
############
#Regression mode with quadratic trend!!!!!!!!!!!
round(accuracy(train.quad.pred$mean, valid.ts), 3)

#Regression model with linear trend
round(accuracy(train.lin.pred$mean, valid.ts), 3)

#holts model
round(accuracy(train.ZZZ.pred$mean, valid.ts), 3)

#auto arima
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)


#Services
USS.ts <- ts(df$United.States.Services, start = c(1997,1), end=c(2019,1), freq = 1)
USS.ts
min(USS.ts)
max(USS.ts)

plot(USS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(3500000,11000000), main = "Value")

nTrain <- length(USS.ts)-5
nValid <- length(USS.ts)-nTrain
nValid
nTrain
train.ts <- window(USS.ts, start = c(1997, 1), end = c(1997, nTrain))
valid.ts <- window(USS.ts, start = c(1997, nTrain), 
                   end = c(2005, nTrain + nValid))
tail(valid.ts)
tail(train.ts)

#Regression mode with quadratic trend
train.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(train.quad)
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

plot(train.quad.pred$mean, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3500000,13000000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Quadtratic Trend for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)) )
lines(train.quad.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

legend(1997,12000000, legend = c("Time Series", 
                                "Forecast for Training Period",
                                "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 12000000))
lines(c(2014.5, 2014.5), c(0, 12000000))
lines(c(2019.25,2019.25), c(0,12000000))
text(2005.25, 12500000, "Training")
text(2017, 12500000, "Validation")
text(2022, 12500000, "Future")
arrows(1997.5, 12000000, 2014.25, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 12000000, 2019, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.5, 12000000, 2024, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(train.quad.pred$mean, valid.ts), 3)

#Regression model with linear trend
train.lin <- tslm(train.ts ~ trend)
summary(train.lin)
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)

plot(train.lin.pred$mean, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3500000,13000000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Linear Trend for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

round(accuracy(train.lin.pred$mean, valid.ts), 3)

legend(1997,12000000, legend = c("Time Series", 
                                 "Forecast for Training Period",
                                 "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 12000000))
lines(c(2014.5, 2014.5), c(0, 12000000))
lines(c(2019.25,2019.25), c(0,12000000))
text(2005.25, 12500000, "Training")
text(2017, 12500000, "Validation")
text(2022, 12500000, "Future")
arrows(1997.5, 12000000, 2014.25, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 12000000, 2019, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.5, 12000000, 2024, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#holts model
train.ZZZ <- ets(train.ts, model = "ZZZ")
train.ZZZ
train.ZZZ.pred <- forecast(train.ZZZ, h = nValid, level = 0)
train.ZZZ.pred


plot(train.ZZZ.pred$mean, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3500000,13000000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Holt's Model for Training and Validation Data", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)) )
lines(train.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

legend(1997,12000000, legend = c("Time Series", 
                                 "Forecast for Training Period",
                                 "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 12000000))
lines(c(2014.5, 2014.5), c(0, 12000000))
lines(c(2019.25,2019.25), c(0,12000000))
text(2005.25, 12500000, "Training")
text(2017, 12500000, "Validation")
text(2022, 12500000, "Future")
arrows(1997.5, 12000000, 2014.25, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 12000000, 2019, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.5, 12000000, 2024, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#auto arima
train.auto.arima<-auto.arima(train.ts)
summary(train.auto.arima)
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Expenses (in Millions)", ylim = c(3500000,13000000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)

legend(1997,12000000, legend = c("Time Series", 
                                 "Forecast for Training Period",
                                 "Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 12000000))
lines(c(2014.5, 2014.5), c(0, 12000000))
lines(c(2019.25,2019.25), c(0,12000000))
text(2005.25, 12500000, "Training")
text(2017, 12500000, "Validation")
text(2022, 12500000, "Future")
arrows(1997.5, 12000000, 2014.25, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.75, 12000000, 2019, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.5, 12000000, 2024, 12000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#Regression mode with quadratic trend
round(accuracy(train.quad.pred$mean, valid.ts), 3)

#Regression model with linear trend
round(accuracy(train.lin.pred$mean, valid.ts), 3)
###########
#holts model!!!!!!!!!
round(accuracy(train.ZZZ.pred$mean, valid.ts), 3)

#auto arima
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)
