library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
ALDG.ts <- ts(df$Alabama.Durable.Goods, start = c(1997,1), end=c(2019,1), freq = 1)
ALDG.ts
max(ALDG.ts)
min(ALDG.ts)

plot(ALDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(10000,21000), main = "Value")
 
ALDG.auto.arima<-auto.arima(ALDG.ts)
summary(ALDG.auto.arima)
ALDG.auto.arima.pred <- forecast(ALDG.auto.arima, h = 5, level = 0)
ALDG.auto.arima.pred

plot(ALDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(10000,27000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Alabama Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ALDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ALDG.ts, col = "black", lwd = 2, lty = 2)
round(accuracy(ALDG.auto.arima.pred$fitted, ALDG.ts), 3)

legend(1997,25000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 26000))
lines(c(2019.25,2019.25), c(0,26000))
text(2008.25, 27000, "Data Set")
text(2022, 27000, "Future")
arrows(1997.5, 26000, 2019.25, 26000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 26000, 2024, 26000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
ALNDG.ts <- ts(df$Alabama.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
max(ALNDG.ts)
min(ALNDG.ts)

plot(ALNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(20000,45000), main = "Value")

ALNDG.auto.arima<-auto.arima(ALNDG.ts)
summary(ALNDG.auto.arima)
ALNDG.auto.arima.pred <- forecast(ALNDG.auto.arima, h = 5, level = 0)
ALNDG.auto.arima.pred

plot(ALNDG.auto.arima.pred,    
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(20000,52000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Alabama Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ALNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ALNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(ALNDG.auto.arima.pred$fitted, ALNDG.ts), 3)

legend(1997,50000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 51000))
lines(c(2019.5,2019.5), c(0,51000))
text(2008.25, 52000, "Data Set")
text(2022, 52000, "Future")
arrows(1997.5, 51000, 2019.25, 51000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 51000, 2024, 51000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Services
ALS.ts <- ts(df$Alabama.Services, start = c(1997,1), end=c(2019,1), freq = 1)
max(ALS.ts)
min(ALS.ts)

plot(ALS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(40000,115000), main = "Value")

ALS.auto.arima<-auto.arima(ALS.ts)
summary(ALS.auto.arima)
ALS.auto.arima.pred <- forecast(ALS.auto.arima, h = 5, level = 0)
ALS.auto.arima.pred

plot(ALS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(40000,150000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Alabama Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ALS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ALS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(ALS.auto.arima.pred$fitted, ALS.ts), 3)

legend(1997,130000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 140000))
lines(c(2019.5,2019.5), c(0,140000))
text(2008.25, 145000, "Data Set")
text(2022, 145000, "Future")
arrows(1997.5, 140000, 2019.25, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 140000, 2024, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
