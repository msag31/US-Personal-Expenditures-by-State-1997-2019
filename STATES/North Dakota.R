library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
NDDG.ts <- ts(df$North.Dakota.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NDDG.ts)
max(NDDG.ts)

NDDG.auto.arima<-auto.arima(NDDG.ts)
summary(NDDG.auto.arima)
NDDG.auto.arima.pred <- forecast(NDDG.auto.arima, h = 5, level = 0)
NDDG.auto.arima.pred

plot(NDDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(1900,5700), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "North Dakota Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NDDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NDDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(NDDG.auto.arima.pred$fitted,NDDG.ts), 3)

legend(1997,5500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 5500))
lines(c(2019.5,2019.5), c(0,5500))
text(2008.25, 5600, "Data Set")
text(2022, 5600, "Future")
arrows(1997.5, 5500, 2019.25, 5500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 5500, 2024, 5500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
NDNDG.ts <- ts(df$North.Dakota.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
NDNDG.ts
min(NDNDG.ts)
max(NDNDG.ts)

NDNDG.auto.arima<-auto.arima(NDNDG.ts)
summary(NDNDG.auto.arima)
NDNDG.auto.arima.pred <- forecast(NDNDG.auto.arima, h = 5, level = 0)
NDNDG.auto.arima.pred

plot(NDNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3000,9000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "North Dakota Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NDNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NDNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NDNDG.auto.arima.pred$fitted,NDNDG.ts), 3)

legend(1997,8800, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 8800))
lines(c(2019.5,2019.5), c(0,8800))
text(2008.25, 8950, "Data Set")
text(2022, 8950, "Future")
arrows(1997.5, 8800, 2019.25, 8800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 8800, 2024, 8800, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
NDS.ts <- ts(df$North.Dakota.Services, start = c(1997), end=c(2019), freq = 1)
min(NDS.ts)
max(NDS.ts)

NDS.auto.arima<-auto.arima(NDS.ts)
summary(NDS.auto.arima)
NDS.auto.arima.pred <- forecast(NDS.auto.arima, h = 5, level = 0)
NDS.auto.arima.pred

plot(NDS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(7000,33000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "North Dakota Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NDS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NDS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NDS.auto.arima.pred$fitted, NDS.ts), 3)

legend(1997,31000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 31000))
lines(c(2019.5,2019.5), c(0,31000))
text(2008.25, 32500, "Data Set")
text(2022, 32500, "Future")
arrows(1997.5, 31000, 2019.25, 31000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 31000, 2024, 31000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
