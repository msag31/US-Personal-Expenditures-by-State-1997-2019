library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
DCDG.ts <- ts(df$District.of.Columbia.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
DCDG.ts
min(DCDG.ts)
max(DCDG.ts)

plot(DCDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(1600,3700), main = "Value")

DCDG.auto.arima<-auto.arima(DCDG.ts)
summary(DCDG.auto.arima)
DCDG.auto.arima.pred <- forecast(DCDG.auto.arima, h = 5, level = 0)
DCDG.auto.arima.pred

plot(DCDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(1600,4400), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "District of Columbia Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(DCDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(DCDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(DCDG.auto.arima.pred$fitted, DCDG.ts), 3)

legend(1997,4250, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 4250))
lines(c(2019.5,2019.5), c(0,4250))
text(2008.25, 4320, "Data Set")
text(2022, 4320, "Future")
arrows(1997.5, 4250, 2019.25, 4250, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 4250, 2024, 4250, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
DCNDG.ts <- ts(df$District.of.Columbia.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(DCNDG.ts)
max(DCNDG.ts)

plot(DCNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(3300,7200), main = "Value")

DCNDG.auto.arima<-auto.arima(DCNDG.ts)
summary(DCNDG.auto.arima)
DCNDG.auto.arima.pred <- forecast(DCNDG.auto.arima, h = 5, level = 0)
DCNDG.auto.arima.pred

plot(DCNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3300,8200), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "District of Columbia Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(DCNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(DCNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(DCNDG.auto.arima.pred$fitted, DCNDG.ts), 3)

legend(1997,8050, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 8050))
lines(c(2019.5,2019.5), c(0,8050))
text(2008.25, 8200, "Data Set")
text(2022, 8200, "Future")
arrows(1997.5, 8050, 2019.25, 8050, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 8050, 2024, 8050, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
DCS.ts <- ts(df$District.of.Columbia.Services, start = c(1997), end=c(2019), freq = 1)
min(DCS.ts)
max(DCS.ts)

plot(DCS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(12000,37000), main = "Value")

DCS.auto.arima<-auto.arima(DCS.ts)
summary(DCS.auto.arima)
DCS.auto.arima.pred <- forecast(DCS.auto.arima, h = 5, level = 0)
DCS.auto.arima.pred

plot(DCS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(12000,44000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "District of Columbia Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(DCS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(DCS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(DCS.auto.arima.pred$fitted, DCS.ts), 3)

legend(1997,42000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 42000))
lines(c(2019.5,2019.5), c(0,42000))
text(2008.25, 43000, "Data Set")
text(2022, 43000, "Future")
arrows(1997.5, 42000, 2019.25, 42000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 42000, 2024, 42000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
