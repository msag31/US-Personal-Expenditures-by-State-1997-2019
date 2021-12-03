library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
CTDG.ts <- ts(df$Connecticut.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
CTDG.ts
min(CTDG.ts)
max(CTDG.ts)

plot(CTDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(9000,18000), main = "Value")

CTDG.auto.arima<-auto.arima(CTDG.ts)
summary(CTDG.auto.arima)
CTDG.auto.arima.pred <- forecast(CTDG.auto.arima, h = 5, level = 0)
CTDG.auto.arima.pred

plot(CTDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(9000,21000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Connecticut Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(CTDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(CTDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(CTDG.auto.arima.pred$fitted, CTDG.ts), 3)

legend(1997,20000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 20000))
lines(c(2019.5,2019.5), c(0,20000))
text(2008.25, 20500, "Data Set")
text(2022, 20500, "Future")
arrows(1997.5, 20000, 2019.25, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 20000, 2024, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
CTNDG.ts <- ts(df$Connecticut.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(CTNDG.ts)
max(CTNDG.ts)

plot(CTNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(19000,38000), main = "Value")

CTNDG.auto.arima<-auto.arima(CTNDG.ts)
summary(CTNDG.auto.arima)
CTNDG.auto.arima.pred <- forecast(CTNDG.auto.arima, h = 5, level = 0)
CTNDG.auto.arima.pred

plot(CTNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(19000,44000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Connecticut Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(CTNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(CTNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(CTNDG.auto.arima.pred$fitted, CTNDG.ts), 3)

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

#Services
CTS.ts <- ts(df$Connecticut.Services, start = c(1997), end=c(2019), freq = 1)
min(CTS.ts)
max(CTS.ts)

plot(CTS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(56000,145000), main = "Value")

CTS.auto.arima<-auto.arima(CTS.ts)
summary(CTS.auto.arima)
CTS.auto.arima.pred <- forecast(CTS.auto.arima, h = 5, level = 0)
CTS.auto.arima.pred

plot(CTS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(56000,175000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Connecticut Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(CTS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(CTS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(CTS.auto.arima.pred$fitted, CTS.ts), 3)

legend(1997,165000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 165000))
lines(c(2019.5,2019.5), c(0,165000))
text(2008.25, 170000, "Data Set")
text(2022, 170000, "Future")
arrows(1997.5, 165000, 2019.25, 165000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 165000, 2024, 165000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
