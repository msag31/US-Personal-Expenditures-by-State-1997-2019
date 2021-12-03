library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
AZDG.ts <- ts(df$Arizona.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
AZDG.ts
max(AZDG.ts)
min(AZDG.ts)

plot(AZDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(11000,35000), main = "Value")

AZDG.auto.arima<-auto.arima(AZDG.ts)
summary(AZDG.auto.arima)
AZDG.auto.arima.pred <- forecast(AZDG.auto.arima, h = 5, level = 0)
AZDG.auto.arima.pred

plot(AZDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(11000,42000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Arizona Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(AZDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(AZDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(AZDG.auto.arima.pred$fitted, AZDG.ts), 3)

legend(1997,40000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 40000))
lines(c(2019.5,2019.5), c(0,40000))
text(2008.25, 41000, "Data Set")
text(2022, 41000, "Future")
arrows(1997.5, 40000, 2019.25, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 40000, 2024, 40000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
AZNDG.ts <- ts(df$Arizona.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
max(AZNDG.ts)
min(AZNDG.ts)

plot(AZNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(20000,61000), main = "Value")

AZNDG.auto.arima<-auto.arima(AZNDG.ts)
summary(AZNDG.auto.arima)
AZNDG.auto.arima.pred <- forecast(AZNDG.auto.arima, h = 5, level = 0)
AZNDG.auto.arima.pred

plot(AZNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(20000,72000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Arizona Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(AZNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(AZNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(AZNDG.auto.arima.pred$fitted, AZNDG.ts), 3)

legend(1997,70000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 70000))
lines(c(2019.5,2019.5), c(0,70000))
text(2008.25, 72000, "Data Set")
text(2022, 72000, "Future")
arrows(1997.5, 70000, 2019.25, 70000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 70000, 2024, 70000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
AZS.ts <- ts(df$Arizona.Services, start = c(1997,1), end=c(2019), freq = 1)
max(AZS.ts)
min(AZS.ts)

plot(AZS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(55000,190000), main = "Value")

AZS.auto.arima<-auto.arima(AZS.ts)
summary(AZS.auto.arima)
AZS.auto.arima.pred <- forecast(AZS.auto.arima, h = 5, level = 0)
AZS.auto.arima.pred

plot(AZS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(55000,250000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Arizona Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(AZS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(AZS.ts, col = "black", lwd = 2, lty = 2)

round(accuracy(AZS.auto.arima.pred$fitted, AZS.ts), 3)

legend(1997,230000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 230000))
lines(c(2019.5,2019.5), c(0,230000))
text(2008.25, 240000, "Data Set")
text(2022, 240000, "Future")
arrows(1997.5, 230000, 2019.25, 230000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 230000, 2024, 230000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
