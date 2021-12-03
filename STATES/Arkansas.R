library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
ARDG.ts <- ts(df$Arkansas.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
ARDG.ts
max(ARDG.ts)
min(ARDG.ts)

plot(ARDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(5500,14000), main = "Value")

ARDG.auto.arima<-auto.arima(ARDG.ts)
summary(ARDG.auto.arima)
ARDG.auto.arima.pred <- forecast(ARDG.auto.arima, h = 5, level = 0)
ARDG.auto.arima.pred

plot(ARDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(5500,15000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Arkansas Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ARDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ARDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(ARDG.auto.arima.pred$fitted, ARDG.ts), 3)

legend(1997,14000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 14000))
lines(c(2019.5,2019.5), c(0,14000))
text(2008.25, 14500, "Data Set")
text(2022, 14500, "Future")
arrows(1997.5, 14000, 2019.25, 14000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 14000, 2024, 14000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
ARNDG.ts <- ts(df$Arkansas.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
max(ARNDG.ts)
min(ARNDG.ts)

plot(ARNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(10000,26000), main = "Value")

ARNDG.auto.arima<-auto.arima(ARNDG.ts)
summary(ARNDG.auto.arima)
ARNDG.auto.arima.pred <- forecast(ARNDG.auto.arima, h = 5, level = 0)
ARNDG.auto.arima.pred

plot(ARNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(10000,31000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Arkansas Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ARNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ARNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(ARNDG.auto.arima.pred$fitted, ARNDG.ts), 3)

legend(1997,29000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 29000))
lines(c(2019.5,2019.5), c(0,29000))
text(2008.25, 30000, "Data Set")
text(2022, 30000, "Future")
arrows(1997.5, 29000, 2019.25, 29000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 29000, 2024, 29000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
ARS.ts <- ts(df$Arkansas.Services, start = c(1997), end=c(2019), freq = 1)
max(ARS.ts)
min(ARS.ts)

plot(ARS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(22000,70000), main = "Value")

ARS.auto.arima<-auto.arima(ARS.ts)
summary(ARS.auto.arima)
ARS.auto.arima.pred <- forecast(ARS.auto.arima, h = 5, level = 0)
ARS.auto.arima.pred

plot(ARS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(22000,84000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Arkansas Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ARS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ARS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(ARS.auto.arima.pred$fitted, ARS.ts), 3)

legend(1997,82000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 82000))
lines(c(2019.5,2019.5), c(0,82000))
text(2008.25, 84000, "Data Set")
text(2022, 84000, "Future")
arrows(1997.5, 82000, 2019.25, 82000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 82000, 2024, 82000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
