library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
MEDG.ts <- ts(df$Maine.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
MEDG.ts
min(MEDG.ts)
max(MEDG.ts)

plot(LADG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(3100,6800), main = "Value")

MEDG.auto.arima<-auto.arima(MEDG.ts)
summary(MEDG.auto.arima)
MEDG.auto.arima.pred <- forecast(MEDG.auto.arima, h = 5, level = 0)
MEDG.auto.arima.pred

plot(MEDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3100,8100), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Maine Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MEDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MEDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(MEDG.auto.arima.pred$fitted,MEDG.ts), 3)

legend(1997,8000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 8000))
lines(c(2019.5,2019.5), c(0,8000))
text(2008.25, 8200, "Data Set")
text(2022, 8200, "Future")
arrows(1997.5, 8000, 2019.25, 8000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 8000, 2024, 8000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
MENDG.ts <- ts(df$Maine.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(MENDG.ts)
max(MENDG.ts)

plot(MENDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(6850,16000), main = "Value")

MENDG.auto.arima<-auto.arima(MENDG.ts)
summary(MENDG.auto.arima)
MENDG.auto.arima.pred <- forecast(MENDG.auto.arima, h = 5, level = 0)
MENDG.auto.arima.pred

plot(MENDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(6850,20000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Maine Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MENDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MENDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MENDG.auto.arima.pred$fitted,MENDG.ts), 3)

legend(1997,18000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 18000))
lines(c(2019.5,2019.5), c(0,18000))
text(2008.25, 18500, "Data Set")
text(2022, 18500, "Future")
arrows(1997.5, 18000, 2019.25, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 18000, 2024, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
MES.ts <- ts(df$Maine.Services, start = c(1997), end=c(2019), freq = 1)
min(MES.ts)
max(MES.ts)

plot(MES.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(15000,42000), main = "Value")

MES.auto.arima<-auto.arima(MES.ts)
summary(MES.auto.arima)
MES.auto.arima.pred <- forecast(MES.auto.arima, h = 5, level = 0)
MES.auto.arima.pred

plot(MES.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(15000,50000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Maine Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MES.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MES.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MES.auto.arima.pred$fitted, MES.ts), 3)

legend(1997,48500, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 48500))
lines(c(2019.5,2019.5), c(0,48500))
text(2008.25, 50000, "Data Set")
text(2022, 50000, "Future")
arrows(1997.5, 48500, 2019.25, 48500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 48500, 2024, 48500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
