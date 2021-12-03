library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
TNDG.ts <- ts(df$Tennessee.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
TNDG.ts
min(TNDG.ts)
max(TNDG.ts)

TNDG.auto.arima<-auto.arima(TNDG.ts)
summary(TNDG.auto.arima)
TNDG.auto.arima.pred <- forecast(TNDG.auto.arima, h = 5, level = 0)
TNDG.auto.arima.pred

plot(TNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(15000,36000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Tennessee Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(TNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(TNDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(TNDG.auto.arima.pred$fitted,TNDG.ts), 3)

legend(1997,34000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 34000))
lines(c(2019.5,2019.5), c(0,34000))
text(2008.25, 35500, "Data Set")
text(2022, 35500, "Future")
arrows(1997.5, 34000, 2019.25, 34000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 34000, 2024, 34000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
TNNDG.ts <- ts(df$Tennessee.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(TNNDG.ts)
max(TNNDG.ts)

TNNDG.auto.arima<-auto.arima(TNNDG.ts)
summary(TNNDG.auto.arima)
TNNDG.auto.arima.pred <- forecast(TNNDG.auto.arima, h = 5, level = 0)
TNNDG.auto.arima.pred

plot(TNNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(26000,70000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Tennessee Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(TNNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(TNNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(TNNDG.auto.arima.pred$fitted,TNNDG.ts), 3)

legend(1997,67000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 67000))
lines(c(2019.5,2019.5), c(0,67000))
text(2008.25, 69000, "Data Set")
text(2022, 69000, "Future")
arrows(1997.5, 67000, 2019.25, 67000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 67000, 2024, 67000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
TNS.ts <- ts(df$Tennessee.Services, start = c(1997), end=c(2019), freq = 1)
min(TNS.ts)
max(TNS.ts)

TNS.auto.arima<-auto.arima(TNS.ts)
summary(TNS.auto.arima)
TNS.auto.arima.pred <- forecast(TNS.auto.arima, h = 5, level = 0)
TNS.auto.arima.pred

plot(TNS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(60000,215000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Tennessee Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(TNS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(TNS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(TNS.auto.arima.pred$fitted, TNS.ts), 3)

legend(1997,205000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 205000))
lines(c(2019.5,2019.5), c(0,205000))
text(2008.25, 213000, "Data Set")
text(2022, 213000, "Future")
arrows(1997.5, 205000, 2019.25, 205000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 205000, 2024, 205000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
