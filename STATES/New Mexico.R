library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
NMDG.ts <- ts(df$New.Mexico.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NMDG.ts)
max(NMDG.ts)

NMDG.auto.arima<-auto.arima(NMDG.ts)
summary(NMDG.auto.arima)
NMDG.auto.arima.pred <- forecast(NMDG.auto.arima, h = 5, level = 0)
NMDG.auto.arima.pred

plot(NMDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(4400,10500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New Mexico Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NMDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NMDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(NMDG.auto.arima.pred$fitted,NMDG.ts), 3)

legend(1997,10000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 10000))
lines(c(2019.5,2019.5), c(0,10000))
text(2008.25, 10500, "Data Set")
text(2022, 10500, "Future")
arrows(1997.5, 10000, 2019.25, 10000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 10000, 2024, 10000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
NMNDG.ts <- ts(df$New.Mexico.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NMNDG.ts)
max(NMNDG.ts)

NMNDG.auto.arima<-auto.arima(NMNDG.ts)
summary(NMNDG.auto.arima)
NMNDG.auto.arima.pred <- forecast(NMNDG.auto.arima, h = 5, level = 0)
NMNDG.auto.arima.pred

plot(NMNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(7300,21000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New Mexico Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NMNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NMNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NMNDG.auto.arima.pred$fitted,NMNDG.ts), 3)

legend(1997,19000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 19000))
lines(c(2019.5,2019.5), c(0,19000))
text(2008.25, 20000, "Data Set")
text(2022, 20000, "Future")
arrows(1997.5, 19000, 2019.25, 19000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 19000, 2024, 19000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
NMS.ts <- ts(df$New.Mexico.Services, start = c(1997), end=c(2019), freq = 1)
min(NMS.ts)
max(NMS.ts)

NMS.auto.arima<-auto.arima(NMS.ts)
summary(NMS.auto.arima)
NMS.auto.arima.pred <- forecast(NMS.auto.arima, h = 5, level = 0)
NMS.auto.arima.pred

plot(NMS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(18500,65000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New Mexico Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NMS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NMS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NMS.auto.arima.pred$fitted, NMS.ts), 3)

legend(1997,62000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 62000))
lines(c(2019.5,2019.5), c(0,62000))
text(2008.25, 64000, "Data Set")
text(2022, 64000, "Future")
arrows(1997.5, 62000, 2019.25, 62000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 62000, 2024, 62000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
