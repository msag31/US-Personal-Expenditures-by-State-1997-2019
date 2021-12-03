library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
NHDG.ts <- ts(df$New.Hampshire.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
NHDG.ts
min(NHDG.ts)
max(NHDG.ts)

NHDG.auto.arima<-auto.arima(NHDG.ts)
summary(NHDG.auto.arima)
NHDG.auto.arima.pred <- forecast(NHDG.auto.arima, h = 5, level = 0)
NHDG.auto.arima.pred

plot(NHDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3300,10000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New Hampshire Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NHDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NHDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(NHDG.auto.arima.pred$fitted,NHDG.ts), 3)

legend(1997,9000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 9000))
lines(c(2019.5,2019.5), c(0,9000))
text(2008.25, 9500, "Data Set")
text(2022, 9500, "Future")
arrows(1997.5, 9000, 2019.25, 9000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 9000, 2024, 9000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
NHNDG.ts <- ts(df$New.Hampshire.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NHNDG.ts)
max(NHNDG.ts)

NHNDG.auto.arima<-auto.arima(NHNDG.ts)
summary(NHNDG.auto.arima)
NHNDG.auto.arima.pred <- forecast(NHNDG.auto.arima, h = 5, level = 0)
NHNDG.auto.arima.pred

plot(NHNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(7100,19000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New Hampshire Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NHNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NHNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NHNDG.auto.arima.pred$fitted,NHNDG.ts), 3)

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
NHS.ts <- ts(df$New.Hampshire.Services, start = c(1997), end=c(2019), freq = 1)
min(NHS.ts)
max(NHS.ts)

NHS.auto.arima<-auto.arima(NHS.ts)
summary(NHS.auto.arima)
NHS.auto.arima.pred <- forecast(NHS.auto.arima, h = 5, level = 0)
NHS.auto.arima.pred

plot(NHS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(17000,65000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New Hampshire Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NHS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NHS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NHS.auto.arima.pred$fitted, NHS.ts), 3)

legend(1997,61500, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 61500))
lines(c(2019.5,2019.5), c(0,61500))
text(2008.25, 64000, "Data Set")
text(2022, 64000, "Future")
arrows(1997.5, 61500, 2019.25, 61500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 61500, 2024, 61500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
