library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
ORNG.ts <- ts(df$Oregon.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
ORNG.ts
min(ORNG.ts)
max(ORNG.ts)

ORNG.auto.arima<-auto.arima(ORNG.ts)
summary(ORNG.auto.arima)
ORNG.auto.arima.pred <- forecast(ORNG.auto.arima, h = 5, level = 0)
ORNG.auto.arima.pred

plot(ORNG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(9500,28000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Oregon Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ORNG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ORNG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(ORNG.auto.arima.pred$fitted,ORNG.ts), 3)

legend(1997,26000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 26000))
lines(c(2019.5,2019.5), c(0,26000))
text(2008.25, 27000, "Data Set")
text(2022, 27000, "Future")
arrows(1997.5, 26000, 2019.25, 26000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 26000, 2024, 26000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
ORNNG.ts <- ts(df$Oregon.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(ORNNG.ts)
max(ORNNG.ts)

ORNNG.auto.arima<-auto.arima(ORNNG.ts)
summary(ORNNG.auto.arima)
ORNNG.auto.arima.pred <- forecast(ORNNG.auto.arima, h = 5, level = 0)
ORNNG.auto.arima.pred

plot(ORNNG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(16000,45000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Oregon Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ORNNG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ORNNG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(ORNNG.auto.arima.pred$fitted,ORNNG.ts), 3)

legend(1997,42500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 42500))
lines(c(2019.5,2019.5), c(0,42500))
text(2008.25, 44000, "Data Set")
text(2022, 44000, "Future")
arrows(1997.5, 42500, 2019.25, 42500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 42500, 2024, 42500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
ORS.ts <- ts(df$Oregon.Services, start = c(1997), end=c(2019), freq = 1)
min(ORS.ts)
max(ORS.ts)

ORS.auto.arima<-auto.arima(ORS.ts)
summary(ORS.auto.arima)
ORS.auto.arima.pred <- forecast(ORS.auto.arima, h = 5, level = 0)
ORS.auto.arima.pred

plot(ORS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(43000,165000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Oregon Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ORS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ORS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(ORS.auto.arima.pred$fitted, ORS.ts), 3)

legend(1997,160000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 160000))
lines(c(2019.5,2019.5), c(0,160000))
text(2008.25, 165000, "Data Set")
text(2022, 165000, "Future")
arrows(1997.5, 160000, 2019.25, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 160000, 2024, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
