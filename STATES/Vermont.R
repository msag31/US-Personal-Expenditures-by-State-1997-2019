library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
VTDG.ts <- ts(df$Vermont.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
VTDG.ts
min(VTDG.ts)
max(VTDG.ts)

VTDG.auto.arima<-auto.arima(VTDG.ts)
summary(VTDG.auto.arima)
VTDG.auto.arima.pred <- forecast(VTDG.auto.arima, h = 5, level = 0)
VTDG.auto.arima.pred

plot(VTDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(1400,3700), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Vermont Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(VTDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(VTDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(VTDG.auto.arima.pred$fitted,VTDG.ts), 3)

legend(1997,3550, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 3550))
lines(c(2019.5,2019.5), c(0,3550))
text(2008.25, 3650, "Data Set")
text(2022, 3650, "Future")
arrows(1997.5, 3550, 2019.25, 3550, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 3550, 2024, 3550, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
VTNDG.ts <- ts(df$Vermont.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(VTNDG.ts)
max(VTNDG.ts)

VTNDG.auto.arima<-auto.arima(VTNDG.ts)
summary(VTNDG.auto.arima)
VTNDG.auto.arima.pred <- forecast(VTNDG.auto.arima, h = 5, level = 0)
VTNDG.auto.arima.pred

plot(VTNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3000,7700), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Vermont Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(VTNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(VTNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(VTNDG.auto.arima.pred$fitted,VTNDG.ts), 3)

legend(1997,7500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 7500))
lines(c(2019.5,2019.5), c(0,7500))
text(2008.25, 7650, "Data Set")
text(2022, 7650, "Future")
arrows(1997.5, 7500, 2019.25, 7500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 7500, 2024, 7500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
VTS.ts <- ts(df$Vermont.Services, start = c(1997), end=c(2019), freq = 1)
min(VTS.ts)
max(VTS.ts)

VTS.auto.arima<-auto.arima(VTS.ts)
summary(VTS.auto.arima)
VTS.auto.arima.pred <- forecast(VTS.auto.arima, h = 5, level = 0)
VTS.auto.arima.pred

plot(VTS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(8100,28000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Vermont Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(VTS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(VTS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(VTS.auto.arima.pred$fitted, VTS.ts), 3)

legend(1997,25500, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 25500))
lines(c(2019.5,2019.5), c(0,25500))
text(2008.25, 27000, "Data Set")
text(2022, 27000, "Future")
arrows(1997.5, 25500, 2019.25, 25500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 25500, 2024, 25500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
