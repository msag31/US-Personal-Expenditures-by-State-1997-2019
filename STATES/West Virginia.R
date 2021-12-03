library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
WVDG.ts <- ts(df$West.Virginia.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
WVDG.ts
min(WVDG.ts)
max(WVDG.ts)

WVDG.auto.arima<-auto.arima(WVDG.ts)
summary(WVDG.auto.arima)
WVDG.auto.arima.pred <- forecast(WVDG.auto.arima, h = 5, level = 0)
WVDG.auto.arima.pred

plot(WVDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3800,7500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "West Virginia Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WVDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WVDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(WVDG.auto.arima.pred$fitted,WVDG.ts), 3)

legend(1997,7200, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 7200))
lines(c(2019.5,2019.5), c(0,7200))
text(2008.25, 7350, "Data Set")
text(2022, 7350, "Future")
arrows(1997.5, 7200, 2019.25, 7200, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 7200, 2024, 7200, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
WVNDG.ts <- ts(df$West.Virginia.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(WVNDG.ts)
max(WVNDG.ts)

WVNDG.auto.arima<-auto.arima(WVNDG.ts)
summary(WVNDG.auto.arima)
WVNDG.auto.arima.pred <- forecast(WVNDG.auto.arima, h = 5, level = 0)
WVNDG.auto.arima.pred

plot(WVNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(7900,20000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "West Virginia Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WVNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WVNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(WVNDG.auto.arima.pred$fitted,WVNDG.ts), 3)

legend(1997,18500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 18500))
lines(c(2019.5,2019.5), c(0,18500))
text(2008.25, 19200, "Data Set")
text(2022, 19200, "Future")
arrows(1997.5, 18500, 2019.25, 18500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 18500, 2024, 18500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
WVS.ts <- ts(df$West.Virginia.Services, start = c(1997), end=c(2019), freq = 1)
min(WVS.ts)
max(WVS.ts)

WVS.auto.arima<-auto.arima(WVS.ts)
summary(WVS.auto.arima)
WVS.auto.arima.pred <- forecast(WVS.auto.arima, h = 5, level = 0)
WVS.auto.arima.pred

plot(WVS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(16500,55000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "West Virginia Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WVS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WVS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(WVS.auto.arima.pred$fitted, WVS.ts), 3)

legend(1997,52500, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 52500))
lines(c(2019.5,2019.5), c(0,52500))
text(2008.25, 54000, "Data Set")
text(2022, 54000, "Future")
arrows(1997.5, 52500, 2019.25, 52500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 52500, 2024, 52500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
