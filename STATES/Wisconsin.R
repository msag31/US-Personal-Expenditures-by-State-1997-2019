library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
WIDG.ts <- ts(df$Wisconsin.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
WIDG.ts
min(WIDG.ts)
max(WIDG.ts)

WIDG.auto.arima<-auto.arima(WIDG.ts)
summary(WIDG.auto.arima)
WIDG.auto.arima.pred <- forecast(WIDG.auto.arima, h = 5, level = 0)
WIDG.auto.arima.pred

plot(WIDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(14000,35000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Wisconsin Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WIDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WIDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(WIDG.auto.arima.pred$fitted,WIDG.ts), 3)

legend(1997,32000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 32000))
lines(c(2019.5,2019.5), c(0,32000))
text(2008.25, 33500, "Data Set")
text(2022, 33500, "Future")
arrows(1997.5, 32000, 2019.25, 32000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 32000, 2024, 32000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
WINDG.ts <- ts(df$Wisconsin.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(WINDG.ts)
max(WINDG.ts)

WINDG.auto.arima<-auto.arima(WINDG.ts)
summary(WINDG.auto.arima)
WINDG.auto.arima.pred <- forecast(WINDG.auto.arima, h = 5, level = 0)
WINDG.auto.arima.pred

plot(WINDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(24000,62500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Wisconsin Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WINDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WINDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(WINDG.auto.arima.pred$fitted,WINDG.ts), 3)

legend(1997,59000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 59000))
lines(c(2019.5,2019.5), c(0,59000))
text(2008.25, 61500, "Data Set")
text(2022, 61500, "Future")
arrows(1997.5, 59000, 2019.25, 59000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 59000, 2024, 59000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
WIS.ts <- ts(df$Wisconsin.Services, start = c(1997), end=c(2019), freq = 1)
min(WIS.ts)
max(WIS.ts)

WIS.auto.arima<-auto.arima(WIS.ts)
summary(WIS.auto.arima)
WIS.auto.arima.pred <- forecast(WIS.auto.arima, h = 5, level = 0)
WIS.auto.arima.pred

plot(WIS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(65000,220000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Wisconsin Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WIS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WIS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(WIS.auto.arima.pred$fitted, WIS.ts), 3)

legend(1997,205000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 205000))
lines(c(2019.5,2019.5), c(0,205000))
text(2008.25, 215000, "Data Set")
text(2022, 215000, "Future")
arrows(1997.5, 205000, 2019.25, 205000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 205000, 2024, 205000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
