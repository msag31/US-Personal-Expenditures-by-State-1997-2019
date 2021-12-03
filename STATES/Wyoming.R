library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
WYDG.ts <- ts(df$Wyoming.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
WYDG.ts
min(WYDG.ts)
max(WYDG.ts)

WYDG.auto.arima<-auto.arima(WYDG.ts)
summary(WYDG.auto.arima)
WYDG.auto.arima.pred <- forecast(WYDG.auto.arima, h = 5, level = 0)
WYDG.auto.arima.pred

plot(WYDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(1200,3000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Wyoming Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WYDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WYDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(WYDG.auto.arima.pred$fitted,WYDG.ts), 3)

legend(1997,2800, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 2800))
lines(c(2019.5,2019.5), c(0,2800))
text(2008.25, 2900, "Data Set")
text(2022, 2900, "Future")
arrows(1997.5, 2800, 2019.25, 2800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 2800, 2024, 2800, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
WYNDG.ts <- ts(df$Wyoming.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(WYNDG.ts)
max(WYNDG.ts)

WYNDG.auto.arima<-auto.arima(WYNDG.ts)
summary(WYNDG.auto.arima)
WYNDG.auto.arima.pred <- forecast(WYNDG.auto.arima, h = 5, level = 0)
WYNDG.auto.arima.pred

plot(WYNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(2400,6500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Wyoming Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WYNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WYNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(WYNDG.auto.arima.pred$fitted,WYNDG.ts), 3)

legend(1997,6300, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 6300))
lines(c(2019.5,2019.5), c(0,6300))
text(2008.25, 6400, "Data Set")
text(2022, 6400, "Future")
arrows(1997.5, 6300, 2019.25, 6300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 6300, 2024, 6300, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
WYS.ts <- ts(df$Wyoming.Services, start = c(1997), end=c(2019), freq = 1)
min(WYS.ts)
max(WYS.ts)

WYS.auto.arima<-auto.arima(WYS.ts)
summary(WYS.auto.arima)
WYS.auto.arima.pred <- forecast(WYS.auto.arima, h = 5, level = 0)
WYS.auto.arima.pred

plot(WYS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(5400,23000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Wyoming Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WYS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WYS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(WYS.auto.arima.pred$fitted, WYS.ts), 3)

legend(1997,21500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 21500))
lines(c(2019.5,2019.5), c(0,21500))
text(2008.25, 22500, "Data Set")
text(2022, 22500, "Future")
arrows(1997.5, 21500, 2019.25, 21500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 21500, 2024, 21500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
