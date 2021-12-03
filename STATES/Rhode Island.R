library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
RIDG.ts <- ts(df$Rhode.Island.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(RIDG.ts)
max(RIDG.ts)

RIDG.auto.arima<-auto.arima(RIDG.ts)
summary(RIDG.auto.arima)
RIDG.auto.arima.pred <- forecast(RIDG.auto.arima, h = 5, level = 0)
RIDG.auto.arima.pred

plot(RIDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(1800,4500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Rhode Island Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(RIDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(RIDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(RIDG.auto.arima.pred$fitted,RIDG.ts), 3)

legend(1997,4200, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 4200))
lines(c(2019.5,2019.5), c(0,4200))
text(2008.25, 4300, "Data Set")
text(2022, 4300, "Future")
arrows(1997.5, 4200, 2019.25, 4200, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 4200, 2024, 4200, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
RINDG.ts <- ts(df$Rhode.Island.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(RINDG.ts)
max(RINDG.ts)

RINDG.auto.arima<-auto.arima(RINDG.ts)
summary(RINDG.auto.arima)
RINDG.auto.arima.pred <- forecast(RINDG.auto.arima, h = 5, level = 0)
RINDG.auto.arima.pred

plot(RINDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(4500,13500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Rhode Island Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(RINDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(RINDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(RINDG.auto.arima.pred$fitted,RINDG.ts), 3)

legend(1997,12500, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 12500))
lines(c(2019.5,2019.5), c(0,12500))
text(2008.25, 13000, "Data Set")
text(2022, 13000, "Future")
arrows(1997.5, 12500, 2019.25, 12500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 12500, 2024, 12500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
RIS.ts <- ts(df$Rhode.Island.Services, start = c(1997), end=c(2019), freq = 1)
min(RIS.ts)
max(RIS.ts)

RIS.auto.arima<-auto.arima(RIS.ts)
summary(RIS.auto.arima)
RIS.auto.arima.pred <- forecast(RIS.auto.arima, h = 5, level = 0)
RIS.auto.arima.pred

plot(RIS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(14000,45000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Rhode Island Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(RIS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(RIS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(RIS.auto.arima.pred$fitted, RIS.ts), 3)

legend(1997,41500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 41500))
lines(c(2019.5,2019.5), c(0,41500))
text(2008.25, 44000, "Data Set")
text(2022, 44000, "Future")
arrows(1997.5, 41500, 2019.25, 41500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 41500, 2024, 41500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
