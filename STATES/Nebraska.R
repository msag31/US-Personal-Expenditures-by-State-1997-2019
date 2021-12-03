library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
NEDG.ts <- ts(df$Nebraska.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
NEDG.ts
min(NEDG.ts)
max(NEDG.ts)

NEDG.auto.arima<-auto.arima(NEDG.ts)
summary(NEDG.auto.arima)
NEDG.auto.arima.pred <- forecast(NEDG.auto.arima, h = 5, level = 0)
NEDG.auto.arima.pred

plot(NEDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(4700,12000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Nebraska Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NEDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NEDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(NEDG.auto.arima.pred$fitted,NEDG.ts), 3)

legend(1997,11000, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 11000))
lines(c(2019.5,2019.5), c(0,11000))
text(2008.25, 11500, "Data Set")
text(2022, 11500, "Future")
arrows(1997.5, 11000, 2019.25, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 11000, 2024, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
NENDG.ts <- ts(df$Nebraska.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NENDG.ts)
max(NENDG.ts)

NENDG.auto.arima<-auto.arima(NENDG.ts)
summary(NENDG.auto.arima)
NENDG.auto.arima.pred <- forecast(NENDG.auto.arima, h = 5, level = 0)
NENDG.auto.arima.pred

plot(NENDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(7600,20000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Nebraska Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NENDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NENDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NENDG.auto.arima.pred$fitted,NENDG.ts), 3)

legend(1997,19000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 19000))
lines(c(2019.5,2019.5), c(0,19000))
text(2008.25, 19500, "Data Set")
text(2022, 19500, "Future")
arrows(1997.5, 19000, 2019.25, 19000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 19000, 2024, 19000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
NES.ts <- ts(df$Nebraska.Services, start = c(1997), end=c(2019), freq = 1)
min(NES.ts)
max(NES.ts)

NES.auto.arima<-auto.arima(NES.ts)
summary(NES.auto.arima)
NES.auto.arima.pred <- forecast(NES.auto.arima, h = 5, level = 0)
NES.auto.arima.pred

plot(NES.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(19000,65000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Nebraska Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NES.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NES.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NES.auto.arima.pred$fitted, NES.ts), 3)

legend(1997,63500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 63500))
lines(c(2019.5,2019.5), c(0,63500))
text(2008.25, 65000, "Data Set")
text(2022, 65000, "Future")
arrows(1997.5, 63500, 2019.25, 63500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 63500, 2024, 63500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
