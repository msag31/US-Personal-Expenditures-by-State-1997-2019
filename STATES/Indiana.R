library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
INDG.ts <- ts(df$Indiana.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
INDG.ts
min(INDG.ts)
max(INDG.ts)

plot(INDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(15000,30000), main = "Value")

INDG.auto.arima<-auto.arima(INDG.ts)
summary(INDG.auto.arima)
INDG.auto.arima.pred <- forecast(INDG.auto.arima, h = 5, level = 0)
INDG.auto.arima.pred

plot(INDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(15000,35000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Indiana Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(INDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(INDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(INDG.auto.arima.pred$fitted,INDG.ts), 3)

legend(1997,33000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 33000))
lines(c(2019.5,2019.5), c(0,33000))
text(2008.25, 34000, "Data Set")
text(2022, 34000, "Future")
arrows(1997.5,33000, 2019.25, 33000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 33000, 2024, 33000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
INNDG.ts <- ts(df$Indiana.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(INNDG.ts)
max(INNDG.ts)

plot(INNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(28000,60000), main = "Value")

INNDG.auto.arima<-auto.arima(INNDG.ts)
summary(INNDG.auto.arima)
INNDG.auto.arima.pred <- forecast(INNDG.auto.arima, h = 5, level = 0)
INNDG.auto.arima.pred

plot(INNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(28000,69000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Indiana Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(INNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(INNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(INNDG.auto.arima.pred$fitted,INNDG.ts), 3)

legend(1997,68000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 68000))
lines(c(2019.5,2019.5), c(0,68000))
text(2008.25, 69500, "Data Set")
text(2022, 69500, "Future")
arrows(1997.5, 68000, 2019.25, 68000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 68000, 2024, 68000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
INS.ts <- ts(df$Indiana.Services, start = c(1997), end=c(2019), freq = 1)
min(INS.ts)
max(INS.ts)

plot(INS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(67000,180000), main = "Value")

INS.auto.arima<-auto.arima(INS.ts)
summary(INS.auto.arima)
INS.auto.arima.pred <- forecast(INS.auto.arima, h = 5, level = 0)
INS.auto.arima.pred

plot(INS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(67000,220000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Indiana Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(INS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(INS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(INS.auto.arima.pred$fitted, INS.ts), 3)

legend(1997,210000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 210000))
lines(c(2019.5,2019.5), c(0,210000))
text(2008.25, 220000, "Data Set")
text(2022, 220000, "Future")
arrows(1997.5, 210000, 2019.25, 210000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 210000, 2024, 210000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
