library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
DEDG.ts <- ts(df$Delaware.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
DEDG.ts
min(DEDG.ts)
max(DEDG.ts)

plot(DEDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(2450,5000), main = "Value")

DEDG.auto.arima<-auto.arima(DEDG.ts)
summary(DEDG.auto.arima)
DEDG.auto.arima.pred <- forecast(DEDG.auto.arima, h = 5, level = 0)
DEDG.auto.arima.pred

plot(DEDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(2450,6500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Delaware Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(DEDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(DEDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(DEDG.auto.arima.pred$fitted, DEDG.ts), 3)

legend(1997,5500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 5700))
lines(c(2019.5,2019.5), c(0,5700))
text(2008.25, 5900, "Data Set")
text(2022, 5900, "Future")
arrows(1997.5, 5700, 2019.25, 5700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 5700, 2024, 5700, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
DENDG.ts <- ts(df$Delaware.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(DENDG.ts)
max(DENDG.ts)

plot(DENDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(3400,9000), main = "Value")

DENDG.auto.arima<-auto.arima(DENDG.ts)
summary(DENDG.auto.arima)
DENDG.auto.arima.pred <- forecast(DENDG.auto.arima, h = 5, level = 0)
DENDG.auto.arima.pred

plot(DENDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3400,11000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Delaware Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(DENDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(DENDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(DENDG.auto.arima.pred$fitted, DENDG.ts), 3)

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

#Services
DES.ts <- ts(df$Delaware.Services, start = c(1997), end=c(2019), freq = 1)
min(DES.ts)
max(DES.ts)

plot(DES.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(10000,32000), main = "Value")

DES.auto.arima<-auto.arima(DES.ts)
summary(DES.auto.arima)
DES.auto.arima.pred <- forecast(DES.auto.arima, h = 5, level = 0)
DES.auto.arima.pred

plot(DES.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(10000,38000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Delaware Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(DES.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(DES.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(DES.auto.arima.pred$fitted, DES.ts), 3)

legend(1997,36000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 36000))
lines(c(2019.5,2019.5), c(0,36000))
text(2008.25, 37500, "Data Set")
text(2022, 37500, "Future")
arrows(1997.5, 36000, 2019.25, 36000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 36000, 2024, 36000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
