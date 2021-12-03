library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
IADG.ts <- ts(df$Iowa.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
IADG.ts
min(IADG.ts)
max(IADG.ts)

plot(IADG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(7000,15000), main = "Value")

IADG.auto.arima<-auto.arima(IADG.ts)
summary(IADG.auto.arima)
IADG.auto.arima.pred <- forecast(IADG.auto.arima, h = 5, level = 0)
IADG.auto.arima.pred

plot(IADG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(7000,18000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Iowa Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(IADG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(IADG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(IADG.auto.arima.pred$fitted,IADG.ts), 3)

legend(1997,17000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 17000))
lines(c(2019.5,2019.5), c(0,17000))
text(2008.25, 17500, "Data Set")
text(2022, 17500, "Future")
arrows(1997.5,17000, 2019.25, 17000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 17000, 2024, 17000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
IANDG.ts <- ts(df$Iowa.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(IANDG.ts)
max(IANDG.ts)

plot(IANDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(13000,28000), main = "Value")

IANDG.auto.arima<-auto.arima(IANDG.ts)
summary(IANDG.auto.arima)
IANDG.auto.arima.pred <- forecast(IANDG.auto.arima, h = 5, level = 0)
IANDG.auto.arima.pred

plot(IANDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(13000,32000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Iowa Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(IANDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(IANDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(IANDG.auto.arima.pred$fitted,IANDG.ts), 3)

legend(1997,31000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 31000))
lines(c(2019.5,2019.5), c(0,31000))
text(2008.25, 31500, "Data Set")
text(2022, 31500, "Future")
arrows(1997.5, 31000, 2019.25, 31000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 31000, 2024, 31000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
IAS.ts <- ts(df$Iowa.Services, start = c(1997), end=c(2019), freq = 1)
min(IAS.ts)
max(IAS.ts)

plot(IAS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(32000,85000), main = "Value")

IAS.auto.arima<-auto.arima(IAS.ts)
summary(IAS.auto.arima)
IAS.auto.arima.pred <- forecast(IAS.auto.arima, h = 5, level = 0)
IAS.auto.arima.pred

plot(IAS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(32000,99000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Iowa Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(IAS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(IAS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(IAS.auto.arima.pred$fitted, IAS.ts), 3)

legend(1997,95000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 95000))
lines(c(2019.5,2019.5), c(0,95000))
text(2008.25, 98000, "Data Set")
text(2022, 98000, "Future")
arrows(1997.5, 95000, 2019.25, 95000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 95000, 2024, 95000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
