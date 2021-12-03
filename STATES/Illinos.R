library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
ILDG.ts <- ts(df$Illinois.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
ILDG.ts
min(ILDG.ts)
max(ILDG.ts)

plot(ILDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(33000,60000), main = "Value")

ILDG.auto.arima<-auto.arima(ILDG.ts)
summary(ILDG.auto.arima)
ILDG.auto.arima.pred <- forecast(ILDG.auto.arima, h = 5, level = 0)
ILDG.auto.arima.pred

plot(ILDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(33000,66000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Illinois Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ILDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ILDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(ILDG.auto.arima.pred$fitted,ILDG.ts), 3)

legend(1997,64000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 64000))
lines(c(2019.5,2019.5), c(0,64000))
text(2008.25, 65000, "Data Set")
text(2022, 65000, "Future")
arrows(1997.5,64000, 2019.25, 64000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 64000, 2024, 64000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
ILNDG.ts <- ts(df$Illinois.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(ILNDG.ts)
max(ILNDG.ts)

plot(ILNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(55000,114000), main = "Value")

ILNDG.auto.arima<-auto.arima(ILNDG.ts)
summary(ILNDG.auto.arima)
ILNDG.auto.arima.pred <- forecast(ILNDG.auto.arima, h = 5, level = 0)
ILNDG.auto.arima.pred

plot(ILNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(55000,140000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Illinois Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ILNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ILNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(ILNDG.auto.arima.pred$fitted,ILNDG.ts), 3)

legend(1997,130000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 130000))
lines(c(2019.5,2019.5), c(0,130000))
text(2008.25, 135000, "Data Set")
text(2022, 135000, "Future")
arrows(1997.5, 130000, 2019.25, 130000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 130000, 2024, 130000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
ILS.ts <- ts(df$Illinois.Services, start = c(1997), end=c(2019), freq = 1)
min(ILS.ts)
max(ILS.ts)

plot(ILS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(170000,420000), main = "Value")

ILS.auto.arima<-auto.arima(ILS.ts)
summary(ILS.auto.arima)
ILS.auto.arima.pred <- forecast(ILS.auto.arima, h = 5, level = 0)
ILS.auto.arima.pred

plot(ILS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(170000,510000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Illinois Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(ILS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(ILS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(ILS.auto.arima.pred$fitted, ILS.ts), 3)

legend(1997,480000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 480000))
lines(c(2019.5,2019.5), c(0,480000))
text(2008.25, 500000, "Data Set")
text(2022, 500000, "Future")
arrows(1997.5, 480000, 2019.25, 480000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 480000, 2024, 480000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
