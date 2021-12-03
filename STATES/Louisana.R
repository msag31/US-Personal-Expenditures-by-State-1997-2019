library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
LADG.ts <- ts(df$Louisiana.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
LADG.ts
min(LADG.ts)
max(LADG.ts)

plot(LADG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(6800,13000), main = "Value")

LADG.auto.arima<-auto.arima(LADG.ts)
summary(LADG.auto.arima)
LADG.auto.arima.pred <- forecast(LADG.auto.arima, h = 5, level = 0)
LADG.auto.arima.pred

plot(LADG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(10000,23000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Louisiana Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(LADG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(LADG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(LADG.auto.arima.pred$fitted,LADG.ts), 3)

legend(1997,21500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 21500))
lines(c(2019.5,2019.5), c(0,21500))
text(2008.25, 22000, "Data Set")
text(2022, 22000, "Future")
arrows(1997.5, 21500, 2019.25, 21500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 21500, 2024, 21500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
LANDG.ts <- ts(df$Louisiana.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(LANDG.ts)
max(LANDG.ts)

plot(LANDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(19000,41000), main = "Value")

LANDG.auto.arima<-auto.arima(LANDG.ts)
summary(LANDG.auto.arima)
LANDG.auto.arima.pred <- forecast(LANDG.auto.arima, h = 5, level = 0)
LANDG.auto.arima.pred

plot(LANDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(19000,48000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Louisiana Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(LANDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(LANDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(LANDG.auto.arima.pred$fitted,LANDG.ts), 3)

legend(1997,47000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 47000))
lines(c(2019.5,2019.5), c(0,47000))
text(2008.25, 48000, "Data Set")
text(2022, 48000, "Future")
arrows(1997.5, 47000, 2019.25, 47000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 47000, 2024, 47000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
LAS.ts <- ts(df$Louisiana.Services, start = c(1997), end=c(2019), freq = 1)
min(LAS.ts)
max(LAS.ts)

plot(LAS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(44000,115000), main = "Value")

LAS.auto.arima<-auto.arima(LAS.ts)
summary(LAS.auto.arima)
LAS.auto.arima.pred <- forecast(LAS.auto.arima, h = 5, level = 0)
LAS.auto.arima.pred

plot(LAS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(44000,140000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Louisiana Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(LAS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(LAS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(LAS.auto.arima.pred$fitted, LAS.ts), 3)

legend(1997,131000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 131000))
lines(c(2019.5,2019.5), c(0,131000))
text(2008.25, 136000, "Data Set")
text(2022, 136000, "Future")
arrows(1997.5, 131000, 2019.25, 131000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 131000, 2024, 131000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
