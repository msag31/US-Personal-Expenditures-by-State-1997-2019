library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
OHNG.ts <- ts(df$Ohio.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
OHNG.ts
min(OHNG.ts)
max(OHNG.ts)

OHNG.auto.arima<-auto.arima(OHNG.ts)
summary(OHNG.auto.arima)
OHNG.auto.arima.pred <- forecast(OHNG.auto.arima, h = 5, level = 0)
OHNG.auto.arima.pred

plot(OHNG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(29000,62000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Ohio Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(OHNG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(OHNG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(OHNG.auto.arima.pred$fitted,OHNG.ts), 3)

legend(1997,59000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 59000))
lines(c(2019.5,2019.5), c(0,59000))
text(2008.25, 60000, "Data Set")
text(2022, 60000, "Future")
arrows(1997.5, 59000, 2019.25, 59000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 59000, 2024, 59000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
OHNNG.ts <- ts(df$Ohio.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(OHNNG.ts)
max(OHNNG.ts)

OHNNG.auto.arima<-auto.arima(OHNNG.ts)
summary(OHNNG.auto.arima)
OHNNG.auto.arima.pred <- forecast(OHNNG.auto.arima, h = 5, level = 0)
OHNNG.auto.arima.pred

plot(OHNNG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(52000,120000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Ohio Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(OHNNG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(OHNNG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(OHNNG.auto.arima.pred$fitted,OHNNG.ts), 3)

legend(1997,115000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 115000))
lines(c(2019.5,2019.5), c(0,115000))
text(2008.25, 117500, "Data Set")
text(2022, 117500, "Future")
arrows(1997.5, 115000, 2019.25, 115000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 115000, 2024, 115000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
OHS.ts <- ts(df$Ohio.Services, start = c(1997), end=c(2019), freq = 1)
min(OHS.ts)
max(OHS.ts)

OHS.auto.arima<-auto.arima(OHS.ts)
summary(OHS.auto.arima)
OHS.auto.arima.pred <- forecast(OHS.auto.arima, h = 5, level = 0)
OHS.auto.arima.pred

plot(OHS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(135000,420000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Ohio Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(OHS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(OHS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(OHS.auto.arima.pred$fitted, OHS.ts), 3)

legend(1997,405000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 405000))
lines(c(2019.5,2019.5), c(0,405000))
text(2008.25, 418000, "Data Set")
text(2022, 418000, "Future")
arrows(1997.5, 405000, 2019.25, 405000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 405000, 2024, 405000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
