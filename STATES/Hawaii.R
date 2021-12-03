library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
HIDG.ts <- ts(df$Hawaii.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
HIDG.ts
min(HIDG.ts)
max(HIDG.ts)

plot(HIDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(2650,7000), main = "Value")

HIDG.auto.arima<-auto.arima(HIDG.ts)
summary(HIDG.auto.arima)
HIDG.auto.arima.pred <- forecast(HIDG.auto.arima, h = 5, level = 0)
HIDG.auto.arima.pred

plot(HIDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(2650,8500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Hawaii Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(HIDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(HIDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(HIDG.auto.arima.pred$fitted,HIDG.ts), 3)

legend(1997,8000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 8000))
lines(c(2019.5,2019.5), c(0,8000))
text(2008.25, 8500, "Data Set")
text(2022, 8500, "Future")
arrows(1997.5,8000, 2019.25, 8000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 8000, 2024, 8000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
HINDG.ts <- ts(df$Hawaii.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(HINDG.ts)
max(HINDG.ts)

plot(HINDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(6000,14000), main = "Value")

HINDG.auto.arima<-auto.arima(HINDG.ts)
summary(HINDG.auto.arima)
HINDG.auto.arima.pred <- forecast(HINDG.auto.arima, h = 5, level = 0)
HINDG.auto.arima.pred

plot(HINDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(6000,18000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Hawaii Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(HINDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(HINDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(HINDG.auto.arima.pred$fitted,HINDG.ts), 3)

legend(1997,16500, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 16500))
lines(c(2019.5,2019.5), c(0,16500))
text(2008.25, 17000, "Data Set")
text(2022, 17000, "Future")
arrows(1997.5, 16500, 2019.25, 16500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 16500, 2024, 16500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
HIS.ts <- ts(df$Hawaii.Services, start = c(1997), end=c(2019), freq = 1)
min(HIS.ts)
max(HIS.ts)

plot(HIS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(16500,50000), main = "Value")

HIS.auto.arima<-auto.arima(HIS.ts)
summary(HIS.auto.arima)
HIS.auto.arima.pred <- forecast(HIS.auto.arima, h = 5, level = 0)
HIS.auto.arima.pred

plot(HIS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(16500,62500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Hawaii Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(HIS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(HIS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(HIS.auto.arima.pred$fitted, HIS.ts), 3)

legend(1997,61000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 61000))
lines(c(2019.5,2019.5), c(0,61000))
text(2008.25, 62500, "Data Set")
text(2022, 62500, "Future")
arrows(1997.5, 61000, 2019.25, 61000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 61000, 2024, 61000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
