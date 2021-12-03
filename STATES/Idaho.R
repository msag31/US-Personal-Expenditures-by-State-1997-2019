library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
IDDG.ts <- ts(df$Idaho.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
IDDG.ts
min(IDDG.ts)
max(IDDG.ts)

plot(IDDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(3400,9000), main = "Value")

IDDG.auto.arima<-auto.arima(IDDG.ts)
summary(IDDG.auto.arima)
IDDG.auto.arima.pred <- forecast(IDDG.auto.arima, h = 5, level = 0)
IDDG.auto.arima.pred

plot(IDDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3400,12000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Idaho Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(IDDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(IDDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(IDDG.auto.arima.pred$fitted,IDDG.ts), 3)

legend(1997,11000, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 11000))
lines(c(2019.5,2019.5), c(0,11000))
text(2008.25, 11500, "Data Set")
text(2022, 11500, "Future")
arrows(1997.5,11000, 2019.25, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 11000, 2024, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
IDNDG.ts <- ts(df$Idaho.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(IDNDG.ts)
max(IDNDG.ts)

plot(IDNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(5200,14000), main = "Value")

IDNDG.auto.arima<-auto.arima(IDNDG.ts)
summary(IDNDG.auto.arima)
IDNDG.auto.arima.pred <- forecast(IDNDG.auto.arima, h = 5, level = 0)
IDNDG.auto.arima.pred

plot(IDNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(5200,16500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Idaho Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(IDNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(IDNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(IDNDG.auto.arima.pred$fitted,IDNDG.ts), 3)

legend(1997,16000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 16000))
lines(c(2019.5,2019.5), c(0,16000))
text(2008.25, 16500, "Data Set")
text(2022, 16500, "Future")
arrows(1997.5, 16000, 2019.25, 16000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 16000, 2024, 16000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
IDS.ts <- ts(df$Idaho.Services, start = c(1997), end=c(2019), freq = 1)
min(IDS.ts)
max(IDS.ts)

plot(IDS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(12000,45000), main = "Value")

IDS.auto.arima<-auto.arima(IDS.ts)
summary(IDS.auto.arima)
IDS.auto.arima.pred <- forecast(IDS.auto.arima, h = 5, level = 0)
IDS.auto.arima.pred

plot(IDS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(12000,58500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Idaho Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(IDS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(IDS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(IDS.auto.arima.pred$fitted, IDS.ts), 3)

legend(1997,57000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 57000))
lines(c(2019.5,2019.5), c(0,57000))
text(2008.25, 58500, "Data Set")
text(2022, 58500, "Future")
arrows(1997.5, 57000, 2019.25, 57000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 57000, 2024, 57000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
