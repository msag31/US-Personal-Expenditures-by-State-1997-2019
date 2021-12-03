library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
CODG.ts <- ts(df$Colorado.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
CODG.ts
min(CODG.ts)
max(CODG.ts)

plot(CODG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(11500,30000), main = "Value")

CODG.auto.arima<-auto.arima(CODG.ts)
summary(CODG.auto.arima)
CODG.auto.arima.pred <- forecast(CODG.auto.arima, h = 5, level = 0)
CODG.auto.arima.pred

plot(CODG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(11500,36000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Colorado Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(CODG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(CODG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(CODG.auto.arima.pred$fitted, CODG.ts), 3)

legend(1997,35000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 35000))
lines(c(2019.5,2019.5), c(0,35000))
text(2008.25, 36000, "Data Set")
text(2022, 36000, "Future")
arrows(1997.5, 35000, 2019.25, 35000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 35000, 2024, 35000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
CONDG.ts <- ts(df$Colorado.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(CONDG.ts)
max(CONDG.ts)

plot(CONDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(19000,53000), main = "Value")

CONDG.auto.arima<-auto.arima(CONDG.ts)
summary(CONDG.auto.arima)
CONDG.auto.arima.pred <- forecast(CONDG.auto.arima, h = 5, level = 0)
CONDG.auto.arima.pred

plot(CONDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(19000,64000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Colorado Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(CONDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(CONDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(CONDG.auto.arima.pred$fitted, CONDG.ts), 3)

legend(1997,61000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 61000))
lines(c(2019.5,2019.5), c(0,61000))
text(2008.25, 63000, "Data Set")
text(2022, 63000, "Future")
arrows(1997.5, 61000, 2019.25, 61000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 61000, 2024, 61000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
COS.ts <- ts(df$Colorado.Services, start = c(1997), end=c(2019), freq = 1)
min(COS.ts)
max(COS.ts)

plot(COS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(57000,190000), main = "Value")

COS.auto.arima<-auto.arima(COS.ts)
summary(COS.auto.arima)
COS.auto.arima.pred <- forecast(COS.auto.arima, h = 5, level = 0)
COS.auto.arima.pred

plot(COS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(57000,260000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Colorado Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(COS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(COS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(COS.auto.arima.pred$fitted, COS.ts), 3)

legend(1997,240000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 240000))
lines(c(2019.5,2019.5), c(0,240000))
text(2008.25, 255000, "Data Set")
text(2022, 255000, "Future")
arrows(1997.5, 240000, 2019.25, 240000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 240000, 2024, 240000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
