library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
PANG.ts <- ts(df$Pennsylvania.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
PANG.ts
min(PANG.ts)
max(PANG.ts)

PANG.auto.arima<-auto.arima(PANG.ts)
summary(PANG.auto.arima)
PANG.auto.arima.pred <- forecast(PANG.auto.arima, h = 5, level = 0)
PANG.auto.arima.pred

plot(PANG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(30000,67000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Pennsylvania Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(PANG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(PANG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(PANG.auto.arima.pred$fitted,PANG.ts), 3)

legend(1997,64500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 64500))
lines(c(2019.5,2019.5), c(0,64500))
text(2008.25, 66000, "Data Set")
text(2022, 66000, "Future")
arrows(1997.5, 64500, 2019.25, 64500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 64500, 2024, 64500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
PANNG.ts <- ts(df$Pennsylvania.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(PANNG.ts)
max(PANNG.ts)

PANNG.auto.arima<-auto.arima(PANNG.ts)
summary(PANNG.auto.arima)
PANNG.auto.arima.pred <- forecast(PANNG.auto.arima, h = 5, level = 0)
PANNG.auto.arima.pred

plot(PANNG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(57000,140000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Pennsylvania Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(PANNG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(PANNG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(PANNG.auto.arima.pred$fitted,PANNG.ts), 3)

legend(1997,135000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 135000))
lines(c(2019.5,2019.5), c(0,135000))
text(2008.25, 138000, "Data Set")
text(2022, 138000, "Future")
arrows(1997.5, 135000, 2019.25, 135000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 135000, 2024, 135000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
PAS.ts <- ts(df$Pennsylvania.Services, start = c(1997), end=c(2019), freq = 1)
min(PAS.ts)
max(PAS.ts)

PAS.auto.arima<-auto.arima(PAS.ts)
summary(PAS.auto.arima)
PAS.auto.arima.pred <- forecast(PAS.auto.arima, h = 5, level = 0)
PAS.auto.arima.pred

plot(PAS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(160000,510000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Pennsylvania Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(PAS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(PAS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(PAS.auto.arima.pred$fitted, PAS.ts), 3)

legend(1997,484000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 484000))
lines(c(2019.5,2019.5), c(0,484000))
text(2008.25, 505000, "Data Set")
text(2022, 505000, "Future")
arrows(1997.5, 484000, 2019.25, 484000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 484000, 2024, 484000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
