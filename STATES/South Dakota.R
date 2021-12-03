library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
SDDG.ts <- ts(df$South.Dakota.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(SDDG.ts)
max(SDDG.ts)

SDDG.auto.arima<-auto.arima(SDDG.ts)
summary(SDDG.auto.arima)
SDDG.auto.arima.pred <- forecast(SDDG.auto.arima, h = 5, level = 0)
SDDG.auto.arima.pred

plot(SDDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(1900,5900), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "South Dakota Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(SDDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(SDDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(SDDG.auto.arima.pred$fitted,SDDG.ts), 3)

legend(1997,5700, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 5700))
lines(c(2019.5,2019.5), c(0,5700))
text(2008.25, 5800, "Data Set")
text(2022, 5800, "Future")
arrows(1997.5, 5700, 2019.25, 5700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 5700, 2024, 5700, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
SDNDG.ts <- ts(df$South.Dakota.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(SDNDG.ts)
max(SDNDG.ts)

SDNDG.auto.arima<-auto.arima(SDNDG.ts)
summary(SDNDG.auto.arima)
SDNDG.auto.arima.pred <- forecast(SDNDG.auto.arima, h = 5, level = 0)
SDNDG.auto.arima.pred

plot(SDNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3200,9000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "South Dakota Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(SDNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(SDNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(SDNDG.auto.arima.pred$fitted,SDNDG.ts), 3)

legend(1997,8600, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 8600))
lines(c(2019.5,2019.5), c(0,8600))
text(2008.25, 8800, "Data Set")
text(2022, 8800, "Future")
arrows(1997.5, 8600, 2019.25, 8600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 8600, 2024, 8600, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
SDS.ts <- ts(df$South.Dakota.Services, start = c(1997), end=c(2019), freq = 1)
min(SDS.ts)
max(SDS.ts)

SDS.auto.arima<-auto.arima(SDS.ts)
summary(SDS.auto.arima)
SDS.auto.arima.pred <- forecast(SDS.auto.arima, h = 5, level = 0)
SDS.auto.arima.pred

plot(SDS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(8300,40000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "South Dakota Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(SDS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(SDS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(SDS.auto.arima.pred$fitted, SDS.ts), 3)

legend(1997,37000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 37000))
lines(c(2019.5,2019.5), c(0,37000))
text(2008.25, 38500, "Data Set")
text(2022, 38500, "Future")
arrows(1997.5, 37000, 2019.25, 37000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 37000, 2024, 37000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
