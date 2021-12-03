library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
KYDG.ts <- ts(df$Kentucky.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
KYDG.ts
min(KYDG.ts)
max(KYDG.ts)

plot(KYDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(6800,13000), main = "Value")

KYDG.auto.arima<-auto.arima(KYDG.ts)
summary(KYDG.auto.arima)
KYDG.auto.arima.pred <- forecast(KYDG.auto.arima, h = 5, level = 0)
KYDG.auto.arima.pred

plot(KYDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(9000,21000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Kentucky Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(KYDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(KYDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(KYDG.auto.arima.pred$fitted,KYDG.ts), 3)

legend(1997,20000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 20000))
lines(c(2019.5,2019.5), c(0,20000))
text(2008.25, 20500, "Data Set")
text(2022, 20500, "Future")
arrows(1997.5, 20000, 2019.25, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 20000, 2024, 20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
KYNDG.ts <- ts(df$Kentucky.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(KYNDG.ts)
max(KYNDG.ts)

plot(KYNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(12000,25000), main = "Value")

KYNDG.auto.arima<-auto.arima(KYNDG.ts)
summary(KSNDG.auto.arima)
KYNDG.auto.arima.pred <- forecast(KYNDG.auto.arima, h = 5, level = 0)
KYNDG.auto.arima.pred

plot(KYNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(18000,48000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Kentucky Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(KYNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(KYNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(KYNDG.auto.arima.pred$fitted,KYNDG.ts), 3)

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
KYS.ts <- ts(df$Kentucky.Services, start = c(1997), end=c(2019), freq = 1)
min(KYS.ts)
max(KYS.ts)

plot(KYS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(28000,77000), main = "Value")

KYS.auto.arima<-auto.arima(KYS.ts)
summary(KYS.auto.arima)
KYS.auto.arima.pred <- forecast(KYS.auto.arima, h = 5, level = 0)
KYS.auto.arima.pred

plot(KYS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(40000,130000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Kentucky Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(KYS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(KYS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(KYS.auto.arima.pred$fitted, KYS.ts), 3)

legend(1997,122000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 122000))
lines(c(2019.5,2019.5), c(0,122000))
text(2008.25, 126000, "Data Set")
text(2022, 126000, "Future")
arrows(1997.5, 122000, 2019.25, 122000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 122000, 2024, 122000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
