library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
KSDG.ts <- ts(df$Kansas.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
KSDG.ts
min(KSDG.ts)
max(KSDG.ts)

plot(KSDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(6800,13000), main = "Value")

KSDG.auto.arima<-auto.arima(KSDG.ts)
summary(KSDG.auto.arima)
KSDG.auto.arima.pred <- forecast(KSDG.auto.arima, h = 5, level = 0)
KSDG.auto.arima.pred

plot(KSDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(6800,15000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Kansas Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(KSDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(KSDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(KSDG.auto.arima.pred$fitted,KSDG.ts), 3)

legend(1997,14000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 14000))
lines(c(2019.5,2019.5), c(0,14000))
text(2008.25, 14500, "Data Set")
text(2022, 14500, "Future")
arrows(1997.5,14000, 2019.25, 14000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 14000, 2024, 14000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
KSNDG.ts <- ts(df$Kansas.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(KSNDG.ts)
max(KSNDG.ts)

plot(KSNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(12000,25000), main = "Value")

KSNDG.auto.arima<-auto.arima(KSNDG.ts)
summary(KSNDG.auto.arima)
KSNDG.auto.arima.pred <- forecast(KSNDG.auto.arima, h = 5, level = 0)
KSNDG.auto.arima.pred

plot(KSNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(12000,30000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Kansas Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(KSNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(KSNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(KSNDG.auto.arima.pred$fitted,KSNDG.ts), 3)

legend(1997,28500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 28500))
lines(c(2019.5,2019.5), c(0,28500))
text(2008.25, 29500, "Data Set")
text(2022, 29500, "Future")
arrows(1997.5, 28500, 2019.25, 28500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 28500, 2024, 28500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
KSS.ts <- ts(df$Kansas.Services, start = c(1997), end=c(2019), freq = 1)
min(KSS.ts)
max(KSS.ts)

plot(KSS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(28000,77000), main = "Value")

KSS.auto.arima<-auto.arima(KSS.ts)
summary(KSS.auto.arima)
KSS.auto.arima.pred <- forecast(KSS.auto.arima, h = 5, level = 0)
KSS.auto.arima.pred

plot(KSS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(28000,90000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Kansas Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(KSS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(KSS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(KSS.auto.arima.pred$fitted, KSS.ts), 3)

legend(1997,88000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 88000))
lines(c(2019.5,2019.5), c(0,88000))
text(2008.25, 90000, "Data Set")
text(2022, 90000, "Future")
arrows(1997.5, 88000, 2019.25, 88000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 88000, 2024, 88000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
