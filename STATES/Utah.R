library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
UTDG.ts <- ts(df$Utah.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
UTDG.ts
min(UTDG.ts)
max(UTDG.ts)

UTDG.auto.arima<-auto.arima(UTDG.ts)
summary(UTDG.auto.arima)
UTDG.auto.arima.pred <- forecast(UTDG.auto.arima, h = 5, level = 0)
UTDG.auto.arima.pred

plot(UTDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(5900,24000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Utah Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(UTDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(UTDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(UTDG.auto.arima.pred$fitted,UTDG.ts), 3)

legend(1997,22000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 22000))
lines(c(2019.5,2019.5), c(0,22000))
text(2008.25, 23000, "Data Set")
text(2022, 23000, "Future")
arrows(1997.5, 22000, 2019.25, 22000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 22000, 2024, 22000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
UTNDG.ts <- ts(df$Utah.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(UTNDG.ts)
max(UTNDG.ts)

UTNDG.auto.arima<-auto.arima(UTNDG.ts)
summary(UTNDG.auto.arima)
UTNDG.auto.arima.pred <- forecast(UTNDG.auto.arima, h = 5, level = 0)
UTNDG.auto.arima.pred

plot(UTNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(9000,33000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Utah Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(UTNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(UTNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(UTNDG.auto.arima.pred$fitted,UTNDG.ts), 3)

legend(1997,31000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 31000))
lines(c(2019.5,2019.5), c(0,31000))
text(2008.25, 32000, "Data Set")
text(2022, 32000, "Future")
arrows(1997.5, 31000, 2019.25, 31000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 31000, 2024, 31000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
UTS.ts <- ts(df$Utah.Services, start = c(1997), end=c(2019), freq = 1)
min(UTS.ts)
max(UTS.ts)

UTS.auto.arima<-auto.arima(UTS.ts)
summary(UTS.auto.arima)
UTS.auto.arima.pred <- forecast(UTS.auto.arima, h = 5, level = 0)
UTS.auto.arima.pred

plot(UTS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(22000,115000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Utah Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(UTS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(UTS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(UTS.auto.arima.pred$fitted, UTS.ts), 3)

legend(1997,109000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 109000))
lines(c(2019.5,2019.5), c(0,109000))
text(2008.25, 115000, "Data Set")
text(2022, 115000, "Future")
arrows(1997.5, 109000, 2019.25, 109000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 109000, 2024, 109000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
