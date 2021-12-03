library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
OKNG.ts <- ts(df$Oklahoma.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
OKNG.ts
min(OKNG.ts)
max(OKNG.ts)

OKNG.auto.arima<-auto.arima(OKNG.ts)
summary(OKNG.auto.arima)
OKNG.auto.arima.pred <- forecast(OKNG.auto.arima, h = 5, level = 0)
OKNG.auto.arima.pred

plot(OKNG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(8400,21000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Oklahoma Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(OKNG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(OKNG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(OKNG.auto.arima.pred$fitted,OKNG.ts), 3)

legend(1997,19500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 19500))
lines(c(2019.5,2019.5), c(0,19500))
text(2008.25, 20500, "Data Set")
text(2022, 20500, "Future")
arrows(1997.5, 19500, 2019.25, 19500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 19500, 2024, 19500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
OKNNG.ts <- ts(df$Oklahoma.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(OKNNG.ts)
max(OKNNG.ts)

OKNNG.auto.arima<-auto.arima(OKNNG.ts)
summary(OKNNG.auto.arima)
OKNNG.auto.arima.pred <- forecast(OKNNG.auto.arima, h = 5, level = 0)
OKNNG.auto.arima.pred

plot(OKNNG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(13500,39000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Oklahoma Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(OKNNG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(OKNNG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(OKNNG.auto.arima.pred$fitted,OKNNG.ts), 3)

legend(1997,37000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 37000))
lines(c(2019.5,2019.5), c(0,37000))
text(2008.25, 38000, "Data Set")
text(2022, 38000, "Future")
arrows(1997.5, 37000, 2019.25, 37000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 37000, 2024, 37000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
OKS.ts <- ts(df$Oklahoma.Services, start = c(1997), end=c(2019), freq = 1)
min(OKS.ts)
max(OKS.ts)

OKS.auto.arima<-auto.arima(OKS.ts)
summary(OKS.auto.arima)
OKS.auto.arima.pred <- forecast(OKS.auto.arima, h = 5, level = 0)
OKS.auto.arima.pred

plot(OKS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(32000,115000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Oklahoma Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(OKS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(OKS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(OKS.auto.arima.pred$fitted, OKS.ts), 3)

legend(1997,109000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 109000))
lines(c(2019.5,2019.5), c(0,109000))
text(2008.25, 114000, "Data Set")
text(2022, 114000, "Future")
arrows(1997.5, 109000, 2019.25, 109000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 109000, 2024, 109000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
