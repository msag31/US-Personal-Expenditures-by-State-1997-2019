library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
MTDG.ts <- ts(df$Montana.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
MTDG.ts
min(MTDG.ts)
max(MTDG.ts)

MTDG.auto.arima<-auto.arima(MTDG.ts)
summary(MTDG.auto.arima)
MTDG.auto.arima.pred <- forecast(MTDG.auto.arima, h = 5, level = 0)
MTDG.auto.arima.pred

plot(MTDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(2200,7400), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Montana Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MTDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MTDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(MTDG.auto.arima.pred$fitted,MTDG.ts), 3)

legend(1997,7100, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 7100))
lines(c(2019.5,2019.5), c(0,7100))
text(2008.25, 7300, "Data Set")
text(2022, 7300, "Future")
arrows(1997.5, 7100, 2019.25, 7100, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 7100, 2024, 7100, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
MTNDG.ts <- ts(df$Montana.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(MTNDG.ts)
max(MTNDG.ts)

MTNDG.auto.arima<-auto.arima(MTNDG.ts)
summary(MTNDG.auto.arima)
MTNDG.auto.arima.pred <- forecast(MTNDG.auto.arima, h = 5, level = 0)
MTNDG.auto.arima.pred

plot(MTNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3800,13000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Montana Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MTNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MTNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MTNDG.auto.arima.pred$fitted,MTNDG.ts), 3)

legend(1997,12000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 12000))
lines(c(2019.5,2019.5), c(0,12000))
text(2008.25, 12500, "Data Set")
text(2022, 12500, "Future")
arrows(1997.5, 12000, 2019.25, 12000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 12000, 2024, 12000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
MTS.ts <- ts(df$Montana.Services, start = c(1997), end=c(2019), freq = 1)
min(MTS.ts)
max(MTS.ts)

MTS.auto.arima<-auto.arima(MTS.ts)
summary(MTS.auto.arima)
MTS.auto.arima.pred <- forecast(MTS.auto.arima, h = 5, level = 0)
MTS.auto.arima.pred

plot(MTS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(9400,40000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Montana Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MTS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MTS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MTS.auto.arima.pred$fitted, MTS.ts), 3)

legend(1997,38000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 38000))
lines(c(2019.5,2019.5), c(0,38000))
text(2008.25, 39000, "Data Set")
text(2022, 39000, "Future")
arrows(1997.5, 38000, 2019.25, 38000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 38000, 2024, 38000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
