library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
MSDG.ts <- ts(df$Mississippi.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
MSDG.ts
min(MSDG.ts)
max(MSDG.ts)

MSDG.auto.arima<-auto.arima(MSDG.ts)
summary(MSDG.auto.arima)
MSDG.auto.arima.pred <- forecast(MSDG.auto.arima, h = 5, level = 0)
MSDG.auto.arima.pred

plot(MSDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(5500,15000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Mississippi Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MSDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MSDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(MSDG.auto.arima.pred$fitted,MSDG.ts), 3)

legend(1997,13000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 13000))
lines(c(2019.5,2019.5), c(0,13000))
text(2008.25, 13500, "Data Set")
text(2022, 13500, "Future")
arrows(1997.5, 13000, 2019.25, 13000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 13000, 2024, 13000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
MSNDG.ts <- ts(df$Mississippi.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(MSNDG.ts)
max(MSNDG.ts)

MSNDG.auto.arima<-auto.arima(MSNDG.ts)
summary(MSNDG.auto.arima)
MSNDG.auto.arima.pred <- forecast(MSNDG.auto.arima, h = 5, level = 0)
MSNDG.auto.arima.pred

plot(MSNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(11000,30000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Mississippi Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MSNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MSNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MSNDG.auto.arima.pred$fitted,MSNDG.ts), 3)

legend(1997,28000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 28000))
lines(c(2019.5,2019.5), c(0,28000))
text(2008.25, 29000, "Data Set")
text(2022, 29000, "Future")
arrows(1997.5, 28000, 2019.25, 28000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 28000, 2024, 28000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
MSS.ts <- ts(df$Mississippi.Services, start = c(1997), end=c(2019), freq = 1)
min(MSS.ts)
max(MSS.ts)

MSS.auto.arima<-auto.arima(MSS.ts)
summary(MSS.auto.arima)
MSS.auto.arima.pred <- forecast(MSS.auto.arima, h = 5, level = 0)
MSS.auto.arima.pred

plot(MSS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(23000,75000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Mississippi Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MSS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MSS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MSS.auto.arima.pred$fitted, MSS.ts), 3)

legend(1997,71000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 71000))
lines(c(2019.5,2019.5), c(0,71000))
text(2008.25, 73000, "Data Set")
text(2022, 73000, "Future")
arrows(1997.5, 71000, 2019.25, 71000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 71000, 2024, 71000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
