library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
NVDG.ts <- ts(df$Nevada.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
NVDG.ts
min(NVDG.ts)
max(NVDG.ts)

NVDG.auto.arima<-auto.arima(NVDG.ts)
summary(NVDG.auto.arima)
NVDG.auto.arima.pred <- forecast(NVDG.auto.arima, h = 5, level = 0)
NVDG.auto.arima.pred

plot(NVDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(5000,18000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Nevada Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NVDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NVDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(NVDG.auto.arima.pred$fitted,NVDG.ts), 3)

legend(1997,16500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 16500))
lines(c(2019.5,2019.5), c(0,16500))
text(2008.25, 17000, "Data Set")
text(2022, 17000, "Future")
arrows(1997.5, 16500, 2019.25, 16500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 16500, 2024, 16500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
NVNDG.ts <- ts(df$Nevada.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NVNDG.ts)
max(NVNDG.ts)

NVNDG.auto.arima<-auto.arima(NVNDG.ts)
summary(NVNDG.auto.arima)
NVNDG.auto.arima.pred <- forecast(NVNDG.auto.arima, h = 5, level = 0)
NVNDG.auto.arima.pred

plot(NVNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(8700,35000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Nevada Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NVNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NVNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NVNDG.auto.arima.pred$fitted,NVNDG.ts), 3)

legend(1997,33500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 33500))
lines(c(2019.5,2019.5), c(0,33500))
text(2008.25, 34500, "Data Set")
text(2022, 34500, "Future")
arrows(1997.5, 33500, 2019.25, 33500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 33500, 2024, 33500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
NVS.ts <- ts(df$Nevada.Services, start = c(1997), end=c(2019), freq = 1)
min(NVS.ts)
max(NVS.ts)

NVS.auto.arima<-auto.arima(NVS.ts)
summary(NVS.auto.arima)
NVS.auto.arima.pred <- forecast(NVS.auto.arima, h = 5, level = 0)
NVS.auto.arima.pred

plot(NVS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(22000,110000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Nevada Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NVS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NVS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NVS.auto.arima.pred$fitted, NVS.ts), 3)

legend(1997,106000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 106000))
lines(c(2019.5,2019.5), c(0,106000))
text(2008.25, 110000, "Data Set")
text(2022, 110000, "Future")
arrows(1997.5, 106000, 2019.25, 106000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 106000, 2024, 106000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
