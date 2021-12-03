library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
MODG.ts <- ts(df$Missouri.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
MODG.ts
min(MODG.ts)
max(MODG.ts)

MODG.auto.arima<-auto.arima(MODG.ts)
summary(MODG.auto.arima)
MODG.auto.arima.pred <- forecast(MODG.auto.arima, h = 5, level = 0)
MODG.auto.arima.pred

plot(MODG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(15000,32000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Missouri Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MODG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MODG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(MODG.auto.arima.pred$fitted,MODG.ts), 3)

legend(1997,30000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 30000))
lines(c(2019.5,2019.5), c(0,30000))
text(2008.25, 31000, "Data Set")
text(2022, 31000, "Future")
arrows(1997.5, 30000, 2019.25, 30000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 30000, 2024, 30000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
MONDG.ts <- ts(df$Missouri.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(MONDG.ts)
max(MONDG.ts)

MONDG.auto.arima<-auto.arima(MONDG.ts)
summary(MONDG.auto.arima)
MONDG.auto.arima.pred <- forecast(MONDG.auto.arima, h = 5, level = 0)
MONDG.auto.arima.pred

plot(MONDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(29000,65000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Missouri Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MONDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MONDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MONDG.auto.arima.pred$fitted,MONDG.ts), 3)

legend(1997,63000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 63000))
lines(c(2019.5,2019.5), c(0,63000))
text(2008.25, 64000, "Data Set")
text(2022, 64000, "Future")
arrows(1997.5, 63000, 2019.25, 63000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 63000, 2024, 63000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
MOS.ts <- ts(df$Missouri.Services, start = c(1997), end=c(2019), freq = 1)
min(MOS.ts)
max(MOS.ts)

MOS.auto.arima<-auto.arima(MOS.ts)
summary(MOS.auto.arima)
MOS.auto.arima.pred <- forecast(MOS.auto.arima, h = 5, level = 0)
MOS.auto.arima.pred

plot(MOS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(65000,205000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Missouri Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MOS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MOS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MOS.auto.arima.pred$fitted, MOS.ts), 3)

legend(1997,200000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 200000))
lines(c(2019.5,2019.5), c(0,200000))
text(2008.25, 205000, "Data Set")
text(2022, 205000, "Future")
arrows(1997.5, 200000, 2019.25, 200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 200000, 2024, 200000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
