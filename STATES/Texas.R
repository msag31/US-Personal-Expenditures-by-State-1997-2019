library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
TXDG.ts <- ts(df$Texas.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
TXDG.ts
min(TXDG.ts)
max(TXDG.ts)

TXDG.auto.arima<-auto.arima(TXDG.ts)
summary(TXDG.auto.arima)
TXDG.auto.arima.pred <- forecast(TXDG.auto.arima, h = 5, level = 0)
TXDG.auto.arima.pred

plot(TXDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(53000,165000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Texas Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(TXDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(TXDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(TXDG.auto.arima.pred$fitted,TXDG.ts), 3)

legend(1997,160000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 160000))
lines(c(2019.5,2019.5), c(0,160000))
text(2008.25, 163000, "Data Set")
text(2022, 163000, "Future")
arrows(1997.5, 160000, 2019.25, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 160000, 2024, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
TXNDG.ts <- ts(df$Texas.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(TXNDG.ts)
max(TXNDG.ts)

TXNDG.auto.arima<-auto.arima(TXNDG.ts)
summary(TXNDG.auto.arima)
TXNDG.auto.arima.pred <- forecast(TXNDG.auto.arima, h = 5, level = 0)
TXNDG.auto.arima.pred

plot(TXNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(89000,310000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Texas Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(TXNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(TXNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(TXNDG.auto.arima.pred$fitted,TXNDG.ts), 3)

legend(1997,295000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 295000))
lines(c(2019.5,2019.5), c(0,295000))
text(2008.25, 305000, "Data Set")
text(2022, 305000, "Future")
arrows(1997.5, 295000, 2019.25, 295000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 295000, 2024, 295000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
TXS.ts <- ts(df$Texas.Services, start = c(1997), end=c(2019), freq = 1)
min(TXS.ts)
max(TXS.ts)

TXS.auto.arima<-auto.arima(TXS.ts)
summary(TXS.auto.arima)
TXS.auto.arima.pred <- forecast(TXS.auto.arima, h = 5, level = 0)
TXS.auto.arima.pred

plot(TXS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(239000,1050000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Texas Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(TXS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(TXS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(TXS.auto.arima.pred$fitted, TXS.ts), 3)

legend(1997,990500, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 990500))
lines(c(2019.5,2019.5), c(0,990500))
text(2008.25, 1030000, "Data Set")
text(2022, 1030000, "Future")
arrows(1997.5, 990500, 2019.25, 990500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 990500, 2024, 990500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
