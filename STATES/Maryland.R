library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
MDDG.ts <- ts(df$Maryland.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
MDDG.ts
min(MDDG.ts)
max(MDDG.ts)

MDDG.auto.arima<-auto.arima(MDDG.ts)
summary(MDDG.auto.arima)
MDDG.auto.arima.pred <- forecast(MDDG.auto.arima, h = 5, level = 0)
MDDG.auto.arima.pred

plot(MDDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(14000,33000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Maryland Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MDDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MDDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(MDDG.auto.arima.pred$fitted,MDDG.ts), 3)

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


#Nondurable goods
MDNDG.ts <- ts(df$Maryland.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(MDNDG.ts)
max(MDNDG.ts)

MDNDG.auto.arima<-auto.arima(MDNDG.ts)
summary(MDNDG.auto.arima)
MDNDG.auto.arima.pred <- forecast(MDNDG.auto.arima, h = 5, level = 0)
MDNDG.auto.arima.pred

plot(MDNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(25000,63000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Maryland Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MDNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MDNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MDNDG.auto.arima.pred$fitted,MDNDG.ts), 3)

legend(1997,61000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 61000))
lines(c(2019.5,2019.5), c(0,61000))
text(2008.25, 62000, "Data Set")
text(2022, 62000, "Future")
arrows(1997.5, 61000, 2019.25, 61000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 61000, 2024, 61000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
MDS.ts <- ts(df$Maryland.Services, start = c(1997), end=c(2019), freq = 1)
min(MDS.ts)
max(MDS.ts)

MDS.auto.arima<-auto.arima(MDS.ts)
summary(MDS.auto.arima)
MDS.auto.arima.pred <- forecast(MDS.auto.arima, h = 5, level = 0)
MDS.auto.arima.pred

plot(MDS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(72000,248000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Maryland Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MDS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MDS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MDS.auto.arima.pred$fitted, MDS.ts), 3)

legend(1997,239000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 238000))
lines(c(2019.5,2019.5), c(0,239000))
text(2008.25, 247000, "Data Set")
text(2022, 247000, "Future")
arrows(1997.5, 239000, 2019.25, 239000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 239000, 2024, 239000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
