library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
MIDG.ts <- ts(df$Michigan.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
MIDG.ts
min(MIDG.ts)
max(MIDG.ts)

MIDG.auto.arima<-auto.arima(MIDG.ts)
summary(MIDG.auto.arima)
MIDG.auto.arima.pred <- forecast(MIDG.auto.arima, h = 5, level = 0)
MIDG.auto.arima.pred

plot(MIDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(26000,55000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Michigan Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MIDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MIDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(MIDG.auto.arima.pred$fitted,MIDG.ts), 3)

legend(1997,52000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 52000))
lines(c(2019.5,2019.5), c(0,52000))
text(2008.25, 53000, "Data Set")
text(2022, 53000, "Future")
arrows(1997.5, 52000, 2019.25, 52000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 52000, 2024, 52000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
MINDG.ts <- ts(df$Michigan.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(MINDG.ts)
max(MINDG.ts)

MINDG.auto.arima<-auto.arima(MINDG.ts)
summary(MINDG.auto.arima)
MINDG.auto.arima.pred <- forecast(MINDG.auto.arima, h = 5, level = 0)
MINDG.auto.arima.pred

plot(MINDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(46000,105000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Michigan Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MINDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MINDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MINDG.auto.arima.pred$fitted,MINDG.ts), 3)

legend(1997,100000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 100000))
lines(c(2019.5,2019.5), c(0,100000))
text(2008.25, 102000, "Data Set")
text(2022, 102000, "Future")
arrows(1997.5, 100000, 2019.25, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 100000, 2024, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
MIS.ts <- ts(df$Michigan.Services, start = c(1997), end=c(2019), freq = 1)
min(MIS.ts)
max(MIS.ts)

MIS.auto.arima<-auto.arima(MIS.ts)
summary(MIS.auto.arima)
MIS.auto.arima.pred <- forecast(MIS.auto.arima, h = 5, level = 0)
MIS.auto.arima.pred

plot(MIS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(128000,335000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Michigan Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MIS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MIS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MIS.auto.arima.pred$fitted, MIS.ts), 3)

legend(1997,325000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 325000))
lines(c(2019.5,2019.5), c(0,325000))
text(2008.25, 335000, "Data Set")
text(2022, 335000, "Future")
arrows(1997.5, 325000, 2019.25, 325000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 325000, 2024, 325000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
