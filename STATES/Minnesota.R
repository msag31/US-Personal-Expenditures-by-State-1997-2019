library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
MNDG.ts <- ts(df$Minnesota.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
MNDG.ts
min(MNDG.ts)
max(MNDG.ts)

MNDG.auto.arima<-auto.arima(MNDG.ts)
summary(MNDG.auto.arima)
MNDG.auto.arima.pred <- forecast(MNDG.auto.arima, h = 5, level = 0)
MNDG.auto.arima.pred

plot(MNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(15000,40000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Minnesota Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MNDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(MNDG.auto.arima.pred$fitted,MNDG.ts), 3)

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


#Nondurable goods
MNNDG.ts <- ts(df$Minnesota.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(MNNDG.ts)
max(MNNDG.ts)

MNNDG.auto.arima<-auto.arima(MNNDG.ts)
summary(MNNDG.auto.arima)
MNNDG.auto.arima.pred <- forecast(MNNDG.auto.arima, h = 5, level = 0)
MNNDG.auto.arima.pred

plot(MNNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(22500,55000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Minnesota Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MNNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MNNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MNNDG.auto.arima.pred$fitted,MNNDG.ts), 3)

legend(1997,54000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 54000))
lines(c(2019.5,2019.5), c(0,54000))
text(2008.25, 55000, "Data Set")
text(2022, 55000, "Future")
arrows(1997.5, 54000, 2019.25, 54000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 54000, 2024, 54000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
MNS.ts <- ts(df$Minnesota.Services, start = c(1997), end=c(2019), freq = 1)
min(MNS.ts)
max(MNS.ts)

MNS.auto.arima<-auto.arima(MNS.ts)
summary(MNS.auto.arima)
MNS.auto.arima.pred <- forecast(MNS.auto.arima, h = 5, level = 0)
MNS.auto.arima.pred

plot(MNS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(69000,225000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Minnesota Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MNS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MNS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MNS.auto.arima.pred$fitted, MNS.ts), 3)

legend(1997,217000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 217000))
lines(c(2019.5,2019.5), c(0,217000))
text(2008.25, 225000, "Data Set")
text(2022, 225000, "Future")
arrows(1997.5, 217000, 2019.25, 217000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 217000, 2024, 217000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
