library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
NYDG.ts <- ts(df$New.York.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NYDG.ts)
max(NYDG.ts)

NYDG.auto.arima<-auto.arima(NYDG.ts)
summary(NYDG.auto.arima)
NYDG.auto.arima.pred <- forecast(NYDG.auto.arima, h = 5, level = 0)
NYDG.auto.arima.pred

plot(NYDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(37000,90000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New York Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NYDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NYDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(NYDG.auto.arima.pred$fitted,NYDG.ts), 3)

legend(1997,88000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 88000))
lines(c(2019.5,2019.5), c(0,88000))
text(2008.25, 89500, "Data Set")
text(2022, 89500, "Future")
arrows(1997.5, 88000, 2019.25, 88000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 88000, 2024, 88000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
NYNDG.ts <- ts(df$New.York.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NYNDG.ts)
max(NYNDG.ts)

NYNDG.auto.arima<-auto.arima(NYNDG.ts)
summary(NYNDG.auto.arima)
NYNDG.auto.arima.pred <- forecast(NYNDG.auto.arima, h = 5, level = 0)
NYNDG.auto.arima.pred

plot(NYNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(94000,250000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New York Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NYNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NYNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NYNDG.auto.arima.pred$fitted,NYNDG.ts), 3)

legend(1997,240000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 240000))
lines(c(2019.5,2019.5), c(0,240000))
text(2008.25, 250000, "Data Set")
text(2022, 250000, "Future")
arrows(1997.5, 240000, 2019.25, 240000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 240000, 2024, 240000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
NYS.ts <- ts(df$New.York.Services, start = c(1997), end=c(2019), freq = 1)
min(NYS.ts)
max(NYS.ts)

NYS.auto.arima<-auto.arima(NYS.ts)
summary(NYS.auto.arima)
NYS.auto.arima.pred <- forecast(NYS.auto.arima, h = 5, level = 0)
NYS.auto.arima.pred

plot(NYS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(283000,950000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New York Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NYS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NYS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NYS.auto.arima.pred$fitted, NYS.ts), 3)

legend(1997,920000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 920000))
lines(c(2019.5,2019.5), c(0,920000))
text(2008.25, 950000, "Data Set")
text(2022, 950000, "Future")
arrows(1997.5, 920000, 2019.25, 920000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 920000, 2024, 920000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
