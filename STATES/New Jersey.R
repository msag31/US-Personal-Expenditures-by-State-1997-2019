library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
NJDG.ts <- ts(df$New.Jersey.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NJDG.ts)
max(NJDG.ts)

NJDG.auto.arima<-auto.arima(NJDG.ts)
summary(NJDG.auto.arima)
NJDG.auto.arima.pred <- forecast(NJDG.auto.arima, h = 5, level = 0)
NJDG.auto.arima.pred

plot(NJDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(23000,53000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New Jersey Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NJDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NJDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(NJDG.auto.arima.pred$fitted,NJDG.ts), 3)

legend(1997,51500, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 51500))
lines(c(2019.5,2019.5), c(0,51500))
text(2008.25, 52500, "Data Set")
text(2022, 52500, "Future")
arrows(1997.5, 51500, 2019.25, 51500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 51500, 2024, 51500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
NJNDG.ts <- ts(df$New.Jersey.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NJNDG.ts)
max(NJNDG.ts)

NJNDG.auto.arima<-auto.arima(NJNDG.ts)
summary(NJNDG.auto.arima)
NJNDG.auto.arima.pred <- forecast(NJNDG.auto.arima, h = 5, level = 0)
NJNDG.auto.arima.pred

plot(NJNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(44000,110000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New Jersey Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NJNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NJNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NJNDG.auto.arima.pred$fitted,NJNDG.ts), 3)

legend(1997,103000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 103000))
lines(c(2019.5,2019.5), c(0,103000))
text(2008.25, 105000, "Data Set")
text(2022, 105000, "Future")
arrows(1997.5, 103000, 2019.25, 103000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 103000, 2024, 103000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
NJS.ts <- ts(df$New.Jersey.Services, start = c(1997), end=c(2019), freq = 1)
min(NJS.ts)
max(NJS.ts)

NJS.auto.arima<-auto.arima(NJS.ts)
summary(NJS.auto.arima)
NJS.auto.arima.pred <- forecast(NJS.auto.arima, h = 5, level = 0)
NJS.auto.arima.pred

plot(NJS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(130000,420000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "New Jersey Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NJS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NJS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NJS.auto.arima.pred$fitted, NJS.ts), 3)

legend(1997,405000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 405000))
lines(c(2019.5,2019.5), c(0,405000))
text(2008.25, 415000, "Data Set")
text(2022, 415000, "Future")
arrows(1997.5, 405000, 2019.25, 405000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 405000, 2024, 405000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
