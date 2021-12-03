library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
SCDG.ts <- ts(df$South.Carolina.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(SCDG.ts)
max(SCDG.ts)

SCDG.auto.arima<-auto.arima(SCDG.ts)
summary(SCDG.auto.arima)
SCDG.auto.arima.pred <- forecast(SCDG.auto.arima, h = 5, level = 0)
SCDG.auto.arima.pred

plot(SCDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(9200,25000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "South Carolina Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(SCDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(SCDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(SCDG.auto.arima.pred$fitted,SCDG.ts), 3)

legend(1997,24200, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 24200))
lines(c(2019.5,2019.5), c(0,24200))
text(2008.25, 25000, "Data Set")
text(2022, 25000, "Future")
arrows(1997.5, 24200, 2019.25, 24200, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 24200, 2024, 24200, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
SCNDG.ts <- ts(df$South.Carolina.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(SCNDG.ts)
max(SCNDG.ts)

SCNDG.auto.arima<-auto.arima(SCNDG.ts)
summary(SCNDG.auto.arima)
SCNDG.auto.arima.pred <- forecast(SCNDG.auto.arima, h = 5, level = 0)
SCNDG.auto.arima.pred

plot(SCNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(17500,57000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "South Carolina Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(SCNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(SCNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(SCNDG.auto.arima.pred$fitted,SCNDG.ts), 3)

legend(1997,54000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 54000))
lines(c(2019.5,2019.5), c(0,54000))
text(2008.25, 56000, "Data Set")
text(2022, 56000, "Future")
arrows(1997.5, 54000, 2019.25, 54000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 54000, 2024, 54000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
SCS.ts <- ts(df$South.Carolina.Services, start = c(1997), end=c(2019), freq = 1)
min(SCS.ts)
max(SCS.ts)

SCS.auto.arima<-auto.arima(SCS.ts)
summary(SCS.auto.arima)
SCS.auto.arima.pred <- forecast(SCS.auto.arima, h = 5, level = 0)
SCS.auto.arima.pred

plot(SCS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(39000,150000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "South Carolina Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(SCS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(SCS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(SCS.auto.arima.pred$fitted, SCS.ts), 3)

legend(1997,144000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 144000))
lines(c(2019.5,2019.5), c(0,144000))
text(2008.25, 149000, "Data Set")
text(2022, 149000, "Future")
arrows(1997.5, 144000, 2019.25, 144000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 144000, 2024, 144000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
