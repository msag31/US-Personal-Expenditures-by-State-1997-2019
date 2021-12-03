library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
NCDG.ts <- ts(df$North.Carolina.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NCDG.ts)
max(NCDG.ts)

NCDG.auto.arima<-auto.arima(NCDG.ts)
summary(NCDG.auto.arima)
NCDG.auto.arima.pred <- forecast(NCDG.auto.arima, h = 5, level = 0)
NCDG.auto.arima.pred

plot(NCDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(21000,57000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "North Carolina Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NCDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NCDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(NCDG.auto.arima.pred$fitted,NCDG.ts), 3)

legend(1997,55000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 55000))
lines(c(2019.5,2019.5), c(0,55000))
text(2008.25, 56000, "Data Set")
text(2022, 56000, "Future")
arrows(1997.5, 55000, 2019.25, 55000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 55000, 2024, 55000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
NCNDG.ts <- ts(df$North.Carolina.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(NCNDG.ts)
max(NCNDG.ts)

NCNDG.auto.arima<-auto.arima(NCNDG.ts)
summary(NCNDG.auto.arima)
NCNDG.auto.arima.pred <- forecast(NCNDG.auto.arima, h = 5, level = 0)
NCNDG.auto.arima.pred

plot(NCNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(36000,115000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "North Carolina Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NCNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NCNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NCNDG.auto.arima.pred$fitted,NCNDG.ts), 3)

legend(1997,108000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 108000))
lines(c(2019.5,2019.5), c(0,108000))
text(2008.25, 110000, "Data Set")
text(2022, 110000, "Future")
arrows(1997.5, 108000, 2019.25, 108000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 108000, 2024, 108000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
NCS.ts <- ts(df$North.Carolina.Services, start = c(1997), end=c(2019), freq = 1)
min(NCS.ts)
max(NCS.ts)

NCS.auto.arima<-auto.arima(NCS.ts)
summary(NCS.auto.arima)
NCS.auto.arima.pred <- forecast(NCS.auto.arima, h = 5, level = 0)
NCS.auto.arima.pred

plot(NCS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(81000,330000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "North Carolina Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(NCS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(NCS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(NCS.auto.arima.pred$fitted, NCS.ts), 3)

legend(1997,313000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 313000))
lines(c(2019.5,2019.5), c(0,313000))
text(2008.25, 330000, "Data Set")
text(2022, 330000, "Future")
arrows(1997.5, 313000, 2019.25, 313000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 313000, 2024, 313000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
