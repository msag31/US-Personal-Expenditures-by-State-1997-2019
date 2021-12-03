library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
MADG.ts <- ts(df$Massachusetts.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
MADG.ts
min(MADG.ts)
max(MADG.ts)

MADG.auto.arima<-auto.arima(MADG.ts)
summary(MADG.auto.arima)
MADG.auto.arima.pred <- forecast(MADG.auto.arima, h = 5, level = 0)
MADG.auto.arima.pred

plot(MADG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(18000,46000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Massachusetts Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MADG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MADG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(MADG.auto.arima.pred$fitted,MADG.ts), 3)

legend(1997,44000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 44000))
lines(c(2019.5,2019.5), c(0,44000))
text(2008.25, 45000, "Data Set")
text(2022, 45000, "Future")
arrows(1997.5, 44000, 2019.25, 44000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 44000, 2024, 44000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
MANDG.ts <- ts(df$Massachusetts.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(MANDG.ts)
max(MANDG.ts)

MANDG.auto.arima<-auto.arima(MANDG.ts)
summary(MANDG.auto.arima)
MANDG.auto.arima.pred <- forecast(MANDG.auto.arima, h = 5, level = 0)
MANDG.auto.arima.pred

plot(MANDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(34000,85000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Massachusetts Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MANDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MANDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MANDG.auto.arima.pred$fitted,MANDG.ts), 3)

legend(1997,82000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 82000))
lines(c(2019.5,2019.5), c(0,82000))
text(2008.25, 84000, "Data Set")
text(2022, 84000, "Future")
arrows(1997.5, 82000, 2019.25, 82000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 82000, 2024, 82000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
MAS.ts <- ts(df$Massachusetts.Services, start = c(1997), end=c(2019), freq = 1)
min(MAS.ts)
max(MAS.ts)

MAS.auto.arima<-auto.arima(MAS.ts)
summary(MAS.auto.arima)
MAS.auto.arima.pred <- forecast(MAS.auto.arima, h = 5, level = 0)
MAS.auto.arima.pred

plot(MAS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(100000,330000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Massachusetts Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(MAS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(MAS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(MAS.auto.arima.pred$fitted, MAS.ts), 3)

legend(1997,320000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 320000))
lines(c(2019.5,2019.5), c(0,320000))
text(2008.25, 330000, "Data Set")
text(2022, 330000, "Future")
arrows(1997.5, 320000, 2019.25, 320000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 320000, 2024, 320000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
