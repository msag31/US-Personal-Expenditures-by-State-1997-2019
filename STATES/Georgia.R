library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
GADG.ts <- ts(df$Georgia.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
GADG.ts
min(GADG.ts)
max(GADG.ts)

plot(GADG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(20000,48000), main = "Value")

GADG.auto.arima<-auto.arima(GADG.ts)
summary(GADG.auto.arima)
GADG.auto.arima.pred <- forecast(GADG.auto.arima, h = 5, level = 0)
GADG.auto.arima.pred

plot(GADG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(20000,54500), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Georgia Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(GADG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(GADG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(GADG.auto.arima.pred$fitted,GADG.ts), 3)

legend(1997,53500, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 53500))
lines(c(2019.5,2019.5), c(0,53500))
text(2008.25, 54500, "Data Set")
text(2022, 54500, "Future")
arrows(1997.5,53500, 2019.25, 53500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 53500, 2024, 53500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
GANDG.ts <- ts(df$Georgia.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(GANDG.ts)
max(GANDG.ts)

plot(GANDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(36000,92000), main = "Value")

GANDG.auto.arima<-auto.arima(GANDG.ts)
summary(GANDG.auto.arima)
GANDG.auto.arima.pred <- forecast(GANDG.auto.arima, h = 5, level = 0)
GANDG.auto.arima.pred

plot(GANDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(36000,106000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Georgia Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(GANDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(GANDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(GANDG.auto.arima.pred$fitted,GANDG.ts), 3)

legend(1997,105000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 105000))
lines(c(2019.5,2019.5), c(0,105000))
text(2008.25, 107000, "Data Set")
text(2022, 107000, "Future")
arrows(1997.5, 105000, 2019.25, 105000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 105000, 2024, 105000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
GAS.ts <- ts(df$Georgia.Services, start = c(1997), end=c(2019), freq = 1)
min(GAS.ts)
max(GAS.ts)

plot(GAS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(85000,270000), main = "Value")

GAS.auto.arima<-auto.arima(GAS.ts)
summary(GAS.auto.arima)
GAS.auto.arima.pred <- forecast(GAS.auto.arima, h = 5, level = 0)
GAS.auto.arima.pred

plot(GAS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(85000,335000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Georgia Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(GAS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(GAS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(GAS.auto.arima.pred$fitted, GAS.ts), 3)

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
