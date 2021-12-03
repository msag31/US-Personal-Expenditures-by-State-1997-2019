library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
WADG.ts <- ts(df$Washington.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
WADG.ts
min(WADG.ts)
max(WADG.ts)

WADG.auto.arima<-auto.arima(WADG.ts)
summary(WADG.auto.arima)
WADG.auto.arima.pred <- forecast(WADG.auto.arima, h = 5, level = 0)
WADG.auto.arima.pred

plot(WADG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(15000,56000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Washington Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WADG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WADG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(WADG.auto.arima.pred$fitted,WADG.ts), 3)

legend(1997,53000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 53000))
lines(c(2019.5,2019.5), c(0,53000))
text(2008.25, 55000, "Data Set")
text(2022, 55000, "Future")
arrows(1997.5, 53000, 2019.25, 53000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 53000, 2024, 53000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
WANDG.ts <- ts(df$Washington.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(WANDG.ts)
max(WANDG.ts)

WANDG.auto.arima<-auto.arima(WANDG.ts)
summary(WANDG.auto.arima)
WANDG.auto.arima.pred <- forecast(WANDG.auto.arima, h = 5, level = 0)
WANDG.auto.arima.pred

plot(WANDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(26500,77000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Washington Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WANDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WANDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(WANDG.auto.arima.pred$fitted,WANDG.ts), 3)

legend(1997,74000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 74000))
lines(c(2019.5,2019.5), c(0,74000))
text(2008.25, 76000, "Data Set")
text(2022, 76000, "Future")
arrows(1997.5, 74000, 2019.25, 74000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 74000, 2024, 74000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
WAS.ts <- ts(df$Washington.Services, start = c(1997), end=c(2019), freq = 1)
min(WAS.ts)
max(WAS.ts)

WAS.auto.arima<-auto.arima(WAS.ts)
summary(WAS.auto.arima)
WAS.auto.arima.pred <- forecast(WAS.auto.arima, h = 5, level = 0)
WAS.auto.arima.pred

plot(WAS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(77500,360000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Washington Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(WAS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(WAS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(WAS.auto.arima.pred$fitted, WAS.ts), 3)

legend(1997,340000, legend = c("Time Series", 
                               "Forecast", 
                               "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 340000))
lines(c(2019.5,2019.5), c(0,340000))
text(2008.25, 355000, "Data Set")
text(2022, 355000, "Future")
arrows(1997.5, 340000, 2019.25, 340000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 340000, 2024, 340000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
