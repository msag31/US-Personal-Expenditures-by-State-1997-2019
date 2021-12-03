library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
VADG.ts <- ts(df$Virginia.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
VADG.ts
min(VADG.ts)
max(VADG.ts)

VADG.auto.arima<-auto.arima(VADG.ts)
summary(VADG.auto.arima)
VADG.auto.arima.pred <- forecast(VADG.auto.arima, h = 5, level = 0)
VADG.auto.arima.pred

plot(VADG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(18000,49000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Virginia Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(VADG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(VADG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(VADG.auto.arima.pred$fitted,VADG.ts), 3)

legend(1997,46500, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 46500))
lines(c(2019.5,2019.5), c(0,46500))
text(2008.25, 48000, "Data Set")
text(2022, 48000, "Future")
arrows(1997.5, 46500, 2019.25, 46500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 46500, 2024, 46500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
VANDG.ts <- ts(df$Virginia.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(VANDG.ts)
max(VANDG.ts)

VANDG.auto.arima<-auto.arima(VANDG.ts)
summary(VANDG.auto.arima)
VANDG.auto.arima.pred <- forecast(VANDG.auto.arima, h = 5, level = 0)
VANDG.auto.arima.pred

plot(VANDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(33000,91000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Virginia Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(VANDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(VANDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(VANDG.auto.arima.pred$fitted,VANDG.ts), 3)

legend(1997,88300, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 88300))
lines(c(2019.5,2019.5), c(0,88300))
text(2008.25, 90000, "Data Set")
text(2022, 90000, "Future")
arrows(1997.5, 88300, 2019.25, 88300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 88300, 2024, 88300, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
VAS.ts <- ts(df$Virginia.Services, start = c(1997), end=c(2019), freq = 1)
min(VAS.ts)
max(VAS.ts)

VAS.auto.arima<-auto.arima(VAS.ts)
summary(VAS.auto.arima)
VAS.auto.arima.pred <- forecast(VAS.auto.arima, h = 5, level = 0)
VAS.auto.arima.pred

plot(VAS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(84000,350000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Virginia Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(VAS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(VAS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(VAS.auto.arima.pred$fitted, VAS.ts), 3)

legend(1997,333300, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 333300))
lines(c(2019.5,2019.5), c(0,333300))
text(2008.25, 345000, "Data Set")
text(2022, 345000, "Future")
arrows(1997.5, 333300, 2019.25, 333300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 333300, 2024, 333300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
