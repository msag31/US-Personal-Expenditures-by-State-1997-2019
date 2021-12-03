library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
FLDG.ts <- ts(df$Florida.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
FLDG.ts
min(FLDG.ts)
max(FLDG.ts)

plot(FLDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(,3700), main = "Value")

FLDG.auto.arima<-auto.arima(FLDG.ts)
summary(FLDG.auto.arima)
FLDG.auto.arima.pred <- forecast(FLDG.auto.arima, h = 5, level = 0)
FLDG.auto.arima.pred

plot(FLDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(44000,140000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Florida Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(FLDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(FLDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(FLDG.auto.arima.pred$fitted,FLDG.ts), 3)

legend(1997,135000, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 135000))
lines(c(2019.5,2019.5), c(0,135000))
text(2008.25, 140000, "Data Set")
text(2022, 140000, "Future")
arrows(1997.5,135000, 2019.25, 135000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 135000, 2024, 135000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
FLNDG.ts <- ts(df$Florida.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
min(FLNDG.ts)
max(FLNDG.ts)

plot(FLNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(70000,200000), main = "Value")

FLNDG.auto.arima<-auto.arima(FLNDG.ts)
summary(FLNDG.auto.arima)
FLNDG.auto.arima.pred <- forecast(FLNDG.auto.arima, h = 5, level = 0)
FLNDG.auto.arima.pred

plot(FLNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(70000,230000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Florida Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(FLNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(FLNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(FLNDG.auto.arima.pred$fitted,FLNDG.ts), 3)

legend(1997,225000, legend = c("Time Series", 
                             "Forecast", 
                             "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 225000))
lines(c(2019.5,2019.5), c(0,225000))
text(2008.25, 230000, "Data Set")
text(2022, 230000, "Future")
arrows(1997.5, 225000, 2019.25, 225000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 225000, 2024, 225000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
FLS.ts <- ts(df$Florida.Services, start = c(1997), end=c(2019), freq = 1)
min(FLS.ts)
max(FLS.ts)

plot(FLS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(195000,620000), main = "Value")

FLS.auto.arima<-auto.arima(FLS.ts)
summary(FLS.auto.arima)
FLS.auto.arima.pred <- forecast(FLS.auto.arima, h = 5, level = 0)
FLS.auto.arima.pred

plot(FLS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(195000,770000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Florida Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(FLS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(FLS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(FLS.auto.arima.pred$fitted, FLS.ts), 3)

legend(1997,740000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 740000))
lines(c(2019.5,2019.5), c(0,740000))
text(2008.25, 760000, "Data Set")
text(2022, 760000, "Future")
arrows(1997.5, 740000, 2019.25, 740000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 740000, 2024, 740000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
