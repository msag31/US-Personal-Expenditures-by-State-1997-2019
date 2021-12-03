library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
CADG.ts <- ts(df$California.Durable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
CADG.ts
max(CADG.ts)
min(CADG.ts)

plot(CADG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(80000,200000), main = "Value")

CADG.auto.arima<-auto.arima(CADG.ts)
summary(CADG.auto.arima)
CADG.auto.arima.pred <- forecast(CADG.auto.arima, h = 5, level = 0)
CADG.auto.arima.pred

plot(CADG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(80000,250000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "California Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(CADG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(CADG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(CADG.auto.arima.pred$fitted, CADG.ts), 3)

legend(1997,230000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 240000))
lines(c(2019.5,2019.5), c(0,240000))
text(2008.25, 245000, "Data Set")
text(2022, 245000, "Future")
arrows(1997.5, 240000, 2019.25, 240000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 240000, 2024, 240000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
CANDG.ts <- ts(df$California.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
max(CANDG.ts)
min(CANDG.ts)

plot(CANDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(135000,360000), main = "Value")

CANDG.auto.arima<-auto.arima(CANDG.ts)
summary(CANDG.auto.arima)
CANDG.auto.arima.pred <- forecast(CANDG.auto.arima, h = 5, level = 0)
CANDG.auto.arima.pred

plot(CANDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(135000,430000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "California Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(CANDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(CANDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(CANDG.auto.arima.pred$fitted, CANDG.ts), 3)

legend(1997,420000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 420000))
lines(c(2019.5,2019.5), c(0,420000))
text(2008.25, 430000, "Data Set")
text(2022, 430000, "Future")
arrows(1997.5, 420000, 2019.25, 420000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 420000, 2024, 420000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Services
CAS.ts <- ts(df$California.Services, start = c(1997), end=c(2019), freq = 1)
max(CAS.ts)
min(CAS.ts)

plot(CAS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(450000,1450000), main = "Value")

CAS.auto.arima<-auto.arima(CAS.ts)
summary(CAS.auto.arima)
CAS.auto.arima.pred <- forecast(CAS.auto.arima, h = 5, level = 0)
CAS.auto.arima.pred

plot(CAS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(450000,1900000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "California Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(CAS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(CAS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(CAS.auto.arima.pred$fitted, CAS.ts), 3)

legend(1997,1800000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 1800000))
lines(c(2019.5,2019.5), c(0,1800000))
text(2008.25, 1850000, "Data Set")
text(2022, 1850000, "Future")
arrows(1997.5, 1800000, 2019.25, 1800000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 1800000, 2024, 1800000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
