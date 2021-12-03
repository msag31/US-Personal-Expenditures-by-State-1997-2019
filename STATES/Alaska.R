library(forecast)
library(zoo)

#Durable Goods
setwd("~//Desktop/CSUEB/Capstone")
df<-read.csv("capstone.csv")
head(df)
AKDG.ts <- ts(df$Alaska.Durable.Goods, start = c(1997,1), end=c(2019,1), freq = 1)
AKDG.ts
max(AKDG.ts)
min(AKDG.ts)

plot(AKDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(1500,3500), main = "Value")

AKDG.auto.arima<-auto.arima(AKDG.ts)
summary(AKDG.auto.arima)
AKDG.auto.arima.pred <- forecast(AKDG.auto.arima, h = 5, level = 0)
AKDG.auto.arima.pred

plot(AKDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(1500,4700), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Alaska Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(AKDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(AKDG.ts, col = "black", lwd = 2, lty = 1)
round(accuracy(AKDG.auto.arima.pred$fitted, AKDG.ts), 3)

legend(1997,4500, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 4600))
lines(c(2019.5,2019.5), c(0,4600))
text(2008.25, 4700, "Data Set")
text(2022, 4700, "Future")
arrows(1997.5, 4600, 2019.25, 4600, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 4600, 2024, 4600, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#Nondurable goods
AKNDG.ts <- ts(df$Alaska.Nondurable.goods, start = c(1997,1), end=c(2019,1), freq = 1)
max(AKNDG.ts)
min(AKNDG.ts)

plot(AKNDG.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(3200,6800), main = "Value")

AKNDG.auto.arima<-auto.arima(AKNDG.ts)
summary(AKNDG.auto.arima)
AKNDG.auto.arima.pred <- forecast(AKNDG.auto.arima, h = 5, level = 0)
AKNDG.auto.arima.pred

plot(AKNDG.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(3200,8000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Alaska Non-Durable Goods", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(AKNDG.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(AKNDG.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(AKNDG.auto.arima.pred$fitted, AKNDG.ts), 3)

legend(1997,7700, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 7700))
lines(c(2019.5,2019.5), c(0,7700))
text(2008.25, 7850, "Data Set")
text(2022, 7850, "Future")
arrows(1997.5, 7700, 2019.25, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 7700, 2024, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)




#Services
AKS.ts <- ts(df$Alaska.Services, start = c(1997,1), end=c(2019,1), freq = 1)
max(AKS.ts)
min(AKS.ts)

plot(AKS.ts,
     xlab = "Year", ylab = "Expenses (in Millions)",
     ylim = c(8500,30000), main = "Value")

AKS.auto.arima<-auto.arima(AKS.ts)
summary(AKS.auto.arima)
AKS.auto.arima.pred <- forecast(AKS.auto.arima, h = 5, level = 0)
AKS.auto.arima.pred

plot(AKS.auto.arima.pred, 
     xlab = "Year", ylab = "Expenses (in Millions)", ylim = c(8500,34000), bty = "l",
     xaxt = "n", xlim = c(1997, 2025), 
     main = "Alaska Services", lwd = 2, flty = 2) 
axis(1, at = seq(1997, 2025, 1), labels = format(seq(1997, 2025, 1)))
lines(AKS.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(AKS.ts, col = "black", lwd = 2, lty = 1)

round(accuracy(AKS.auto.arima.pred$fitted, AKS.ts), 3)

legend(1997,32000, legend = c("Time Series", 
                              "Forecast", 
                              "Forecast for 5 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1997, 1997), c(0, 32000))
lines(c(2019.5,2019.5), c(0,32000))
text(2008.25, 33000, "Data Set")
text(2022, 33000, "Future")
arrows(1997.5, 32000, 2019.25, 32000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.75, 32000, 2024, 32000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
