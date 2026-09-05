raw_data <- scan("~/TCC/dados/Scripts/airline.txt")
length(raw_data)
x <- raw_data[1:144]
airline <- ts(x, start = c(1949,1), frequency = 12)   # frequencia 12 = dados mensais
summary(airline)

plot(airline, main = "Passageiros de linha aerea",
     xlab = "Ano", ylab = "Passageiros (mil)", col = "steelblue")

par(mfrow = c(1,2))
acf(airline,  lag.max = 60)
pacf(airline, lag.max = 60)
par(mfrow = c(1,1))

library(tseries)
adf.test(airline)
pp.test(airline)
kpss.test(airline, null = "Level")
kpss.test(airline, null = "Trend")

library(trend)
tempo <- as.numeric(time(airline))
summary(lm(as.numeric(airline) ~ tempo))
mk.test(as.numeric(airline))
cs.test(as.numeric(airline))

# Sazonalidade
plot(decompose(airline))
plot(stl(airline, "per"))
library(forecast)
nsdiffs(airline)
ndiffs(airline)
