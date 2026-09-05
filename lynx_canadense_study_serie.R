raw_data <- read.table("~/TCC/dados/Scripts/lynx.txt", header = TRUE)$data
length(raw_data)
x <- raw_data[1:114]
lynx <- ts(x, start = 1821, frequency = 1)   # frequencia 1 = dados anuais
summary(lynx)

plot(lynx, main = "Linces canadenses capturados",
     xlab = "Ano", ylab = "Numero de linces", col = "steelblue")

par(mfrow = c(1,2))
acf(lynx,  lag.max = 40)
pacf(lynx, lag.max = 40)
par(mfrow = c(1,1))

library(tseries)
adf.test(lynx)
pp.test(lynx)
kpss.test(lynx, null = "Level")
kpss.test(lynx, null = "Trend")

library(trend)
tempo <- as.numeric(time(lynx))
summary(lm(as.numeric(lynx) ~ tempo))
mk.test(as.numeric(lynx))
cs.test(as.numeric(lynx))

# Ciclo (serie anual: decompose/stl nao se aplicam)
espectro <- spectrum(as.numeric(lynx), log = "no")
1 / espectro$freq[which.max(espectro$spec)]   # periodo dominante em anos (~10)