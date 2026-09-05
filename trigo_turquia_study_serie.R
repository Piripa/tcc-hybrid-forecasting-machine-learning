trigo <- read.table("~/TCC/dados/Scripts/wheat_turkey_01.txt",
                    fill = TRUE, col.names = paste0("V", 1:6))
raw_data <- trigo$V4          # coluna 4 = rendimento (kg/da)
length(raw_data)
x <- raw_data[1:85]
wheat <- ts(x, start = 1938, frequency = 1)   # frequencia 1 = dados anuais
summary(wheat)

plot(wheat, main = "Rendimento do trigo - Turquia",
     xlab = "Ano", ylab = "Rendimento (kg/da)", col = "steelblue")

par(mfrow = c(1,2))
acf(wheat,  lag.max = 30)
pacf(wheat, lag.max = 30)
par(mfrow = c(1,1))

library(tseries)
adf.test(wheat)
pp.test(wheat)
kpss.test(wheat, null = "Level")
kpss.test(wheat, null = "Trend")

library(trend)
tempo <- as.numeric(time(wheat))
summary(lm(as.numeric(wheat) ~ tempo))
mk.test(as.numeric(wheat))
cs.test(as.numeric(wheat))

# Ciclo (serie anual: decompose/stl nao se aplicam)
espectro <- spectrum(as.numeric(wheat), log = "no")
1 / espectro$freq[which.max(espectro$spec)]