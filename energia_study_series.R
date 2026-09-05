nsw <- read.csv("~/TCC/dados/Scripts/PRICE_AND_DEMAND_201303_NSW1.csv")
nrow(nsw)                       # 1488 registros de meia em meia hora

# Converte para taxas horarias: media dos dois registros de 30 min de cada hora
hora     <- rep(1:744, each = 2)
raw_data <- as.numeric(tapply(nsw$RRP, hora, mean))
length(raw_data)                # 744 = 31 x 24, igual ao artigo

x <- raw_data[1:744]
eletric <- ts(x, start = c(1,1), frequency = 24)   # 24 horas = 1 dia
summary(eletric)

plot(eletric, main = "Precos horarios de eletricidade - NSW (mar/2013)",
     xlab = "Dia", ylab = "Preco (AUD/MWh)", col = "steelblue")

par(mfrow = c(1,2))
acf(as.numeric(eletric),  lag.max = 60)
pacf(as.numeric(eletric), lag.max = 60)
par(mfrow = c(1,1))

library(tseries)
adf.test(eletric); pp.test(eletric)
kpss.test(eletric, null = "Level"); kpss.test(eletric, null = "Trend")

library(trend)
tempo <- as.numeric(time(eletric))
summary(lm(as.numeric(eletric) ~ tempo))
mk.test(as.numeric(eletric)); cs.test(as.numeric(eletric))

plot(decompose(eletric)); plot(stl(eletric, "per"))
library(forecast); nsdiffs(eletric); ndiffs(eletric)