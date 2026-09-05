raw_data <- scan("~/TCC/dados/Scripts/sunspot.txt")
length(raw_data)

x <- raw_data[1:289]

sunspot <- ts(x,start = 1700, frequency = 1)
#Frequencia igual a 1 porque a série tem dados anuais

summary(sunspot)

plot(sunspot, main = "Manchas Solares anuais", xlab = "Ano", ylab = "Numero de manchas solares", col = "steelblue")
par(mfrow = c(1,2))
acf(sunspot, lag.max = 60)
pacf(sunspot, lag.max = 60)
par(mfrow = c(1,1))


#   ADF  -> H0: existe raiz unitaria (NAO estacionaria)
#   KPSS -> H0: a serie E estacionaria
#
#   ADF nao rejeita + KPSS rejeita  => nao estacionaria (diferenciar)
#   ADF rejeita     + KPSS nao rej. => estacionaria
#   Ambos rejeitam / nenhum rejeita => evidencia ambigua


library(tseries)

adf.test(sunspot)
pp.test(sunspot)
kpss.test(sunspot, null = "Level")
kpss.test(sunspot, null = "Trend")



#Test de tendência

#install.packages("trend")   # só uma vez, na vida
#options(repos = c(CRAN = "https://cloud.r-project.org"))
#install.packages("trend")
#install.packages("extraDistr")
library(trend) 

tempo <- as.numeric(time(sunspot))
summary(lm(as.numeric(sunspot) ~ tempo))
mk.test(as.numeric(sunspot))
cs.test(as.numeric(sunspot))


#Test de Sazonalidade
decompose(sunspot)
stl(sunspot, "per")
nsdiffs(sunspot) 


