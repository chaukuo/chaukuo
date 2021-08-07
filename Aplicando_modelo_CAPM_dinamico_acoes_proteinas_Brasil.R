#######################################

# Parte 3: Dynamic Linear Models (DLM)

#######################################

library(tidyverse)
library(tseries)
library(forecast)
library(quantmod)
library(ggplot2)
library(BatchGetSymbols)
library(dplyr)
library(tidyr)
library(dlm)
library(plyr)

# Introdução CAPM

# Introdução empresas

# Iremos trabalhar com empresas do setor de proteínas, a saber, BRF, JBS, Marfrig, e uma empresa do setor agrícola, a SLC
# O setor de proteínas sofreu intensa mudanças desde 2009, logo após a fusão da Perdigão e a Sadia, ao mesmo tempo em que engatinhava seu crescimento, até se tornar em uma empresa focada em carne bovina para alimentos processados
# A Marfrig permaneceu focada no segmento de produtos relacionados à carne bovina, com um pé nos Estados Unidos
# Já a SLC, diferentemente das 3 empresas anteriores, possui fazendas, e a intenção é fazer um contraponto às empresas de proteínas, que justamente dependem do grão para ração, que é justamente fornecido pela SLC

# Breve resumo da BRF
# Breve resumo da JBS
# Breve resumo da Marfrig
# Breve resumo da SLC


what_metrics <- yahooQF(c("Price/Sales", 
                          "P/E Ratio",
                          "Price/EPS Estimate Next Year",
                          "PEG Ratio",
                          "Dividend Yield", 
                          "Market Capitalization"))

# Especificando os ativos para se trabalhar

getSymbols("^BVSP", from="2010-01-01", to="2021-05-01")
getSymbols("BRFS3.SA", from="2010-01-01", to="2021-05-01")
getSymbols("JBSS3.SA", from="2010-01-01", to="2021-05-01")
getSymbols("MRFG3.SA", from="2010-01-01", to="2021-05-01")
getSymbols("SLCE3.SA", from="2010-01-01", to="2021-05-01")
getSymbols("BEEF3.SA", from="2010-01-01", to="2021-05-01")

ibov = na.locf(BVSP$BVSP.Adjusted)
brf = na.locf(BRFS3.SA$BRFS3.SA.Adjusted)
jbs = na.locf(JBSS3.SA$JBSS3.SA.Adjusted)
marfrig = na.locf(MRFG3.SA$MRFG3.SA.Adjusted)
slc = na.locf(SLCE3.SA$SLCE3.SA.Adjusted)
minerva = na.locf(BEEF3.SA$BEEF3.SA.Adjusted)

glimpse(brf)

plot(ibov, main = "Índice de ações IBOVESPA", xlab = "Data", ylab = "Índice", col = "red")
plot(brf, main = "Preço das ações BRF", xlab = "Data", ylab = "Preço (R$/ação)", col = "blue")
plot(jbs, main = "Preço das ações JBS", xlab = "Data", ylab = "Preço (R$/ação)", col = "gray")
plot(marfrig, main = "Preço das ações Marfrig", xlab = "Data", ylab = "Preço (R$/ação)", col = "green")
plot(slc, main = "Preço das ações SLC", xlab = "Data", ylab = "Preço (R$/ação)")

metrics_brf <- getQuote(paste("BRFS3.SA", sep="", collapse=";"), what=what_metrics)
metrics_jbs <- getQuote(paste("JBSS3.SA", sep="", collapse=";"), what=what_metrics)
metrics_marfrig <- getQuote(paste("MRFG3.SA", sep="", collapse=";"), what=what_metrics)
metrics_slc <- getQuote(paste("SLCE3.SA", sep="", collapse=";"), what=what_metrics)

metrics_brf
metrics_jbs
metrics_marfrig
metrics_slc

cor.test(ibov, brf)
ggplot(aes(x = BVSP$BVSP.Adjusted, y = BRFS3.SA$BRFS3.SA.Adjusted)) +
  geom_point()

class(brf)

brf %>%
  ggplot(aes(x="", y = BRFS3.SA$BRFS3.SA.Adjusted)) +
  geom_line() +
  theme_classic() +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Apple price chart") +
  scale_y_continuous(breaks = seq(0,5,100))

# Visulamente, a série da BRF aparenta ser estacionária
# iremos fazer o teste Dickey-Fuller para verificar se é estacionária ou não

library(tseries)
library(fUnitRoots)
adf.test(ibov)
adf.test(brf)
adf.test(jbs)
adf.test(marfrig)
adf.test(slc)

# Pelo teste ADF, não descartamos a hipótese nula de que as séries são não estacionárias
# Agora, iremos diferenciar para converter em séries estacionárias
# De acordo como Petris & Petrone, em modelos dinâmicos não necessariamente se precisa trabalhar com séries estacionárias
# No entanto, como  estamos trabalhando com os retornos dos ativos, cabe diferenciar para modelar com série estacionária

brf <- diff(log(brf), lag=1)[-1]
ibov = diff(log(ibov), lag=1)[-1]
jbs = diff(log(jbs), lag=1)[-1]
marfrig = diff(log(marfrig), lag=1)[-1]
slc = diff(log(slc), lag=1)[-1]
minerva = diff(log(minerva), lag=1)[-1]

plot(ibov)
plot(brf)
plot(jbs)
plot(marfrig)
plot(slc)
plot(minerva)

# Podemos visualmente ver que já há uma estacionariedade na série dos retornos

# Estático

# Vamos calcular o beta estático, primeiramente. Ou seja, assume-se de que a variável-objeto não se move ao longo do tempo, o que pode ser uma premissa forte

# O 1o passo é calcular o beta, que pode ser obtido mediante uma regressão linear simples
# Ou seja, quanto varia um ativo (no caso a BRF) dada a variação do mercado como um todo

estatico_brf = lm(brf ~ ibov)
estatico_jbs = lm(jbs ~ ibov)
estatico_marfrig = lm(marfrig ~ ibov)
estatico_slc = lm(slc ~ ibov)

summary(estatico_brf)
summary(estatico_jbs)
summary(estatico_marfrig)
summary(estatico_slc)

# Erro quadrado médio estático

# Feita a regressão linear, calculamos o erro quadrado médio para avaliar o desempenho do modelo especificado

mse_estatico_brf = mean((estatico_brf$fitted - brf)^2)
mae_estatico_brf = mean(abs(estatico_brf$fitted - brf))

mae_estatico_brf

mse_estatico_jbs = mean((estatico_jbs$fitted - jbs)^2)
mae_estatico_jbs = mean(abs(estatico_jbs$fitted - jbs))

mae_estatico_jbs

mse_estatico_marfrig = mean((estatico_marfrig$fitted - marfrig)^2)
mae_estatico_marfrig = mean(abs(estatico_marfrig$fitted - marfrig))

mae_estatico_marfrig

mse_estatico_slc = mean((estatico_slc$fitted - slc)^2)
mae_estatico_slc = mean(abs(estatico_slc$fitted - slc))

mae_estatico_slc

# Pelo modelo linear, obtemos um erro quadrado médio de 0.014
# Iremos à frente comparar este erro com o de outros modelos

########################

#   Dynamic CAPM

#######################

# Agora, iremos estimar os betas dinâmicos, e depois comparar com o obtido pelo estático
# Dessa forma, seguimos a abordagem Bayesiana XXXXXXXXXXXXXXXX

# Lembrando, que um modelo dinâmico segue à especificação:

# Y(t) = Tetha(t) * F(t) + e(t), em que e(t) ~ N(0, V)
# Tetha(t) = Tetha(t-1) + E(r), em que e(t) ~ N(0, W)

# Ou seja, ao contrário da regressão linear, em que o coeficiente é constante, agora o coeficiente também é função do passado
# Máxima verossimilhança na versão frequentista

# Exemplo

modelo_capm <- function(parametros) {
  
  dlmModReg(ibov, dV = exp(parametros[1]), dW = exp(parametros[2 : 3]))
  
}

modelo_capm
# definindo a função
# 5b6v *red5t6r

# ???Ou seja, estamos acima especificando os valores de v e w da distribuição
# No caso, apenas do índice Ibovespa
### 2, 3 são os v e w

# Na equação abaixo, iremos criar 

outMLE_brf <- dlmMLE(brf, parm = rep(0, 3), modelo_capm)

outMLE_brf

outMLE_jbs <- dlmMLE(jbs, parm = rep(0, 3), modelo_capm)

outMLE_jbs

outMLE_marfrig <- dlmMLE(marfrig, parm = rep(0, 3), modelo_capm)

outMLE_marfrig

outMLE_slc <- dlmMLE(slc, parm = rep(0, 3), modelo_capm)

outMLE_slc

outMLE_minerva <- dlmMLE(minerva, parm = rep(0, 3), modelo_capm)

outMLE_minerva


# Como interpretar o outMLE? O OutMLE nos fornece o alfa, beta  e ...

exp(outMLE_brf$par)

exp(outMLE_jbs$par)

exp(outMLE_marfrig$par)

exp(outMLE_slc$par)

exp(outMLE_minerva$par)

# Atráves da equação acima, obtemos os coeficientes beta (4.045118e-04)
# v = 5.344800e-10
# w = 4.489663e-04

# Obtidos os betas, v e w, agora devemos plugar no modelo

model_brf <- modelo_capm(outMLE_brf$par)

model_brf

model_jbs <- modelo_capm(outMLE_jbs$par)

model_jbs

model_marfrig <- modelo_capm(outMLE_marfrig$par)

model_marfrig

model_slc <- modelo_capm(outMLE_slc$par)

model_slc

model_minerva <- modelo_capm(outMLE_minerva$par)

model_minerva

# Acima obtemos os betas dinâmicos

model_filtered_brf = dlmFilter(brf,model_brf)

model_filtered_brf$m #a3fas e betas

model_filtered_jbs = dlmFilter(jbs,model_jbs)

model_filtered_jbs$m

model_filtered_marfrig = dlmFilter(marfrig,model_marfrig)

model_filtered_marfrig$m

model_filtered_slc = dlmFilter(slc,model_slc)

model_filtered_slc$m

model_filtered_minerva = dlmFilter(minerva,model_minerva)

model_filtered_minerva$m

# Plotando os estados filtrados

par(mfrow=c(1,1))
ts.plot(dropFirst(model_filtered_brf$m[,1]), ylab='', main=expression(alpha[t]),lwd=2, col=2)
abline(h=estatico_brf$coefficients[1], lty=2)

par(mfrow=c(1,1))
ts.plot(dropFirst(model_filtered_jbs$m[,1]), ylab='', main=expression(alpha[t]),lwd=2, col=2)
abline(h=estatico_jbs$coefficients[1], lty=2)

par(mfrow=c(1,1))
ts.plot(dropFirst(model_filtered_marfrig$m[,1]), ylab='', main=expression(alpha[t]),lwd=2, col=2)
abline(h=estatico_marfrig$coefficients[1], lty=2)

par(mfrow=c(1,1))
ts.plot(dropFirst(model_filtered_slc$m[,1]), ylab='', main=expression(alpha[t]),lwd=2, col=2)
abline(h=estatico_slc$coefficients[1], lty=2)

par(mfrow=c(1,1))
ts.plot(dropFirst(model_filtered_minerva$m[,1]), ylab='', main=expression(alpha[t]),lwd=2, col=2)
abline(h=estatico_minerva$coefficients[1], lty=2)

# Os betas iniciais são bastante altos pois o a priori é um número que se desvia bastante
# Com mais informações, o modelo corrige e vai melhorando

ts.plot(dropFirst(model_filtered_brf$m[,2]), ylab='',main=expression(beta[t]), lwd=2, col=4)
abline(h=estatico_brf$coefficients[2], lty=2)

outS_brf <- dlmSmooth(brf, model_brf)

ts.plot(dropFirst(model_filtered_jbs$m[,2]), ylab='',main=expression(beta[t]), lwd=2, col=4)
abline(h=estatico_jbs$coefficients[2], lty=2)

outS_jbs <- dlmSmooth(jbs, model_jbs)

ts.plot(dropFirst(model_filtered_marfrig$m[,2]), ylab='',main=expression(beta[t]), lwd=2, col=4)
abline(h=estatico_marfrig$coefficients[2], lty=2)

outS_marfrig <- dlmSmooth(marfrig, model_marfrig)

ts.plot(dropFirst(model_filtered_slcf$m[,2]), ylab='',main=expression(beta[t]), lwd=2, col=4)
abline(h=estatico_slc$coefficients[2], lty=2)

outS_slc <- dlmSmooth(slc, model_slc)

ts.plot(dropFirst(model_filtered_minerva$m[,2]), ylab='',main=expression(beta[t]), lwd=2, col=4)
abline(h=estatico_slc$coefficients[2], lty=2)

outS_slc <- dlmSmooth(slc, model_slc)

# Pelo que observamos, no estado filtrado o beta gira em torno do estático

# Plotando os estados suavizados (após observarmos toda a amostra) reest50aca6

par(mfrow=c(1,1))
ts.plot(dropFirst(outS_brf$s[,1]), ylab='', main=expression(alpha[t]),lwd=2, col=2)
abline(h=estatico_brf$coefficients[1], lty=2)

ts.plot(dropFirst(outS_brf$s[,2]), ylab='',main=expression(beta[t]), lwd=2, col=4)
abline(h=estatico_brf$coefficients[2], lty=2)

par(mfrow=c(1,1))
ts.plot(dropFirst(outS_jbs$s[,1]), ylab='', main=expression(alpha[t]),lwd=2, col=2)
abline(h=estatico_jbs$coefficients[1], lty=2)

ts.plot(dropFirst(outS_jbs$s[,2]), ylab='',main=expression(beta[t]), lwd=2, col=4)
abline(h=estatico_jbs$coefficients[2], lty=2)

par(mfrow=c(1,1))
ts.plot(dropFirst(outS_marfrig$s[,1]), ylab='', main=expression(alpha[t]),lwd=2, col=2)
abline(h=estatico_marfrig$coefficients[1], lty=2)

ts.plot(dropFirst(outS_marfrig$s[,2]), ylab='',main=expression(beta[t]), lwd=2, col=4)
abline(h=estatico_marfrig$coefficients[2], lty=2)

par(mfrow=c(1,1))
ts.plot(dropFirst(outS_slc$s[,1]), ylab='', main=expression(alpha[t]),lwd=2, col=2)
abline(h=estatico_marfrig$coefficients[1], lty=2)

ts.plot(dropFirst(outS_slc$s[,2]), ylab='',main=expression(beta[t]), lwd=2, col=4)
abline(h=estatico_marfrig$coefficients[2], lty=2)

# O alfa parece nao variar muito
# Mas o beta varia razoavelmente!


##############
# CAPM - GARCH
##############

# Como beta_t = cov_t(x,mercado)/var_t(mercado), pelo GARCH conseguimos obter var_t(mercado). Para
# obter a cov_t(x,mercado), basta calcularmos var(ativo+mercado) e var(ativo-mercado) 

library("fGarch")
library("rugarch")

spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                   variance.model = list(model="sGARCH", garchOrder=c(1,1)),
                   distribution.model = "norm")

garch_mercado = ugarchfit(spec, data = ibov)

temp1 = brf + ibov
temp2 = brf - ibov

garch_mais_brf = ugarchfit(spec, data = temp1)
garch_menos_brf = ugarchfit(spec, data = temp2)

var_mercado = garch_mercado@fit$sigma^2
var_mais_brf    = garch_mais_brf@fit$sigma^2
var_menos_brf   = garch_menos_brf@fit$sigma^2

tvp_beta_brf = (var_mais_brf - var_menos_brf)/(4*var_mercado)

ts.plot(tvp_beta_brf, col=1, ylab='BRF', main='Time-Varying Beta', lwd=1)
abline(h=estatico_brf$coefficients[2], lty=2, lwd=2, col=2)



temp3 = jbs + ibov
temp4 = jbs - ibov

garch_mais_jbs = ugarchfit(spec, data = temp3)
garch_menos_jbs = ugarchfit(spec, data = temp4)

var_mercado = garch_mercado@fit$sigma^2
var_mais_jbs    = garch_mais_jbs@fit$sigma^2
var_menos_jbs   = garch_menos_jbs@fit$sigma^2

tvp_beta_jbs = (var_mais_jbs - var_menos_jbs)/(4*var_mercado)

ts.plot(tvp_beta_jbs, col=1, ylab='JBS', main='Time-Varying Beta', lwd=1)
abline(h=estatico_jbs$coefficients[2], lty=2, lwd=2, col=2)


temp5 = marfrig + ibov
temp6 = marfrig - ibov

garch_mais_marfrig = ugarchfit(spec, data = temp5)
garch_menos_marfrig = ugarchfit(spec, data = temp6)

var_mercado = garch_mercado@fit$sigma^2
var_mais_marfrig    = garch_mais_marfrig@fit$sigma^2
var_menos_marfrig   = garch_menos_marfrig@fit$sigma^2

tvp_beta_marfrig = (var_mais_marfrig - var_menos_marfrig)/(4*var_mercado)

ts.plot(tvp_beta_marfrig, col=1, ylab='Marfrig', main='Time-Varying Beta', lwd=1)
abline(h=estatico_marfrig$coefficients[2], lty=2, lwd=2, col=2)



temp7 = slc + ibov
temp8 = slc - ibov

garch_mais_slc = ugarchfit(spec, data = temp7)
garch_menos_slc = ugarchfit(spec, data = temp8)

var_mercado = garch_mercado@fit$sigma^2
var_mais_slc    = garch_mais_slc@fit$sigma^2
var_menos_slc   = garch_menos_slc@fit$sigma^2

tvp_beta_slc = (var_mais_slc - var_menos_slc)/(4*var_mercado)

ts.plot(tvp_beta_slc, col=1, ylab='Marfrig', main='Time-Varying Beta', lwd=1)
abline(h=estatico_slc$coefficients[2], lty=2, lwd=2, col=2)

###
# Backtest do CAPM 

# Kalman filter vs CAPM estático BRF

forecast_brf = model_filtered_brf$f

# Erro quadrado médio KF

mse_kf_brf = mean((forecast_brf - brf)^2)
mae_kf_brf = mean(abs(forecast_brf - brf))

mse_kf_brf/mse_estatico_brf
mae_kf_brf/mae_estatico_brf

# Pelos critérios acima, os modelos de filtro de Kalman performam melhor do que o estático

fitted_brf = rep(NA, nrow(brf))
erro_brf = rep(NA, nrow(brf))

for (t in 1:nrow(brf)){
  fitted_brf[t] = tvp_beta_brf[t]*ibov[t]
  erro_brf[t]   = brf[t] - fitted_brf[t] 
}

MSE_beta_garch_brf = mean(erro_brf^2)
MAE_beta_garch_brf = mean(abs(erro_brf))

MAE_beta_garch_brf/mae_estatico_brf

MSE_beta_garch_brf/mse_estatico_brf



# Kalman filter vs CAPM estático JBS

forecast_jbs = model_filtered_jbs$f

# Erro quadrado médio KF

mse_kf_jbs = mean((forecast_jbs - jbs)^2)
mae_kf_jbs = mean(abs(forecast_jbs - jbs))

mse_kf_jbs/mse_estatico_jbs
mae_kf_jbs/mae_estatico_jbs

# Pelos critérios acima, os modelos de filtro de Kalman performam melhor do que o estático

fitted_jbs = rep(NA, nrow(jbs))
erro_jbs = rep(NA, nrow(jbs))

for (t in 1:nrow(jbs)){
  fitted_jbs[t] = tvp_beta_jbs[t]*ibov[t]
  erro_jbs[t]   = jbs[t] - fitted_jbs[t] 
}

MSE_beta_garch_jbs = mean(erro_jbs^2)
MAE_beta_garch_jbs = mean(abs(erro_jbs))

MAE_beta_garch_jbs/mae_estatico_jbs

MSE_beta_garch_jbs/mse_estatico_jbs



# Kalman filter vs CAPM estático Marfrig

forecast_marfrig = model_filtered_marfrig$f

# Erro quadrado médio KF

mse_kf_marfrig = mean((forecast_marfrig - marfrig)^2)
mae_kf_marfrig = mean(abs(forecast_marfrig - marfrig))

mse_kf_marfrig/mse_estatico_marfrig
mae_kf_marfrig/mae_estatico_marfrig

# Pelos critérios acima, os modelos de filtro de Kalman performam melhor do que o estático

fitted_marfrig = rep(NA, nrow(marfrig))
erro_marfrig= rep(NA, nrow(marfrig))

for (t in 1:nrow(marfrig)){
  fitted_marfrig[t] = tvp_beta_marfrig[t]*ibov[t]
  erro_marfrig[t]   = marfrig[t] - fitted_marfrig[t] 
}

MSE_beta_garch_marfrig = mean(erro_marfrig^2)
MAE_beta_garch_marfrig = mean(abs(erro_marfrig))

MAE_beta_garch_marfrig/mae_estatico_marfrig

MSE_beta_garch_marfrig/mse_estatico_marfrig



# Kalman filter vs CAPM estático SLC

forecast_slc= model_filtered_slc$f

# Erro quadrado médio KF

mse_kf_slc = mean((forecast_slc - slc)^2)
mae_kf_slc = mean(abs(forecast_slc - slc))

mse_kf_slc/mse_estatico_slc
mae_kf_slc/mae_estatico_slc

# Pelos critérios acima, os modelos de filtro de Kalman performam melhor do que o estático

fitted_slc = rep(NA, nrow(slc))
erro_slc= rep(NA, nrow(slc))

for (t in 1:nrow(slc)){
  fitted_slc[t] = tvp_beta_slc[t]*ibov[t]
  erro_slc[t]   = slc[t] - fitted_slc[t] 
}

MSE_beta_garch_slc = mean(erro_slc^2)
MAE_beta_garch_slc = mean(abs(erro_slc))

MAE_beta_garch_slc/mae_estatico_slc

MSE_beta_garch_slc/mse_estatico_slc
