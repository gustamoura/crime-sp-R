rm(list=ls())
library(tidyverse)
library(forecast)

#lendo dataset e transformando y para obter estacionariedade ====
df <- readRDS("inputs_tidy/dataset_final.rds") %>% 
  dplyr::mutate(d_l_crimes_letais_SP = log(crimes_letais_SP) - lag(log(crimes_letais_SP)))#primeira diferença do log

#transformando dataset para tipo time series =====
d_l_crimes_letais_SP <- ts(df$d_l_crimes_letais_SP, start = c(2001, 1), end = c(2018, 12), frequency = 12)

#estimação arma(0,1) =====  
fit_1 <- arima(d_l_crimes_letais_SP, order = c(0,0,1))

#checa resíduos e testa autocorrelação
checkresiduals(fit_1)


#estimação arma(2,6) =====
fit_2 <- arima(d_l_crimes_letais_SP, order = c(2,0,6))

#checa resíduos e testa autocorrelação
checkresiduals(fit_2)

#estimação arma(2,1) =====
fit_3 <- arima(d_l_crimes_letais_SP, order = c(2,0,1))

#checa resíduos e testa autocorrelação
checkresiduals(fit_3)
