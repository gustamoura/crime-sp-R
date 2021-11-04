rm(list=ls())
library(tidyverse)
library(forecast)
library(plotly)
library(lubridate)

#1) Reading dataset with target and explanatory variables====
df <- readRDS("inputs_tidy/dataset_final.rds") %>% 
  dplyr::mutate(d_l_crimes_letais_SP = log(crimes_letais_SP) - lag(log(crimes_letais_SP)))#first difference of log of lethal crimes level in SP

#lethal crimes level in SP - Time-Series Object
crimes_letais_SP <- ts(df$crimes_letais_SP, start = c(2001, 1), end = c(2018, 12), frequency = 12)

#first difference of log of lethal crimes level in SP - stationary series - Time-Series Object
d_l_crimes_letais_SP <- ts(df$d_l_crimes_letais_SP, start = c(2001, 1), end = c(2018, 12), frequency = 12)

#decomposing lethal crimes level in SP and plotting seasonality - curiositiy
dec_lethal_crime <- crimes_letais_SP %>% decompose(type = "additive")

seas_lethal_crime <- data.frame(date_tidy = zoo::as.Date(time(dec_lethal_crime$seasonal)), seasonality = as.matrix(dec_lethal_crime$seasonal))

#decomposing fist difference of log of lethal crimes level in SP and plotting seasonality
dec_d_l_lethal_crime <- d_l_crimes_letais_SP %>% decompose(type = "additive")

seas_d_l_lethal_crime <- data.frame(date_tidy = zoo::as.Date(time(dec_d_l_lethal_crime$seasonal)), seasonality = as.matrix(dec_d_l_lethal_crime$seasonal))




#2) Time series forecast and cross-validation====

#2.1)ARMA(0,1)====
fit_0_1 <- Arima(d_l_crimes_letais_SP, order = c(0,0,1))

#understand crime levels variations during whole year --> policy makers understand when to expect the worst
#2.1.1) forecasting 12 months ahead -> out of sample 
forecast(fit_0_1) %>% autoplot() #MA(1) tends to average after 2 months -> short memory

#2.1.2) forecasting 12 months ahead -> in sample
#jan/2018 - dec/2018 = test set
test_0_1 <- window(d_l_crimes_letais_SP, start = 2018)
#jan/2001 - dec/2017 = train set
train_forecast_0_1 <- forecast(Arima(window(d_l_crimes_letais_SP, start = 2001, end = c(2017,12)), order = c(0,0,1)), h = 12)

accuracy(object = train_forecast_0_1, x = test_0_1)#better accuracy than naive method (theil's U < 1)

#2.1.3)Cross-validation -> one and three months ahead -> testing short and medium run forecast accuracy
f1 <- function(x, h){forecast(Arima(x, order=c(0,0,1)), h=h)}
#tsCV() retorna erros de previsão: E[t] = Y_real(t+1) - Y_hat(t+1/t)
e1 <- tsCV(d_l_crimes_letais_SP, f1, h=6) %>% 
  data.frame() %>% 
  dplyr::bind_cols(df %>% dplyr::select(data_tidy, y_real = d_l_crimes_letais_SP)) %>% 
  dplyr::select(data_tidy, h.1, h.3, y_real) 

ac_short <- e1 %>% 
  dplyr::mutate(mape_i = abs(h.1/y_real),
                rmse_i = h.1^2,
                mae_i = abs(h.1)) %>%
  dplyr::select(data_tidy, mape_i, rmse_i, mae_i)

ac_medium <- e1 %>% 
  dplyr::mutate(mape_i = abs(h.3/y_real),
                rmse_i = h.3^2,
                mae_i = abs(h.3)) %>%
  dplyr::select(data_tidy, mape_i, rmse_i, mae_i)

#2.1)ARMA(2,6)====
fit_2_6 <- Arima(d_l_crimes_letais_SP, order = c(2,0,6))

#understand crime levels variations during whole year --> policy makers understand when to expect the worst

#2.1.1) forecasting 12 months ahead -> out of sample 
forecast(fit_2_6) %>% autoplot() #MA(1) tends to average after 2 months -> short memory

#2.1.2) forecasting 12 months ahead -> in sample
#jan/2018 - dec/2018 = test set
test_2_6 <- window(d_l_crimes_letais_SP, start = 2018)
#jan/2001 - dec/2017 = train set
train_forecast_2_6 <- forecast(Arima(window(d_l_crimes_letais_SP, start = 2001, end = c(2017,12)), order = c(2,0,6)), h = 12)

accuracy(object = train_forecast_2_6, x = test_2_6)#better accuracy than naive method (theil's U < 1)

#2.1.3)Cross-validation -> one and three months ahead -> testing short and medium run forecast accuracy

f1 <- function(x, h){forecast(Arima(x, order=c(2,0,6)), h=h)}
#tsCV() retorna erros de previsão: E[t] = Y_real(t+1) - Y_hat(t+1/t)
e1 <- tsCV(d_l_crimes_letais_SP, f1, h=6) %>% 
  data.frame() %>% 
  dplyr::bind_cols(df %>% dplyr::select(data_tidy, y_real = d_l_crimes_letais_SP)) %>% 
  dplyr::select(data_tidy, h.1, h.3, y_real) 

ac_short <- e1 %>% 
  dplyr::mutate(mape_i = abs(h.1/y_real),
                rmse_i = h.1^2,
                mae_i = abs(h.1)) %>%
  dplyr::select(data_tidy, mape_i, rmse_i, mae_i)

ac_medium <- e1 %>% 
  dplyr::mutate(mape_i = abs(h.3/y_real),
                rmse_i = h.3^2,
                mae_i = abs(h.3)) %>%
  dplyr::select(data_tidy, mape_i, rmse_i, mae_i)
#2.1)ARMA(2,1)====
fit_2_1 <- Arima(d_l_crimes_letais_SP, order = c(2,0,1))

#understand crime levels variations during whole year --> policy makers understand when to expect the worst

#2.1.1) forecasting 12 months ahead -> out of sample 
forecast(fit_2_1) %>% autoplot() #MA(1) tends to average after 2 months -> short memory

#2.1.2) forecasting 12 months ahead -> in sample
#jan/2018 - dec/2018 = test set
test_2_1 <- window(d_l_crimes_letais_SP, start = 2018)
#jan/2001 - dec/2017 = train set
train_forecast_2_1 <- forecast(Arima(window(d_l_crimes_letais_SP, start = 2001, end = c(2017,12)), order = c(2,0,1)), h = 12)

accuracy(object = train_forecast_2_1, x = test_2_1)#better accuracy than naive method (theil's U < 1)

#2.1.3)Cross-validation -> one and three months ahead -> testing short and medium run forecast accuracy

f1 <- function(x, h){forecast(Arima(x, order=c(2,0,1)), h=h)}
#tsCV() retorna erros de previsão: E[t] = Y_real(t+1) - Y_hat(t+1/t)
e1 <- tsCV(d_l_crimes_letais_SP, f1, h=6) %>% 
  data.frame() %>% 
  dplyr::bind_cols(df %>% dplyr::select(data_tidy, y_real = d_l_crimes_letais_SP)) %>% 
  dplyr::select(data_tidy, h.1, h.3, y_real) 

ac_short <- e1 %>% 
  dplyr::mutate(mape_i = abs(h.1/y_real),
                rmse_i = h.1^2,
                mae_i = abs(h.1)) %>%
  dplyr::select(data_tidy, mape_i, rmse_i, mae_i)

ac_medium <- e1 %>% 
  dplyr::mutate(mape_i = abs(h.3/y_real),
                rmse_i = h.3^2,
                mae_i = abs(h.3)) %>%
  dplyr::select(data_tidy, mape_i, rmse_i, mae_i)
