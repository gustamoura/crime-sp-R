rm(list=ls())
library(tidyverse)
library(plotly)
library(openxlsx)

#1) DATASET WIDE E TIDY PARA CRIMES LETAIS=========
#(OCORR�NCIAS DE HOM�CIDIO DOLOSO + OCORR�NCIAS DE LATROC�NIO), apenas cidade de sp

#dataset longer para ocorrencias de homicidio doloso
crime_longer_doloso <- readr::read_csv("inputs_brutos/monthly_occurrences_policiaSP.csv") %>%
  dplyr::select(-Total, -Regiao) %>% 
  dplyr::filter(Cidade == "S�o Paulo",
                Ano <= 2018,
                Natureza == "HOMIC�DIO DOLOSO (2)") %>% 
  tidyr::pivot_longer(cols = -c(Natureza, Ano, Cidade),
                      names_to = "mes",
                      values_to = "hom_doloso") %>% 
  dplyr::mutate(mes = base::match(mes, month.name)) %>% 
  dplyr::mutate(data_tidy = as.Date(paste0(Ano, "-", mes, "-01"), format("%Y-%m-%d"))) %>% 
  dplyr::select(data_tidy, hom_doloso) %>% 
  dplyr::arrange(data_tidy)

#dataset longer para ocorrencias de latrocinio
crime_longer_latrocinio <- readr::read_csv("inputs_brutos/monthly_occurrences_policiaSP.csv") %>%
  dplyr::select(-Total, -Regiao) %>% 
  dplyr::filter(Cidade == "S�o Paulo",
                Ano <= 2018,
                Natureza == "LATROC�NIO") %>% 
  tidyr::pivot_longer(cols = -c(Natureza, Ano, Cidade),
                      names_to = "mes",
                      values_to = "latrocinio") %>% 
  dplyr::mutate(mes = base::match(mes, month.name)) %>% 
  dplyr::mutate(data_tidy = as.Date(paste0(Ano, "-", mes, "-01"), format("%Y-%m-%d"))) %>% 
  dplyr::select(data_tidy, latrocinio) %>% 
  dplyr::arrange(data_tidy)
  
#juntar os datasets e fazer a soma para ter ocorr�ncias de crimes letais na cidade de sp
df_letais <- crime_longer_doloso %>% 
  dplyr::left_join(crime_longer_latrocinio, by = c("data_tidy")) %>% 
  dplyr::mutate(crimes_letais_SP = hom_doloso + latrocinio) %>% 
  dplyr::select(data_tidy, crimes_letais_SP)

saveRDS(df_letais, "inputs_tidy/crimes_letais_SP.rds")
write.xlsx(df_letais, "inputs_tidy/crimes_letais_SP.xlsx")
#########

#2) DATASET WIDE TIDY DE PRODUTIVIDADE POLICIAL =========

#dataset wide com s�ries de produtividade policial
police_prcvty <- readr::read_csv("inputs_brutos/PoliceProductivity_policiaSP.csv") %>% 
  dplyr::select(-Total, -Regiao) %>% 
  dplyr::filter(Cidade == "S�o Paulo",
                Ano <= 2018) %>% 
  tidyr::pivot_longer(cols = -c(Natureza, Ano, Cidade),
                      names_to = "mes",
                      values_to = "value") %>% 
  tidyr::pivot_wider(names_from = "Natureza", 
                     values_from = "value") %>% 
  dplyr::mutate(mes = base::match(mes, month.name)) %>% 
  dplyr::mutate(data_tidy = as.Date(paste0(Ano, "-", mes, "-01"), format("%Y-%m-%d"))) %>% 
  dplyr::select(-c(Ano, Cidade, mes)) %>% 
  dplyr::select(data_tidy, dplyr::everything()) %>% dplyr::arrange(data_tidy)

saveRDS(police_prcvty, "inputs_tidy/police_productivity.rds")
write.xlsx(police_prcvty, "inputs_tidy/police_productivity.xlsx")
#########

#3) DATASETS ATIVIDADE ECON�MICA: DESEMPREGO E RENDIMENTO M�DIO REAL DOS OCUPADOS=====

#rendimento m�dio real dos ocupados
#obs: pegar planilha com nome Tab46

economia <- readxl::read_excel("inputs_brutos/rendimento_real_m�dio_dos_ocupados.xlsx", sheet = "Tab46") %>% 
  tidyr::pivot_longer(cols = -c(Anos),
                      names_to = "mes",
                      values_to = "renda_real_ocup") %>% 
  dplyr::mutate(mes = base::match(mes, month.name)) %>% 
  dplyr::mutate(data_tidy = as.Date(paste0(Anos, "-", mes, "-01"), format("%Y-%m-%d"))) %>% 
  dplyr::select(data_tidy, renda_real_ocup) %>% dplyr::arrange(data_tidy) %>% 
  dplyr::left_join(readr::read_csv("inputs_brutos/desemprego_serie.csv", 
                                   col_types = cols(...1 = col_skip())) %>% dplyr::select(data_tidy = Data, everything()),
                   by = c("data_tidy")) %>% 
  dplyr::rename(tx_desemp = "Taxa de desemprego")

saveRDS(economia, "inputs_tidy/renda_desemprego_SP.rds")
write.xlsx(economia, "inputs_tidy/renda_desemprego_SP.xlsx")
########