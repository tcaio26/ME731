library(tidyverse)

setwd('trabalho2')

df_raw = read_csv('dados/all_stocks_2006-01-01_to_2018-01-01.csv') %>% select(c('Date', 'Open', 'Name'))

df = df_raw %>% pivot_wider(names_from = 'Name', values_from = 'Open')

write_csv(df, 'dados/series.csv')
