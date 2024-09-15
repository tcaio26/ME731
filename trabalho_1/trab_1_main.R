setwd('trabalho_1')
library(pacman)
p_load(tidyverse, GGally)

dados_raw = read_csv('ClassicHit.csv') %>% rename_with(~tolower(.x)) %>% mutate(across(c(5,8,10), factor), duration=duration/1000)

dados = dados_raw[,which(sapply(dados_raw, class)=='numeric')]

ggpairs(dados, lower = list(continuous = wrap('points', alpha=0.05)))

lm(year~.-duration, data = dados) %>% summary()
lm(popularity~.-year, data = dados) %>% summary()


