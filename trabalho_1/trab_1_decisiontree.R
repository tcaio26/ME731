###arquivo para arvore de decisão
setwd('trabalho_1') #remover

if(!require('pacman', character.only = TRUE)){
  install.packages('pacman')
} #instala o pacote pacman, caso já não esteja instalado. este pacote consegue carregar todos os outros de maneira simples.
library(pacman) 

p_load(tidyverse, rpart, party, partykit, rpart.plot) #função do pacote pacman para carregar todos os outros pacotes necessários.

dados_raw = read_csv('Speed Dating Data.csv') #carregando dados

na_prop = dados_raw %>% sapply(FUN = function(x) sum(is.na(x))/nrow(dados_raw)) %>% as.vector()
cols_na_demais = colnames(dados_raw)[which(na_prop>0.2)] 

dados = dados_raw[,-which(na_prop>0.2)] %>% mutate(match = as.factor(match)) #eliminando variáveis com mais de 20% de dados faltantes

smp = sample(1:8378, 3000)
treino = dados[smp,which(sapply(dados, class)!='character')]
previsão = dados[-smp,which(sapply(dados, class)!='character')] #dividindo dados em treino (3000 obs.) e previsão (5378 obs.)

mod = treino %>% rpart(formula = match~.-dec-dec_o) #modelando
rpart.plot(mod, type = 1) #plot

prev = rpart.predict(mod, previsão, type = 'vector') #previsão

sum(previsão$match == prev-1)/nrow(previsão) #analisando porcentagem de acertos
t = cbind(real = previsão$match, previsto = prev) %>% as.data.frame() %>% table()


modteste = rpart(Species~., iris)

plot(modteste)
text(modteste, use.n = T)

summary(modteste)
