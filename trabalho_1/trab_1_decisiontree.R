###arquivo para arvore de decisão
setwd('trabalho_1') #remover

if(!require('pacman', character.only = TRUE)){
  install.packages('pacman')
} #instala o pacote pacman, caso já não esteja instalado. este pacote consegue carregar todos os outros de maneira simples.
library(pacman) 

p_load(tidyverse, rpart, party, partykit, rpart.plot) #função do pacote pacman para carregar todos os outros pacotes necessários.

dados_raw = read_csv('Cancer_Data.csv')

dados = dados_raw %>% select(2:12) %>% mutate(diagnosis = factor(diagnosis, levels = c('B','M')))

smp = sample(1:nrow(dados), 0.4*nrow(dados))
treino = dados[smp,]
teste = dados[-smp,]

mod = treino %>% rpart(formula = diagnosis~.)
rpart.plot(mod, type = 1)

prev = ifelse(rpart.predict(mod, teste, type = 'vector')==2, 'M', 'B')

sum(teste$diagnosis==prev)/nrow(teste)
resultados = cbind(previsao = prev, diagnostico = teste$diagnosis) %>% as.data.frame() %>% table()

proportions(resultados)
