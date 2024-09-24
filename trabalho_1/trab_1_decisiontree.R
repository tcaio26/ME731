#Aluno: Caio Théodore Genovese Huss Oliveira, RA: 247005

if(!require('pacman', character.only = TRUE)){
  install.packages('pacman')
} #instala o pacote pacman, caso já não esteja instalado. este pacote consegue carregar todos os outros de maneira simples.
library(pacman) 

p_load(tidyverse, rpart, party, partykit, rpart.plot) #função do pacote pacman para carregar todos os outros pacotes necessários.

dados_raw = read_csv('Cancer_Data.csv') #lendo dados

dados = dados_raw %>% mutate(diagnosis = factor(diagnosis, levels = c('B','M'))) #convertendo o resultado para fator

set.seed(247005) #reprodutibilidade
smp = sample(1:nrow(dados), 0.4*nrow(dados)) #dividindo os dados em treino e teste, de forma aleatória
treino = dados[smp,]
teste = dados[-smp,]

mod = treino %>% rpart(formula = diagnosis~.) #treino do modelo
rpart.plot(mod, type = 1) #plot do modelo

prev = ifelse(rpart.predict(mod, teste, type = 'vector')==2, 'M', 'B') #previsões nos dados de teste

sum(teste$diagnosis==prev)/nrow(teste) #porcentagem de acertos
resultados = cbind(previsao = prev, diagnostico = teste$diagnosis) %>% as.data.frame() %>% table() #tabulação dos resultados

proportions(resultados)
