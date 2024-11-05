#Aluno: Caio Théodore Genovese Huss Oliveira, RA: 247005

if(!require('pacman', character.only = TRUE)){
  install.packages('pacman')
} #instala o pacote pacman, caso já não esteja instalado. este pacote consegue carregar todos os outros de maneira simples.
library(pacman) 

<<<<<<< HEAD
p_load(tidyverse, rpart, rpart.plot, xtable) #função do pacote pacman para carregar todos os outros pacotes necessários.
=======
p_load(tidyverse, rpart, rpart.plot, party, partykit) #função do pacote pacman para carregar todos os outros pacotes necessários.
>>>>>>> a3c5630125a1f873feed1c537f9e8c710c006ca8

dados = read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRIqtKZhDirowFELywY2sEJjn-7tmqHBREbH84WmPVehcbvdPcK5APzMznnprjo0h6FFkIAPHQM0CRw/pub?gid=635094344&single=true&output=csv') #lendo dados

sapply(dados, function(x) sum(is.na(x))) #conferindo se há valores faltantes nas colunas

dados = dados %>% mutate(diagnosis = factor(diagnosis, levels = c('B','M')), across(3:31, as.numeric)) %>% select(-1)#convertendo o resultado para fator, outras variáveis para numérico e removendo a coluna ID

set.seed(247) #reprodutibilidade
smp = sample(1:nrow(dados), 0.4*nrow(dados)) #dividindo os dados em treino e teste, de forma aleatória
treino = dados[smp,]
teste = dados[-smp,]

mod = treino %>% rpart(formula = diagnosis~.) #treino do modelo

png('arvore_de_decisão.png', width = 10, height = 10, res = 100, units = 'cm')
rpart.plot(mod, type = 1) #plot do modelo
dev.off() #salva a imagem em um arquivo .png

resultados_treino = cbind(previsao = rpart.predict(mod, treino, type = 'vector'), diag = mod$y) %>% as.data.frame()
addmargins(table(resultados_treino))

prev = ifelse(rpart.predict(mod, teste, type = 'vector')==2, 'M', 'B') #previsões nos dados de teste

sum(teste$diagnosis==prev)/nrow(teste) #porcentagem de acertos

resultados = cbind(previsao = prev, diagnostico = teste$diagnosis) %>% as.data.frame() %>% table() #tabulação dos resultados

<<<<<<< HEAD
addmargins(resultados) %>% xtable()
proportions(resultados) %>% addmargins() %>% xtable(digits=3) %>% print()

(sensibilidade = resultados[2,2]/sum(resultados[2,]))
(especificidade = resultados[1,1]/sum(resultados[1,]))
=======
resultados[2,2]/sum(resultados[,2]) #sensibilidade = VP/(VP+FN)
resultados[1,1]/sum(resultados[,1]) #especificidade = VN/(VN+FP)

addmargins(resultados)
proportions(resultados) %>% addmargins() %>% round(3) #matrizes de confusão
>>>>>>> a3c5630125a1f873feed1c537f9e8c710c006ca8
