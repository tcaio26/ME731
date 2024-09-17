setwd('trabalho_1') #remover no final
library(pacman) #carrega outros pacotes
p_load(tidyverse, GGally, faux, MASS, car) #tidyverse: conjunto de pacotes muito uteis. GGally: matriz de correlações e dispersões.

dados_raw = read_csv('ClassicHit.csv') %>% rename_with(~tolower(.x)) %>% mutate(across(c(5,8,10), factor), duration=duration/1000)

dados = dados_raw[,which(sapply(dados_raw, class)=='numeric')]

corrplot::corrplot(cor(dados))

mod1 = lm(energy~.-loudness-energy, data = dados[,c(4:7,9)])
mod2 = lm(loudness~.-year-energy, data = dados[,c(4:7,9)])

modmulti = lm(cbind(year, loudness, energy)~., data = dados[,c(1,4:7,9)])
Anova(modmulti)


##########Simulação
set.seed(247005) #resultados reprodutíveis

corrs = c(runif(9,0.2,0.6),0.8) #correlações entre variaveis

set.seed(NULL) #aleatoriedade da estimação dos parâmetros

t = rnorm_multi(1000, 5, mu = 0, sd = 10, r = corrs)
colnames(t) = c(paste0('x',seq(1:3)),paste0('y',1:2))

mod_y1 = lm(y1~.-y2,t)
mod_y2 = lm(y2~.-y1,t)
mod_multi = lm(cbind(y1,y2)~., t)

summary(mod_y1)
summary(mod_y2)
summary(mod_multi)

estimates = tibble(b1_y1_sep = 0, b2_y1_sep = 0, b1_y2_sep = 0, b2_y2_sep = 0,
                   b1_y1_multi = 0, b2_y1_multi = 0, b1_y2_multi = 0, b2_y2_multi = 0)

for(i in 1:100){
  t = rnorm_multi(10^5, 5, mu = 0, sd = 10, r = corrs)
  colnames(t) = c(paste0('x',seq(1:3)),paste0('y',1:2))
  
  mod_y1 = lm(y1~.-y2,t)
  mod_y2 = lm(y2~.-y1,t)
  mod_multi = lm(cbind(y1,y2)~., t)
  
  estimates = rbind(estimates,
                    c(mod_y1$coefficients[2], mod_y1$coefficients[3], mod_y2$coefficients[2], mod_y2$coefficients[3],
                      mod_multi$coefficients[2,1], mod_multi$coefficients[3,1], mod_multi$coefficients[2,2], mod_multi$coefficients[3,2]))
}

ggpairs(estimates[-1,])

vcov(mod_multi)
Anova(mod_multi)


###########dados sinteticos kaggle
dados2_raw = read_csv('Student_Performance.csv')
dados2 = dados2_raw %>% rename_with(~tolower(gsub(' ', '_', .x))) %>% dplyr::select(-'extracurricular_activities') %>% 
  rename('sample' = sample_question_papers_practiced)

erros = rnorm_multi(10^4, 2, mu = 0, sd = 5, r = 0.9)

dados_manip = dados2 %>% cbind(erros) %>% mutate(p1 = hours_studied*2.5 + sleep_hours*0.4 + sample*0.2 + X1,
                                                 p2 = hours_studied*2.5 + previous_scores*0.4 + sample*0.22 + X2, .keep = 'used') %>% dplyr::select(-(5:6))

cor(dados_manip)

mod1 = lm(p1~.-p2, data = dados_manip) 
mod2 = lm(p2~.-p1, data = dados_manip)

modmulti = lm(cbind(p1,p2)~., data = dados_manip)
