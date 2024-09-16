setwd('trabalho_1') #remover no final
library(pacman) #carrega outros pacotes
p_load(tidyverse, GGally, faux, MASS) #tidyverse: conjunto de pacotes muito uteis. GGally: matriz de correlações e dispersões.

dados_raw = read_csv('ClassicHit.csv') %>% rename_with(~tolower(.x)) %>% mutate(across(c(5,8,10), factor), duration=duration/1000)

dados = dados_raw[,which(sapply(dados_raw, class)=='numeric')]

ggpairs(dados, lower = list(continuous = wrap('points', alpha=0.05)))

lm(year~.-duration, data = dados) %>% summary()
lm(popularity~.-year, data = dados) %>% summary()


##########Simulação
set.seed(247005) #resultados reprodutíveis

corrs = c(runif(9,0.2,0.6),0.8) #correlações entre variaveis

set.seed(NULL) #aleatoriedade da estimação dos parâmetros

t = rnorm_multi(10^5, 5, mu = 0, sd = 10, r = corrs)
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


