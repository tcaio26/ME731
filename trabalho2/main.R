if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, quantmod, xts, vars, tseries, gghighlight, lubridate, lmtest, forecast, xtable, bruceR) #pacotes necessários

#leitura de dados
tickers = c("AAPL", "AMZN", "NFLX", "GOOGL", "MSFT") #marcadores das empresas

start_date = as.Date("2006-01-01")
end_date = as.Date("2017-12-31") #datas de início e fim das séries

dados_faang = list()

for (ticker in tickers) {
  getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
  dados_faang[[ticker]] = Op(get(ticker))  # Extrai os preços de abertura
}


dados_xts = do.call(merge, dados_faang)
colnames(dados_xts) = tickers #junta as séries em um objeto e nomeia

dados_xts = na.omit(dados_xts) #remove valores faltantes

df = as.data.frame(dados_xts) #cria um objeto da classe data.frame, para funcionar com a função VAR()

rm(list = c('AAPL', 'AMZN', 'GOOGL', 'MSFT', 'NFLX')) #remove objetos desnecessários

#plots das séries

#png('serie_AAPL.png', width = 18, height = 6, units = 'cm', res = 100)
plot(dados_xts$AAPL, main = 'AAPL', asp = 1/4)
#dev.off()

#png('serie_AMZN.png', width = 18, height = 6, units = 'cm', res = 100)
plot(dados_xts$AMZN, main = 'AMZN', asp = 1/4)
#dev.off()

#png('serie_GOOGL.png', width = 18, height = 6, units = 'cm', res = 100)
plot(dados_xts$GOOGL, main = 'GOOGL', asp = 1/4)
#dev.off()

#png('serie_MSFT.png', width = 18, height = 6, units = 'cm', res = 100)
plot(dados_xts$MSFT, main = 'MSFT', asp = 1/4)
#dev.off()

#png('serie_NFLX.png', width = 18, height = 6, units = 'cm', res = 100)
plot(dados_xts$NFLX, main = 'NFLX', asp = 1/4)
#dev.off()

#manipulação e aplicação

dados_log = do.call(merge, lapply(dados_faang, log)) #cria séries estacionárias com uma transformação logarítmica
df_log = as.data.frame(dados_log)
df_diff = do.call(merge, lapply(dados_faang, function(x) diff(log(x)))) %>% as.data.frame() #series com trans. log e uma diferenciação
df_diff = df_diff[-1,]

df_treino = df[ymd(rownames(df))<ymd('2017-01-01'),]
df_log_treino = df_log[ymd(rownames(df_log))<ymd('2017-01-01'),] #restring dados para ajustar o modelo a antes de 2017
df_diff_treino = df_diff[ymd(rownames(df_diff))<ymd('2017-01-01'),]


for (serie in 1:ncol(dados_log)) {
  print(colnames(dados_log)[serie])
  print(adf.test(dados_log[,serie], 'e', k = 1))
  k = kpss.test(diff(dados_xts[,serie]), null = 'Trend') #testes de estacionariedade
  print(k$p.value)
}

crits_log = VARselect(df_log_treino, lag.max = 10) #seleção de critérios
crits_diff = VARselect(df_diff, lag.max = 10)

#png('AIC_lag.png', width = 16, height = 8, units = 'cm', res = 200)   #gráfico de criterios
crits_log$criteria %>% t() %>% cbind(i = 1:10) %>% as.data.frame() %>% ggplot(aes(x = i))+
  geom_line(aes(y = `AIC(n)`), color = 'darkred')+
  geom_point(data = data.frame(i = 2, y = t(crits_log$criteria[1,2])), aes(y = y), color = 'darkred')+
  geom_label(data = data.frame(i = 3, y = t(crits_log$criteria[1,2])), aes(y = y, label = paste0('AIC: ', round(y,3))), size = 2.5)+
  scale_x_continuous(breaks = 1:10)+
  labs(x = 'p', y = 'AIC')+
  theme_bw()
#dev.off()

#modelagem
(modelo_log_p2 = VAR(df_log_treino, p = 2, type = 'trend')) 
(modelo_log_p6 = VAR(df_log_treino, p = 6, type = 'trend')) 
(modelo_log_p1 = VAR(df_log_treino, p = 1, type = 'trend'))  #modelos
(modelo_p14 = VAR(df_treino, p = 14, type = 'trend'))
(modelo_fds = VAR(df_log_treino, p = 34, type = 'trend')) 
modelo_diff = VAR(df_diff_treino, p = 1)

#teste de correlação temporal
modelos = list(modelo_log_p1, modelo_log_p2, modelo_log_p6, modelo_fds, modelo_diff)
tabela = data.frame(modelo = character(0), estatistica = numeric(0), GL = numeric(0), p_valor = numeric(0))
for(modelo in modelos){
  t = serial.test(modelo, lags.pt = 50)
  tabela = rbind(tabela, 
                 data.frame(modelo = paste('modelo', modelo$p), estatistica = round(t$serial$statistic), GL = t$serial$parameter, p_valor = round(t$serial$p.value, 3)))
}

#causalidade granger
granger_causality(modelo_log_p2, test = 'F')

#decomposição
decomp = fevd(modelo_log_p2, n.ahead =5)
decomp$AAPL.Open %>% xtable(digits = 3) %>% print(include.rownames=T)
decomp$AMZN.Open %>% xtable(digits = 3) %>% print(include.rownames=T)
decomp$GOOGL.Open %>% xtable(digits = 3) %>% print(include.rownames=T)
decomp$MSFT.Open %>% xtable(digits = 3) %>% print(include.rownames=T)
decomp$NFLX.Open %>% xtable(digits = 3) %>% print(include.rownames=T)

###PREVISÃO E TESTE
modelo = modelo_log_p2
df = df_log
previsoes = predict(modelo, n.ahead = 20, ci = 0.9)

teste = df[ymd(rownames(df))>=ymd('2017-01-01')&ymd(rownames(df))<ymd('2017-02-01'),]

dados =  mutate(df, data = ymd(rownames(df)))
df_plot = dados[ymd(rownames(dados))>=ymd('2016-01-01')&ymd(rownames(dados))<ymd('2017-02-01'),]

#png('prev_AAPL.png', height = 8, width = 12, units = 'cm', res = 200)
ggplot(data = cbind(as.data.frame(previsoes$fcst$AAPL.Open), data = ymd(rownames(teste))), aes(x = data))+
  geom_line(data = df_plot, aes(y = exp(AAPL.Open), group = 1))+
  geom_line(aes(y = exp(fcst)), color = 'darkred', linewidth = 1)+
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), fill = 'red', alpha = 0.2)+
  labs(y = 'AAPL', title = 'Previsão para o preço de AAPL')+
  theme_bw()
#dev.off()

#png('prev_AMZN.png', height = 8, width = 12, units = 'cm', res = 200)
ggplot(data = cbind(as.data.frame(previsoes$fcst$AMZN.Open), data = ymd(rownames(teste))), aes(x = data))+
  geom_line(data = df_plot, aes(y = exp(AMZN.Open), group = 1))+
  geom_line(aes(y = exp(fcst)), color = 'darkred', linewidth = 1)+
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), fill = 'red', alpha = 0.2)+
  labs(y = 'AMZN', title = 'Previsão para o preço de AMZN')+
  theme_bw()
#dev.off()

#png('prev_GOOGL.png', height = 8, width = 12, units = 'cm', res = 200)
ggplot(data = cbind(as.data.frame(previsoes$fcst$GOOGL.Open), data = ymd(rownames(teste))), aes(x = data))+
  geom_line(data = df_plot, aes(y = exp(GOOGL.Open), group = 1))+
  geom_line(aes(y = exp(fcst)), color = 'darkred', linewidth = 1)+
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), fill = 'red', alpha = 0.2)+
  labs(y = 'GOOGL', title = 'Previsão para o preço de GOOGL')+
  theme_bw()
#dev.off()

#png('prev_MSFT.png', height = 8, width = 12, units = 'cm', res = 200)
ggplot(data = cbind(as.data.frame(previsoes$fcst$MSFT.Open), data = ymd(rownames(teste))), aes(x = data))+
  geom_line(data = df_plot, aes(y = exp(MSFT.Open), group = 1))+
  geom_line(aes(y = exp(fcst)), color = 'darkred', linewidth = 1)+
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), fill = 'red', alpha = 0.2)+
  labs(y = 'MSFT', title = 'Previsão para o preço de MSFT')+
  theme_bw()
#dev.off()

#png('prev_NFLX.png', height = 8, width = 12, units = 'cm', res = 200)
ggplot(data = cbind(as.data.frame(previsoes$fcst$NFLX.Open), data = ymd(rownames(teste))), aes(x = data))+
  geom_line(data = df_plot, aes(y = exp(NFLX.Open), group = 1))+
  geom_line(aes(y = exp(fcst)), color = 'darkred', linewidth = 1)+
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), fill = 'red', alpha = 0.2)+
  labs(y = 'NFLX', title = 'Previsão para o preço de NFLX')+
  theme_bw()
#dev.off()

