setwd('trabalho2') #RETIRAR ANTES DE ENVIAR
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, quantmod, xts, vars, tseries, gghighlight, lubridate, lmtest, forecast) #pacotes necessários

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

dados_log = do.call(merge, lapply(dados_faang, log)) #cria séries estacionárias com uma transformação logarítmica e uma diferenciação
df_log = as.data.frame(dados_log)
df_diff = do.call(merge, lapply(dados_faang, function(x) diff(log(x)))) %>% as.data.frame()
df_diff = df_diff[-1,]

df_treino = df[ymd(rownames(df))<ymd('2017-01-01'),]
df_log_treino = df_log[ymd(rownames(df_log))<ymd('2017-01-01'),] #restring dados para ajustar o modelo a antes de 2017
df_diff_treino = df_diff[ymd(rownames(df_diff))<ymd('2017-01-01'),]


for (serie in 1:ncol(dados_log)) {
  print(colnames(dados_log)[serie])
  print(adf.test(dados_log[,serie], 'e', k = 1))
  k = kpss.test(diff(dados_xts[,serie]), null = 'Trend')
  print(k$p.value)
}

crits_log = VARselect(df_log_treino, lag.max = 20, season = 12)
crits = VARselect(df_treino, lag.max = 20)
crits_diff = VARselect(df_diff, lag.max = 10)

crits$criteria %>% t() %>% cbind(i = 1:20) %>% as.data.frame() %>% ggplot(aes(x = i))+
  geom_line(aes(y = `AIC(n)`), color = 'darkred')+
  geom_point(data = data.frame(i = 14, y = t(crits$criteria[1,14])), aes(y = y), color = 'darkred')+
  geom_label(data = data.frame(i = 15.5, y = t(crits$criteria[1,14])), aes(y = y, label = paste0('AIC: ', round(y,3))))+
  labs(x = 'p', y = 'AIC')+
  theme_bw()
  
crits_log$criteria %>% t() %>% cbind(i = 1:20) %>% as.data.frame() %>% ggplot(aes(x = i))+
  geom_line(aes(y = `AIC(n)`), color = 'darkred')+
  geom_point(data = data.frame(i = 2, y = t(crits_log$criteria[1,2])), aes(y = y), color = 'darkred')+
  geom_label(data = data.frame(i = 1, y = t(crits_log$criteria[1,2])), aes(y = y, label = paste0('AIC: ', round(y,3))))+
  labs(x = 'p', y = 'AIC')+
  theme_bw()

#modelagem
(modelo_log_p2 = VAR(df_log_treino, p = 2)) %>% summary()
(modelo_log_p6 = VAR(df_log_treino, p = 6)) %>% summary()
(modelo_log_p1 = VAR(df_log_treino, p = 1)) %>% summary()
(modelo_p14 = VAR(df_treino, p = 14)) %>% summary()
(modelo_diff = VAR(df_diff_treino, p = 1)) %>% summary()
(modelo_fds = VAR(df_log_treino, p = 34)) %>% plot()

normality.test(modelo_log_p2) %>% plot()
normality.test(modelo_log_p1) %>% plot()
normality.test(modelo_p14) %>% plot()
normality.test(modelo_log_p6)
normality.test(modelo_diff) %>% plot()

for(i in 1:100){
  t = serial.test(VAR(df_log_treino, p = i), lags.pt = 100)
  print(paste(round(t$serial$p.value, 3), i, sep = '|||'))
}


roots(modelo_log_p2)

###PREVISÃO E TESTE
modelo = modelo_fds
df = df_log
previsoes = predict(modelo, n.ahead = 20, ci = 0.9)
plot(previsoes)
fanchart(previsoes)


teste = df[ymd(rownames(df))>=ymd('2017-01-01')&ymd(rownames(df))<ymd('2017-02-01'),]


dados = df
df_plot = dados[ymd(rownames(dados))>=ymd('2016-01-01')&ymd(rownames(dados))<ymd('2017-02-01'),]

ggplot(data = cbind(as.data.frame(previsoes$fcst$AAPL.Open), data = ymd(rownames(teste))), aes(x = data))+
  geom_line(data = df_plot, aes(y = exp(AAPL.Open), group = 1))+
  geom_line(aes(y = exp(fcst), color = 'forecast'), linewidth = 1)+
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), alpha = 0.2)

ggplot(data = cbind(as.data.frame(previsoes$fcst$AMZN.Open), data = ymd(rownames(teste))), aes(x = data))+
  geom_line(data = df_plot, aes(y = exp(AMZN.Open), group = 1))+
  geom_line(aes(y = exp(fcst), color = 'forecast'), linewidth = 1)+
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), alpha = 0.2)

ggplot(data = cbind(as.data.frame(previsoes$fcst$GOOGL.Open), data = ymd(rownames(teste))), aes(x = data))+
  geom_line(data = df_plot, aes(y = exp(GOOGL.Open), group = 1))+
  geom_line(aes(y = exp(fcst), color = 'forecast'), linewidth = 1)+
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), alpha = 0.2)

ggplot(data = cbind(as.data.frame(previsoes$fcst$MSFT.Open), data = ymd(rownames(teste))), aes(x = data))+
  geom_line(data = df_plot, aes(y = exp(MSFT.Open), group = 1))+
  geom_line(aes(y = exp(fcst), color = 'forecast'), linewidth = 1)+
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), alpha = 0.2)

ggplot(data = cbind(as.data.frame(previsoes$fcst$NFLX.Open), data = ymd(rownames(teste))), aes(x = data))+
  geom_line(data = df_plot, aes(y = exp(NFLX.Open), group = 1))+
  geom_line(aes(y = exp(fcst), color = 'forecast'), linewidth = 1)+
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), alpha = 0.2)

