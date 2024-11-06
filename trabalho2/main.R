setwd('trabalho2') #RETIRAR ANTES DE ENVIAR
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, quantmod, xts, vars) #pacotes necessários

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

rm(list = c('AAPL', 'AMZN', 'GOOGL', 'MSFT', 'NFLX', 'dados_faang')) #remove objetos desnecessários

png('serie_AAPL.png', width = 18, height = 6, units = 'cm', res = 100)
plot(dados_xts$AAPL, main = 'AAPL', asp = 1/4)
dev.off()

png('serie_AMZN.png', width = 18, height = 6, units = 'cm', res = 100)
plot(dados_xts$AMZN, main = 'AMZN', asp = 1/4)
dev.off()

png('serie_GOOGL.png', width = 18, height = 6, units = 'cm', res = 100)
plot(dados_xts$GOOGL, main = 'GOOGL', asp = 1/4)
dev.off()

png('serie_MSFT.png', width = 18, height = 6, units = 'cm', res = 100)
plot(dados_xts$MSFT, main = 'MSFT', asp = 1/4)
dev.off()

png('serie_NFLX.png', width = 18, height = 6, units = 'cm', res = 100)
plot(dados_xts$NFLX, main = 'NFLX', asp = 1/4)
dev.off()

#manipulação e aplicação

VARselect(df, lag.max = 15)
VAR(df, p = 6, ic = 'AIC') %>% summary()
