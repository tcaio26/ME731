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

#manipulação e aplicação
