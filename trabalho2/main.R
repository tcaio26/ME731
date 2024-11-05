setwd('trabalho2')
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, quantmod, xts, vars)

tickers = c("AAPL", "AMZN", "NFLX", "GOOGL", "MSFT")

start_date = as.Date("2006-01-01")
end_date = as.Date("2017-12-31")

dados_faang = list()

for (ticker in tickers) {
  getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
  dados_faang[[ticker]] <- Cl(get(ticker))  # Extrai apenas os preços de fechamento
}

# Combine as séries em um único objeto xts com apenas os dias úteis
dados_xts <- do.call(merge, dados_faang)
colnames(dados_xts) <- tickers  # Nomeia as colunas com os tickers

# Remova as linhas com valores NA (caso alguma série tenha lacunas)
dados_xts <- na.omit(dados_xts)

# Converta para um data frame para uso com a função VAR
df <- as.data.frame(dados_xts)

modelo_teste = VAR(df, p = 2)

rm(list = c('AAPL', 'AMZN', 'GOOGL', 'MSFT', 'NFLX', 'dados_faang'))


