setwd('trabalho2')
library(tidyverse)

df_raw = read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRnk9nil2iRRo48jWxWUFjX1ZFRhI42rCbjFxJlg9GLHIAnq6iIwYftbxxyHqVkS6q2AG0d-CaB4sZz/pub?gid=412999225&single=true&output=csv')

apply(as.matrix(df_raw), 2, function(x) sum(is.na(x))) #todas as séries tem um ou 2 NAs

df = df_raw %>% drop_na()
