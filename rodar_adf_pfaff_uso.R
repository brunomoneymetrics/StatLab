library(dplyr)
library(urca)
library(stargazer)
library(readr)



# Carregar os dados
start <- c(2006, 12)
freq <- 12
data <- ts(read.csv2('macrodata.csv', header = TRUE, sep = ';', dec = ',')[,-1],
           start = start, frequency = freq)

# Rodar a função
resultado <- rodar_adf_pfaff(data)

# Visualizar as estatísticas do teste
print(resultado$estatisticas)

# Visualizar os valores críticos
print(resultado$criticos)

# Visualizar o diagnóstico final
print(resultado$resultado)

