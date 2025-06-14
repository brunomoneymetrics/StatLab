library(ggplot2)
library(forecast)
library(mFilter)
library(rbcb)
library(vars)
library(scales)
library(gridExtra)
library(dplyr)
library(grid)

# Utilidades
custom_colors = c('#264653', '#E9C46A') 

# Coletando os dados

ipca = get_series(433 , as='ts', start_date = '2010-01-01') 
selic = get_series(4189, as = 'ts',start_date = '2010-01-01')
cambio = get_series(3697, as = 'ts',start_date = '2010-01-01')
industria =get_series(21940, as = 'ts',start_date = '2010-01-01')

cambio2 =  cambio 

#criar o hiato do produto 
hp = hpfilter(industria, freq =14400, type='lambda')
hiato_a = ts(hp$cycle, start = start(industria),freq =12) 

hiato_pred = forecast(auto.arima(hiato_a, max.p = 4, max.q = 4, 
                                 seasonal = F), h=1, level = 40)$mean

hiato = ts(c(hiato_a,hiato_pred), start = start(industria), freq =12)

# Teste de estacionaridade

data = window(ts.intersect(ipca,selic,cambio,hiato),
              start= c(2010,01))
rodar_adf_pfaff(data)

# Primeira diferença para selic
dselic = diff(selic)
dcambio = diff(cambio)


# Segundo teste de estacionaridade


data2 = window(ts.intersect(ipca,dselic,dcambio,hiato),
              start= c(2010,01))
rodar_adf_pfaff(data2) 

# Criar dummies sazonais

dummies = seasonaldummy(data2[,'ipca']) 

# IPCA PLOT 
g1 = autoplot(data2[,1],colour = "#264653", size = .8) + 
  labs(title = 'IPCA', x = '', y = '',
       subtitle = 'Estacionário ao Redor de uma Tendência Linear') +
  theme_minimal(base_size = 13) +  
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#34495E"),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", face= 'bold'),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
    ))

# DSELIC PLOT 

g2 = autoplot(data2[,2],colour = "#2A9D8F", size = .8) + 
  labs(title = 'SELIC: Primeira diferença', x = '', y = '',
       subtitle = "Estacionário ao Redor de uma Constante") +
  theme_minimal(base_size = 13) +  
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#34495E"),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", face= 'bold'),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
    )
  )

# DCAMBIO PLOT 

g3 = autoplot(data2[,3],colour = "#E9C46A", size = .8) + 
  labs(title = 'Câmbio: Primeira diferença', x = '', y = '',
       subtitle = "Estacionário ao Redor de uma Constante") +
  theme_minimal(base_size = 13) +  
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#34495E"),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", face= 'bold'),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
    )
  )


# HIATO DO PRODUTO PLOT 

g4 = autoplot(data2[,3],colour = "#F4A261", size = .8) + 
  labs(title = 'Hiato do Produto', x = '', y = '',
       subtitle = "Estacionário ao Redor de uma Constante",
       caption = "Elaborado por Bruno Pereira com dados do BCB") +
  theme_minimal(base_size = 13) +  
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#34495E"),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D", face= 'bold'),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.caption = element_text(hjust = 1)
  )



# Título principal
main_title <- textGrob(
  "Modelo Vetorial Autorregressivo (VAR) para Previsão do IPCA",
  gp = gpar(fontsize = 16, fontface = "bold", col = "#34495E")
)

# Subtítulo descritivo
subtitle <- textGrob(
  "Visualização das variáveis utilizadas no modelo: IPCA, SELIC, câmbio e hiato do produto",
  gp = gpar(fontsize = 12, col = "#7F8C8D", fontface ="bold")
)

# Organiza tudo com gráficos (g1, g2, g3, g4)
grid.arrange(
  arrangeGrob(main_title, subtitle, ncol = 1, heights = c(0.3, 0.2)),
  arrangeGrob(g1, g2, g3, g4, layout_matrix = matrix(1:4, ncol = 2, byrow = TRUE)),
  heights = c(0.15, 1)
)



# nomeando os vetore: 

colnames(data2) = c('ipca', 'dselic', 'dcambio', 'hiato')


# Estimando o modelo 

lag = VARselect(data2, lag.max = 12, type = 'trend', season = 12) 
var = VAR(data2, min(lag$selection), type = "both", exogen = dummies)

# Previsão 

setp_h = 7 

var_predict = predict(var, n.ahead = setp_h, ci = .4, 
                      dumvar = head(dummies,setp_h))

fc_ipca <- var_predict$fcst$ipca


# Visualização 


# Extrair a matriz de previsões

fc_mean  <- fc_ipca[, "fcst"]
fc_lower <- fc_ipca[, "lower"]
fc_upper <- fc_ipca[, "upper"]

# Construir ts para as previsões
start_pred <- end(ipca) + c(0, 1)
ts_mean <- ts(fc_mean, start = start_pred, frequency = frequency(ipca))
ts_lower <- ts(fc_lower, start = start_pred, frequency = frequency(ipca))
ts_upper <- ts(fc_upper, start = start_pred, frequency = frequency(ipca))

# Construindo forecast

fc_ts <- structure(list(
  method = "VAR model",
  model = NULL,
  mean = ts_mean,
  lower = ts_lower,
  upper = ts_upper,
  level = 95,
  x = ipca,
  fitted = NULL,
  residuals = NULL
), class = "forecast")

# Plot
autoplot(fc_ts) +
  labs(title = "Previsão do IPCA com Modelo VAR",
       subtitle = "Intervalo de confiança de 95%",
       x = "", y = "Inflação mensal (%)") +
  theme_linedraw(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#264653"),
    plot.subtitle = element_text(size = 12, color = "#7F8C8D"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
