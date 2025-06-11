library(urca)

rodar_adf_pfaff <- function(data_ts) {
  # Cria matrizes para armazenar estatísticas e valores críticos
  teststat <- matrix(NA, nrow = ncol(data_ts), ncol = 6)
  colnames(teststat) <- c('tau3', 'phi2', 'phi3', 'tau2', 'phi1', 'tau1')
  rownames(teststat) <- colnames(data_ts)
  
  cval <- matrix(NA, ncol = 3, nrow = 6)
  colnames(cval) <- c('1pct','5pct','10pct')
  rownames(cval) <- c('tau3','phi2','phi3','tau2', 'phi1', 'tau1')
  
  results <- matrix(NA, ncol = 1, nrow = ncol(data_ts))
  colnames(results) <- 'ProcessoGerador'
  rownames(results) <- colnames(data_ts)
  
  for (i in 1:ncol(data_ts)) {
    serie <- data_ts[, i]
    
    # Trend (com tendência e constante)
    adf_trend <- ur.df(serie, type = 'trend', selectlags = 'AIC')
    teststat[i, 1:3] <- adf_trend@teststat[1:3]
    cval[1:3, ] <- adf_trend@cval[1:3, ]
    
    # Drift (com constante)
    adf_drift <- ur.df(serie, type = 'drift', selectlags = 'AIC')
    teststat[i, 4:5] <- adf_drift@teststat[1:2]
    cval[4:5, ] <- adf_drift@cval[1:2, ]
    
    # None (sem constante nem tendência)
    adf_none <- ur.df(serie, type = 'none', selectlags = 'AIC')
    teststat[i, 6] <- adf_none@teststat[1]
    cval[6, ] <- adf_none@cval[1, ]
    
    # Protocolo de Pfaff (2008)
    if (teststat[i, 1] < cval[1,1] | teststat[i, 1] < cval[1,2] | teststat[i, 1] < cval[1,3]) {
      results[i, ] <- 'Estacionário ao Redor de uma Tendência Linear'
    } else if (teststat[i, 3] > cval[3,1] | teststat[i, 3] > cval[3,2] | teststat[i, 3] > cval[3,3]) {
      results[i, ] <- 'Passeio Aleatório com Tendência Linear'
    } else if (teststat[i, 4] < cval[4,1] | teststat[i, 4] < cval[4,2] | teststat[i, 4] < cval[4,3]) {
      results[i, ] <- 'Estacionário ao Redor de uma Constante'
    } else if (teststat[i, 5] > cval[5,1] | teststat[i, 5] > cval[5,2] | teststat[i, 5] > cval[5,3]) {
      results[i, ] <- 'Passeio Aleatório com Drift'
    } else if (teststat[i, 6] < cval[6,1] | teststat[i, 6] < cval[6,2] | teststat[i, 6] < cval[6,3]) {
      results[i, ] <- 'Estacionário com Média Zero'
    } else {
      results[i, ] <- 'Passeio Aleatório sem Drift'
    }
  }
  
  return(list(estatisticas = teststat, criticos = cval, resultado = results))
}
