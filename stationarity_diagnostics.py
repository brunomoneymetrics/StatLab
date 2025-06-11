
import pandas as pd
import numpy as np
from statsmodels.tsa.stattools import adfuller





def rodar_adf_pfaff(df):
    """
    Roda o protocolo de Pfaff (2008) para detecção de raiz unitária
    em cada coluna de um DataFrame contendo séries temporais.

    Parâmetros:
    df (pd.DataFrame): DataFrame com séries temporais nas colunas.

    Retorna:
    pd.DataFrame: Classificação do processo gerador de cada série.
    """

    resultados = {}

    for col in df.columns:
        serie = df[col].dropna()

        # Etapa 1 - Teste com tendência linear (regression='ct')
        resultado_ct = adfuller(serie, regression='ct', autolag='AIC')
        stat_ct = resultado_ct[0]
        crit_ct = resultado_ct[4]

        if stat_ct < crit_ct['10%']:
            resultados[col] = 'Estacionário ao Redor de uma Tendência Linear'
            continue

        # Etapa 2 - Teste com constante apenas (regression='c')
        resultado_c = adfuller(serie, regression='c', autolag='AIC')
        stat_c = resultado_c[0]
        crit_c = resultado_c[4]

        if stat_c < crit_c['10%']:
            resultados[col] = 'Estacionário ao Redor de uma Constante'
            continue

        # Etapa 3 - Teste sem constante nem tendência (regression='n')
        resultado_n = adfuller(serie, regression='n', autolag='AIC')
        stat_n = resultado_n[0]
        crit_n = resultado_n[4]

        if stat_n < crit_n['10%']:
            resultados[col] = 'Estacionário com Média Zero'
        else:
            resultados[col] = 'Passeio Aleatório sem Drift'

    # Converter para DataFrame
    resultados_df = pd.DataFrame.from_dict(resultados, orient='index', columns=['Processo Gerador'])

    return resultados_df