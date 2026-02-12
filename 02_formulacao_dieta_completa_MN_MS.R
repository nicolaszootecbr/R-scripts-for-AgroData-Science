# ---------------------------------------------------------
# Script: Formulação de Dieta com Conversão MN/MS e Gráficos
# Objetivo: Balanceamento de ração para bovinos (Base MS e MN)
# Autor: Nicolas Adrian Viana de Oliveira (UFBA)
# ---------------------------------------------------------

# 1. SETUP
if(!require(pacman)) install.packages("pacman")
pacman::p_load(linprog, ggplot2, dplyr)

# 2. CONFIGURAÇÕES TÉCNICAS (Composição % na MN)
ingredientes <- c("Fuba_Milho", "Farelo_Soja", "Calcario", "Fosfato_Bicalcico")
ms_teores    <- c(0.90, 0.90, 1.00, 1.00) 
names(ms_teores) <- ingredientes

# Matriz de Nutrientes (Convertendo MN para base MS)
# Ex: PB_MS = (PB_MN / MS_Teor)
comp_ms <- rbind(
  PB  = c(8.00, 45.00, 0, 0) / ms_teores / 100,
  NDT = c(80.00, 75.00, 0, 0) / ms_teores / 100,
  Ca  = c(0.02, 0.25, 22.00, 0) / ms_teores / 100,
  P   = c(0.25, 0.50, 0, 17.00) / ms_teores / 100,
  Qtd = c(1, 1, 1, 1)
)

# 3. EXIGÊNCIAS (Total - Contribuição da Silagem)
# Silagem: 8.1kg MS | 6.5% PB | 60% NDT | 0.3% Ca | 0.2% P
bvec <- c(PB = 2.4835, NDT = 9.64, Ca = 0.0757, P = 0.0538, Qtd = 9.9)

# 4. RESOLUÇÃO E CONVERSÃO PARA MN
res <- solveLP(c(1,1,1,1), bvec, comp_ms, c(">=", ">=", ">=", ">=", "=="))
sol_ms <- res$solution
sol_mn <- sol_ms / ms_teores

# 5. VISUALIZAÇÃO PROFISSIONAL
df_grafico <- data.frame(
  Nutriente = c("PB", "NDT", "Ca", "P"),
  Exigencia = c(3.01, 14.5, 0.1, 0.07),
  Fornecido = as.numeric(comp_ms[1:4,] %*% sol_ms) + c(0.526, 4.86, 0.024, 0.016)
)

ggplot(df_grafico, aes(x = Nutriente)) +
  geom_bar(aes(y = Fornecido, fill = "Fornecido"), stat = "identity") +
  geom_point(aes(y = Exigencia, color = "Exigência"), size = 4) +
  scale_fill_manual(values = "#2c3e50") +
  scale_color_manual(values = "red") +
  labs(title = "AgroAdrian Planner: Balanço Nutricional Total (MS)",
       subtitle = "Comparativo entre exigência biológica e dieta formulada") +
  theme_light()
