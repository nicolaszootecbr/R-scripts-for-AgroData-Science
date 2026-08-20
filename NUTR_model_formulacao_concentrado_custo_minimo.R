# Carregar os pacotes necessarios
library(linprog)  # Para programacao linear
library(ggplot2)  # Para visualizacao grafica
library(dplyr)    # Para manipulacao de dados

# Definir ingredientes e nutrientes do Concentrado
ingredientes <- c("Fuba de Milho", "Farelo de Soja", "Calcario", "Fosfato Bicalcico")
nutrientes <- c("PB", "NDT", "Ca", "P", "Quantidade Concentrado")

# Funcao objetivo (custos ficticios)
cvec <- c(1, 1, 1, 1)
names(cvec) <- ingredientes
cvec

# Matriz de coeficientes das restricoes (em % de MS, corrigido para MN ก๗ MS)
# Teores de MS (usados para converter MN para MS)
ms_teores <- c(0.90, 0.90, 1.00, 1.00)  # 90% MS para Fuba e Farelo, 100% para Calcario e Fosfato
names(ms_teores) <- ingredientes
ms_teores

# Composicao em % de MN do Concentrado (convertida para % MS)
PB_MN <- c(8.00, 45.00, 0, 0)      # PB em % MN
NDT_MN <- c(80.00, 75.00, 0, 0)    # NDT em % MN
Ca_MN <- c(0.02, 0.25, 22.00, 0)   # Ca em % MN
P_MN <- c(0.25, 0.50, 0, 17.00)    # P em % MN

# Converter MN para MS usando teores de MS em %
PB <- PB_MN / ms_teores / 100  # PB em % MS
NDT <- NDT_MN / ms_teores / 100  # NDT em % MS
Ca <- Ca_MN / ms_teores / 100  # Ca em % MS
P <- P_MN / ms_teores / 100  # P em % MS
Quantidade <- c(1, 1, 1, 1)  # Quantidade total de MS no concentrado

Amat <- rbind(PB, NDT, Ca, P, Quantidade)
rownames(Amat) <- nutrientes
colnames(Amat) <- ingredientes
Amat

# Vetor de termos independentes (exigencias totais menos silagem)
# Exigencias totais: PB = 3,01 kg, NDT = 14,5 kg, Ca = 0,1 kg, P = 0,07 kg
# Silagem Fornece (8,1 kg MS): PB = 6,5% กั 8,1 = 0,5265 kg, NDT = 60% กั 8,1 = 4,86 kg,
# Ca = 0,3% กั 8,1 = 0,0243 kg, P = 0,2% กั 8,1 = 0,0162 kg
bvec <- c(
  3.01 - 0.5265,    # PB: 3,01 - 0,5265 = 2,4835 kg
  14.5 - 4.86,      # NDT: 14,5 - 4,86 = 9,64 kg
  0.1 - 0.0243,     # Ca: 0,1 - 0,0243 = 0,0757 kg
  0.07 - 0.0162,    # P: 0,07 - 0,0162 = 0,0538 kg
  9.9               # Quantidade de MS no concentrado (fixo, 55% de 18 kg)
)
names(bvec) <- nutrientes
bvec
# Direcao das restricoes
const.dir <- c(">=", ">=", ">=", ">=", ">=")

# Resolver o problema
Resultado <- solveLP(cvec, bvec, Amat, const.dir, maximum = FALSE, lpSolve = TRUE)

# Exibir os resultados
print("Resultados da Programacao Linear (Corrigido):")
print(Resultado)
summary(Resultado)

# Solucao (quantidades dos ingredientes no concentrado em kg de MS)
solution_MS <- Resultado$solution
names(solution_MS) <- ingredientes
print("Quantidades dos Ingredientes no Concentrado (kg de MS):")
print(solution_MS)

# Converter quantidades de MS para MN
solution_MN <- solution_MS / ms_teores
print("Quantidades dos Ingredientes no Concentrado (kg de MN):")
print(solution_MN)

# Verificar atendimento das restricoes (somente do concentrado, em kg MS)
concentrate_attendance_MS <- Amat[1:4,] %*% solution_MS  # Apenas nutrientes
names(concentrate_attendance_MS) <- c("PB", "NDT", "Ca", "P")
print("Atendimento das Restricoes pelo Concentrado (kg MS):")
print(concentrate_attendance_MS)

# Calcular atendimento total (concentrado + silagem, em kg MS)
total_attendance_MS <- c(
  concentrate_attendance_MS["PB"] + (6.5 * 8.1 / 100),    # Total PB (kg MS)
  concentrate_attendance_MS["NDT"] + (60 * 8.1 / 100),    # Total NDT (kg MS)
  concentrate_attendance_MS["Ca"] + (0.3 * 8.1 / 100),    # Total Ca (kg MS)
  concentrate_attendance_MS["P"] + (0.2 * 8.1 / 100)      # Total P (kg MS)
)
names(total_attendance_MS) <- c("PB", "NDT", "Ca", "P")
print("Atendimento Total das Restricoes (concentrado + silagem, kg MS):")
print(total_attendance_MS)

# Particao dos nutrientes no concentrado (em MN)
concentrate_attendance_MN <- concentrate_attendance_MS / ms_teores[ingredientes]
print("Particao dos Nutrientes no Concentrado (kg MN):")
print(concentrate_attendance_MN)

# Particao dos nutrientes totais (Volumoso + Concentrado, em MN)
# Silagem em MN: 8,1 kg MS / 0,30 (30% MS) = 27 kg MN
silage_MS <- 8.1  # kg MS
silage_MN <- silage_MS / 0.30  # kg MN (27 kg MN)
silage_nutrients_MN <- c(
  (6.5 / 100) * silage_MS / 0.30,  # PB (6,5% de MS ก๗ MN)
  (60 / 100) * silage_MS / 0.30,   # NDT (60% de MS ก๗ MN)
  (0.3 / 100) * silage_MS / 0.30,  # Ca (0,3% de MS ก๗ MN)
  (0.2 / 100) * silage_MS / 0.30   # P (0,2% de MS ก๗ MN)
)
names(silage_nutrients_MN) <- c("PB", "NDT", "Ca", "P")
silage_nutrients_MN
total_attendance_MN <- concentrate_attendance_MN + silage_nutrients_MN
print("Particao Total dos Nutrientes (Volumoso + Concentrado, kg MN):")
print(total_attendance_MN)

# Particao dos ingredientes na dieta total (Volumoso + Concentrado, em MS)
total_solution_MS <- c(8.1, solution_MS)  # Silagem (8,1 kg MS) + Ingredientes do concentrado (em MS)
names(total_solution_MS) <- c("Silagem", ingredientes)
print("Particao dos Ingredientes na Dieta Total (kg MS):")
print(total_solution_MS)
total_solution_MN <- c(silage_MN, solution_MN)
names(total_solution_MN) <- c("Silagem", ingredientes)
print("Particao dos Ingredientes na Dieta Total (kg MN):")
print(total_solution_MN)

# Calcular proporcoes dos ingredientes no concentrado (%MS)
total_ms_concentrate <- sum(solution_MS)
proportions_MS <- (solution_MS / total_ms_concentrate) * 100
print("Proporcoes dos Ingredientes no Concentrado (%MS):")
print(proportions_MS)

# Proporcoes em MN para o concentrado
proportions_MN <- (solution_MN / sum(solution_MN)) * 100
print("Proporcoes dos Ingredientes no Concentrado (%MN):")
print(proportions_MN)

# Proporcoes totais na dieta (Volumoso + Concentrado, %MN e %MS)
total_proportions_MN <- (total_solution_MN / sum(total_solution_MN)) * 100
total_proportions_MS <- (c(total_solution_MS) / 18) * 100  # Total MS = 18 kg
print("Proporcoes dos Ingredientes na Dieta Total (%MN):")
print(total_proportions_MN)
print("Proporcoes dos Ingredientes na Dieta Total (%MS):")
print(total_proportions_MS)

# Calcular PB e NDT totais em %MS (baseado nos 18 kg de MS totais)
pb_total_percent <- (total_attendance_MS["PB"] / 18) * 100
ndt_total_percent <- (total_attendance_MS["NDT"] / 18) * 100
print("PB Total (%MS):")
print(pb_total_percent)
print("NDT Total (%MS):")
print(ndt_total_percent)

# Visualizacao grafica com as principais informacoes para tomada de decisao
# Preparar dados para o grafico
nutrient_data <- data.frame(
  Nutrient = c("PB", "NDT", "Ca", "P"),
  Requirement = c(3.01, 14.5, 0.1, 0.07),  # Exigencias totais (kg MS)
  Supplied = c(total_attendance_MS["PB"], total_attendance_MS["NDT"], 
               total_attendance_MS["Ca"], total_attendance_MS["P"]),  # Fornecidos (kg MS)
  Unit = "kg MS"
)

# Grafico de barras comparando exigencias vs. fornecimento
ggplot(nutrient_data, aes(x = Nutrient, y = Supplied, fill = "Supplied")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(aes(yintercept = Requirement, color = "Requirement"), linetype = "dashed") +
  labs(title = "Comparacao entre Exigencias e Fornecimento Nutricional",
       x = "Nutriente", y = "Quantidade (kg MS)",
       fill = "Legenda", color = "Legenda") +
  scale_color_manual(values = c("Requirement" = "red")) +
  scale_fill_manual(values = c("Supplied" = "blue")) +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.2f kg", Supplied), y = Supplied), 
            vjust = -0.5, size = 3) +
  geom_text(aes(label = sprintf("%.2f kg", Requirement), y = Requirement), 
            vjust = 1.5, color = "red", size = 3)

# Grafico de barras para proporcoes dos ingredientes na dieta (%MS)
ingredient_data <- data.frame(
  Ingredient = c("Silagem", ingredientes),
  Proportion_MS = total_proportions_MS
)


ggplot(ingredient_data, aes(x = Ingredient, y = Proportion_MS, fill = Ingredient)) +
  geom_bar(stat = "identity") +
  labs(title = "Proporcoes dos Ingredientes na Dieta Total (%MS)",
       x = "Ingrediente", y = "% MS",
       fill = "Ingrediente") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = sprintf("%.1f%%", Proportion_MS)), 
            vjust = -0.5, size = 3)
