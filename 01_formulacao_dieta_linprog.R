# ---------------------------------------------------------
# Projeto: AgroAdrian Planner - Módulo Nutrição
# Script: Formulação de Custo Mínimo via Programação Linear
# Autor: Nicolas Adrian Viana de Oliveira (UFBA)
# ---------------------------------------------------------

# 1. Carregar pacotes necessários
if(!require(linprog)) install.packages("linprog"); library(linprog)

# 2. Parâmetros de Entrada (Ingredientes e Nutrientes)
ingredientes <- c("Fuba_Milho", "Farelo_Soja", "Calcario", "Fosfato_Bicalcico")
nutrientes   <- c("PB", "NDT", "Ca", "P", "Qtd_MS_Conc")

# 3. Função Objetivo (Preços por kg de MS - Exemplo)
# Nota: Substitua pelos valores reais de mercado da Bahia
custos <- c(1.20, 2.50, 0.50, 4.00) 
names(custos) <- ingredientes

# 4. Matriz de Coeficientes Técnicos (Composição em % da MS)
Amat <- rbind(
  PB         = c(0.0889, 0.5000, 0.0000, 0.0000),
  NDT        = c(0.8889, 0.8333, 0.0000, 0.0000),
  Ca         = c(0.0002, 0.0028, 0.3800, 0.2200),
  P          = c(0.0028, 0.0056, 0.0000, 0.1700),
  Qtd_MS_Conc = c(1.0000, 1.0000, 1.0000, 1.0000)
)
colnames(Amat) <- ingredientes

# 5. Vetor de Exigências (Termos Independentes - bvec)
# Ajustado para suprir o que a silagem não atende
bvec <- c(PB = 2.4835, NDT = 9.64, Ca = 0.0757, P = 0.0538, Qtd_MS_Conc = 9.9)

# 6. Direção das Restrições
# Nota: Usamos '==' para a quantidade total para fechar a mistura
direcoes <- c(">=", ">=", ">=", ">=", "==")

# 7. Resolução do Modelo
modelo_dieta <- solveLP(custos, bvec, Amat, direcoes, maximum = FALSE)

# 8. Exibição de Resultados Técnicos
cat("\n--- Resumo da Formulação (AgroAdrian) ---\n")
print(modelo_dieta$solution)
