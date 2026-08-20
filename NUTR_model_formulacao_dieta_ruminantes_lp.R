# Carregar o pacote
library(linprog)

# Definir ingredientes e nutrientes
ingredientes <- c("Fubá de Milho", "Farelo de Soja", "Calcário", "Fosfato Bicálcico")
nutrientes <- c("PB", "NDT", "Ca", "P", "Quantidade Concentrado")

# Função objetivo (custos fictícios)
cvec <- c(1, 1, 1, 1)
names(cvec) <- ingredientes
cvec

# Matriz de coeficientes das restrições (em % de MS, corrigido para MN ??? MS)
PB <- c(8.89/100, 50/100, 0, 0)      # PB do concentrado (% MS)
NDT <- c(88.89/100, 83.33/100, 0, 0) # NDT do concentrado (% MS)
Ca <- c(0.0222/100, 0.2778/100, 38/100, 22/100)  # Ca do concentrado (% MS)
P <- c(0.2778/100, 0.5556/100, 0, 17/100)   # P do concentrado (% MS)
Quantidade <- c(1, 1, 1, 1)  # Quantidade total de MS no concentrado

Amat <- rbind(PB, NDT, Ca, P, Quantidade)
rownames(Amat) <- nutrientes
colnames(Amat) <- ingredientes
Amat

# Vetor de termos independentes (exigências totais menos silagem, ajustando NDT para viabilidade)
# Exigências totais: PB = 3,01 kg, NDT = 14,5 kg, Ca = 0,1 kg, P = 0,07 kg
# Silagem (8,1 kg MS): PB = 6,5% × 8,1 = 0,5265 kg, NDT = 60% × 8,1 = 4,86 kg, 
# Ca = 0,3% × 8,1 = 0,0243 kg, P = 0,2% × 8,1 = 0,0162 kg
bvec <- c(
  3.01 - 0.5265,    # PB: 3,01 - 0,5265 = 2,4835 kg
  14.5 - 4.86,     # NDT: 14,5 - 4,86 = 9,64 kg
  0.1 - 0.0243,     # Ca: 0,1 - 0,0243 = 0,0757 kg
  0.07 - 0.0162,    # P: 0,07 - 0,0162 = 0,0538 kg
  9.9               # Quantidade de MS no concentrado (fixo, 55% de 18 kg)
)
names(bvec) <- nutrientes
bvec

# Direção das restrições
const.dir <- c(">=", ">=", ">=", ">=", ">=")

# Resolver o problema
Resultado <- solveLP(cvec, bvec, Amat, const.dir, maximum = FALSE, lpSolve = TRUE)

# Exibir os resultados
print("Resultados da Programação Linear (Corrigido):")
print(Resultado)

# Solução (quantidades dos ingredientes no concentrado em kg de MS)
solucao <- Resultado$solution
names(solucao) <- ingredientes
print("Quantidades dos Ingredientes no Concentrado (kg de MS):")
print(solucao)

# Verificar atendimento das restrições (somente do concentrado)
atendimento_concentrado <- Amat[1:4,] %*% solucao  # Apenas nutrientes
names(atendimento_concentrado) <- c("PB", "NDT", "Ca", "P")
print("Atendimento das Restrições pelo Concentrado (kg):")
print(atendimento_concentrado)

# Calcular atendimento total (concentrado + silagem)
atendimento_total <- c(
  atendimento_concentrado["PB"] + (6.5 * 8.1 / 100),    # PB total (kg)
  atendimento_concentrado["NDT"] + (60 * 8.1 / 100),    # NDT total (kg)
  atendimento_concentrado["Ca"] + (0.3 * 8.1 / 100),    # Ca total (kg)
  atendimento_concentrado["P"] + (0.2 * 8.1 / 100)      # P total (kg)
)
names(atendimento_total) <- c("PB", "NDT", "Ca", "P")
print("Atendimento Total das Restrições (concentrado + silagem, kg):")
print(atendimento_total)

# Calcular proporções dos ingredientes no concentrado (%MS)
total_ms_concentrado <- sum(solucao)
proporcoes <- (solucao / total_ms_concentrado) * 100
print("Proporções dos Ingredientes no Concentrado (%MS):")
print(proporcoes)

# Calcular PB e NDT totais em %MS (baseado nos 18 kg de MS totais)
pb_total_percent <- (atendimento_total["PB"] / 18) * 100
ndt_total_percent <- (atendimento_total["NDT"] / 18) * 100
print("PB Total (%MS):")
print(pb_total_percent)
print("NDT Total (%MS):")
print(ndt_total_percent)
