# ------------------------------------------------------------
# Script em R para Análise Detalhada da Pecuária de Corte no Brasil
# Foco: Bovinos e Bubalinos, com detalhamento na Bahia e suas Mesorregiões
# ------------------------------------------------------------

# --- 0. Setup: Carregando Pacotes Necessários ---
message("--- Carregando Pacotes ---")
# Verificar e instalar pacotes se não estiverem presentes
packages_needed <- c("sidrar", "tidyverse", "ggplot2", "geobr", "sf",
                     "readr", "scales", "knitr", "rmarkdown",
                     "ipeadatar", "GetBCBData", "stringr", "purrr") # Added purrr explicitly

for (pkg in packages_needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    message(paste("Pacote", pkg, "instalado."))
  }
  library(pkg, character.only = TRUE)
  # message(paste("Pacote", pkg, "carregado.")) # Optional: confirmation message
}

library(ipeadatar)

# Definir variáveis globais
ano_referencia_sidra <- "last" # Pega o último disponível no SIDRA
ano_referencia_mapas <- 2020  # Ano comum para limites geográficos no geobr
tabela_efetivo <- 3939        # Tabela SIDRA para efetivo
var_efetivo <- 105            # Variável: Efetivo dos rebanhos (Cabeças)
classif_efetivo <- "c79"      # Classificação: Tipo de rebanho
cat_efetivo <- list(c(2670, 2675)) # Categorias: Bovino e Bubalino
cod_bahia <- "29"             # Código IBGE para Bahia

# --- 1. Aquisição de Dados de Efetivo (Cabeças) - SIDRA Tabela 3939 ---
message(paste("\n--- Iniciando Aquisição de Dados de Efetivo (SIDRA Tabela", tabela_efetivo, ") ---"))

# --- 1.1 Helper Function para buscar e limpar dados SIDRA (REVISÃO 2) ---
fetch_clean_sidra_efetivo <- function(geo_level, geo_filter_list = NULL, nivel_label) {
  message(paste("   Buscando Efetivo para Nível:", nivel_label))
  data_raw <- NULL # Initialize data_raw to NULL
  tryCatch({
    # API Call
    data_raw <- get_sidra(
      x = tabela_efetivo,
      variable = var_efetivo,
      period = ano_referencia_sidra,
      geo = geo_level,
      geo.filter = geo_filter_list,
      classific = classif_efetivo,
      category = cat_efetivo,
      header = TRUE # Explicitly request header names from sidrar
    )
    
    # --- DEBUG: Print raw column names ---
    # message("      -> Colunas Brutas Recebidas:")
    # print(names(data_raw))
    # ------------------------------------
    
    # Define expected original column names based on API response
    expected_ano_col <- "Ano"
    expected_valor_col <- "Valor"
    expected_animal_col <- "Tipo de rebanho"
    
    # Define expected geographic and code columns based on geo_level
    expected_geo_col <- switch(geo_level,
                               "Brazil" = "Brasil", # Name might vary, handled below
                               "Region" = "Grande Região",
                               "State" = "Unidade da Federação",
                               "MesoRegion" = "Mesorregião Geográfica",
                               "City" = "Município",
                               NA_character_)
    expected_geo_code_col <- switch(geo_level,
                                    "State" = "Unidade da Federação (Código)",
                                    "MesoRegion" = "Mesorregião Geográfica (Código)",
                                    "City" = "Município (Código)",
                                    NA_character_)
    target_code_col <- switch(geo_level,
                              "State" = "Cod_UF",
                              "MesoRegion" = "Cod_Meso",
                              "City" = "Cod_Mun",
                              NA_character_)
    
    
    # --- Verification and Selection ---
    # Check essential columns first
    essential_cols <- c(expected_ano_col, expected_valor_col, expected_animal_col)
    if(!all(essential_cols %in% names(data_raw))) {
      missing_essential <- setdiff(essential_cols, names(data_raw))
      stop(paste("Colunas essenciais não encontradas:", paste(missing_essential, collapse=", ")))
    }
    
    # Select essential columns
    data_selected <- data_raw %>%
      select(
        Ano = all_of(expected_ano_col),
        Cabecas = all_of(expected_valor_col),
        Animal = all_of(expected_animal_col)
      )
    
    # Add Geographic Column (handle Brazil case)
    geo_col_found <- NA_character_
    if (!is.na(expected_geo_col) && expected_geo_col %in% names(data_raw)) {
      geo_col_found <- expected_geo_col
    } else if (geo_level == "Brazil") {
      # If 'Brasil' column doesn't exist, we'll add 'Unidade_Territorial' later
      message("      -> Nota: Coluna 'Brasil' não encontrada explicitamente. 'Unidade_Territorial' será adicionada.")
    } else if (!is.na(expected_geo_col)) {
      stop(paste("Coluna geográfica esperada '", expected_geo_col, "' não encontrada."))
    }
    # Add the geographic column if found
    if (!is.na(geo_col_found)) {
      data_selected <- data_selected %>%
        mutate(Unidade_Territorial = data_raw[[geo_col_found]]) # Use [[ ]] for dynamic access
    }
    
    # Add Geographic Code Column if expected and exists
    code_col_found <- NA_character_
    if (!is.na(expected_geo_code_col) && expected_geo_code_col %in% names(data_raw)) {
      code_col_found <- expected_geo_code_col
      data_selected <- data_selected %>%
        mutate(Geo_Code_Orig = data_raw[[code_col_found]]) # Use a temporary name
    }
    
    # --- Mutate and Filter ---
    data_cleaned <- data_selected %>%
      mutate(Cabecas = as.numeric(Cabecas),
             Nivel_Geo = nivel_label) %>%
      filter(!is.na(Cabecas))
    
    # Add Unidade_Territorial for Brazil if it wasn't added before
    if (geo_level == "Brazil" && !"Unidade_Territorial" %in% names(data_cleaned)){
      data_cleaned <- data_cleaned %>% mutate(Unidade_Territorial = "Brasil")
    }
    
    # Extract specific code (Cod_UF, Cod_Meso, etc.) if temp code column exists
    if ("Geo_Code_Orig" %in% names(data_cleaned) && !is.na(target_code_col)) {
      data_cleaned <- data_cleaned %>%
        mutate(!!target_code_col := str_extract(Geo_Code_Orig, "\\d+")) %>%
        select(-Geo_Code_Orig) # Remove temporary code column
    }
    
    # --- Final Distinct ---
    data_cleaned <- data_cleaned %>% distinct()
    
    if (nrow(data_cleaned) == 0) {
      message(paste("      -> Atenção: Nenhum dado retornado ou processado para Nível:", nivel_label))
    } else {
      message(paste("      -> Sucesso para Nível:", nivel_label, "-", nrow(data_cleaned), "linhas."))
    }
    return(data_cleaned)
    
  }, error = function(e) {
    message(paste("      -> ERRO para Nível:", nivel_label, "-", e$message))
    if(!is.null(data_raw)) { # Check if data_raw was assigned before error
      message("      -> Colunas retornadas pela API:")
      print(names(data_raw))
    } else {
      message("      -> Erro ocorreu antes de receber dados da API ou data_raw é NULL.")
    }
    return(NULL)
  })
}


# 1.2 Chamando a função para cada nível desejado (RE-EXECUTAR ESTA PARTE)
message("\n--- Re-executando Aquisição de Dados (Revisão 2) ---")
dados_efetivo_nacional <- fetch_clean_sidra_efetivo(geo_level = "Brazil", nivel_label = "Brasil")
dados_efetivo_regional <- fetch_clean_sidra_efetivo(geo_level = "Region", nivel_label = "Região")
dados_efetivo_estadual <- fetch_clean_sidra_efetivo(geo_level = "State", nivel_label = "Estado")
dados_efetivo_bahia_meso <- fetch_clean_sidra_efetivo(geo_level = "MesoRegion",
                                                      geo_filter_list = list("State" = cod_bahia),
                                                      nivel_label = "Mesorregião (BA)")
# Opcional: Descomente a linha abaixo para buscar dados municipais (pode demorar)
# dados_efetivo_bahia_municipio <- fetch_clean_sidra_efetivo(geo_level = "City",
#                                                            geo_filter_list = list("State" = cod_bahia),
#                                                            nivel_label = "Município (BA)")


# 1.3 Combinar todos os dados de efetivo obtidos (RE-EXECUTAR ESTA PARTE)
message("\n--- Re-combinando Dados de Efetivo (Revisão 2) ---")

lista_efetivo_dfs <- list(
  dados_efetivo_nacional,
  dados_efetivo_regional,
  dados_efetivo_estadual,
  dados_efetivo_bahia_meso
  # , dados_efetivo_bahia_municipio
)
lista_efetivo_validos <- purrr::keep(lista_efetivo_dfs, ~ !is.null(.x) && nrow(.x) > 0)

if (length(lista_efetivo_validos) > 0) {
  dados_efetivo_combinados <- bind_rows(lista_efetivo_validos)
  message(paste("Dados de efetivo combinados com sucesso.", nrow(dados_efetivo_combinados), "linhas no total."))
  print("Contagem de linhas por Nível Geográfico:")
  print(table(dados_efetivo_combinados$Nivel_Geo))
  print("Amostra dos dados combinados:")
  print(head(dados_efetivo_combinados))
} else {
  message("ERRO: Nenhum dado de efetivo foi carregado com sucesso. Verifique as mensagens de erro anteriores.")
  dados_efetivo_combinados <- NULL
}

message("\n--- Fim da Re-execução da Parte 1 (Revisão 2) ---")

# --- ATUALIZAR Variáveis Globais (GARANTIR QUE ESTEJAM DEFINIDAS ANTES DA FUNÇÃO) ---
tabela_valor_venda <- 8986
vars_valor_venda <- c(9743, 9745, 9747, 9749)
classif_censo_default <- "c830"
# ------ DEFINIR AQUI ------
cat_censo_total <- list("46427") # Categoria 'Total' da classificação c830
# --------------------------

# --- 2. Aquisição de Dados Econômicos (Valor da VENDA - Censo Agro 2017) ---
message(paste("\n--- Iniciando Aquisição de Dados de Valor da Venda (Censo Agro Tabela", tabela_valor_venda, ") ---"))

# --- 2.1 Helper Function para buscar e limpar dados de Valor da Venda (COMO ANTES) ---
# (Cole a função fetch_clean_censo_valor_venda aqui - a função em si está correta)
fetch_clean_censo_valor_venda <- function(geo_level, geo_filter_list = NULL, nivel_label) {
  message(paste("   Buscando Valor Venda para Nível:", nivel_label))
  data_raw <- NULL
  tryCatch({
    # API Call - Pedir todas as variáveis de valor de uma vez
    data_raw <- get_sidra(
      x = tabela_valor_venda,
      variable = vars_valor_venda,
      period = "2017", # Ano fixo do Censo Agro
      geo = geo_level,
      geo.filter = geo_filter_list,
      # Pedir o total geral usando uma das classificações
      classific = classif_censo_default,
      # ------ USA A VARIÁVEL GLOBAL AQUI ------
      category = cat_censo_total,
      # ---------------------------------------
      header = TRUE
    )
    
    # (Restante da função como na versão anterior...)
    # Define expected columns
    expected_ano_col <- "Ano"
    expected_valor_col <- "Valor" # Valor é em Mil Reais
    expected_variavel_col <- "Variável" # Coluna que identifica qual valor é (venda<X, venda>Y, etc.)
    
    # Geographic columns
    expected_geo_col <- switch(geo_level,
                               "Brazil" = "Brasil",
                               "Region" = "Grande Região",
                               "State" = "Unidade da Federação",
                               NA_character_) # Censo não tem Meso/Mun nessas tabelas
    expected_geo_code_col <- switch(geo_level,
                                    "State" = "Unidade da Federação (Código)",
                                    NA_character_)
    target_code_col <- switch(geo_level,
                              "State" = "Cod_UF",
                              NA_character_)
    
    # --- Verification ---
    essential_cols <- c(expected_ano_col, expected_valor_col, expected_variavel_col)
    if(!all(essential_cols %in% names(data_raw))) {
      missing_essential <- setdiff(essential_cols, names(data_raw))
      stop(paste("Colunas Valor Venda essenciais não encontradas:", paste(missing_essential, collapse=", ")))
    }
    
    # --- Selection and Renaming ---
    data_selected <- data_raw %>%
      select(
        Ano = all_of(expected_ano_col),
        Valor_Venda_Mil_Reais = all_of(expected_valor_col),
        Tipo_Venda_Var = all_of(expected_variavel_col) # Guarda a variável original
      )
    
    # Add Geographic Column
    geo_col_found <- NA_character_
    if (!is.na(expected_geo_col) && expected_geo_col %in% names(data_raw)) {
      geo_col_found <- expected_geo_col
    } else if (geo_level == "Brazil") {
      message("      -> Nota Valor Venda: Coluna 'Brasil' não encontrada. Adicionando 'Unidade_Territorial'.")
    } else if (!is.na(expected_geo_col)){
      stop(paste("Coluna geográfica Valor Venda '", expected_geo_col, "' não encontrada."))
    }
    if (!is.na(geo_col_found)) {
      data_selected <- data_selected %>%
        mutate(Unidade_Territorial = data_raw[[geo_col_found]])
    }
    
    # Add Geographic Code Column (temporary)
    if (!is.na(expected_geo_code_col) && expected_geo_code_col %in% names(data_raw)) {
      data_selected <- data_selected %>%
        mutate(Geo_Code_Orig = data_raw[[expected_geo_code_col]])
    }
    
    # --- Calculate Total Value, Mutate, Filter ---
    data_cleaned <- data_selected %>%
      mutate(
        Valor_Venda_Mil_Reais = as.numeric(Valor_Venda_Mil_Reais),
        Nivel_Geo = nivel_label
      ) %>%
      filter(!is.na(Valor_Venda_Mil_Reais)) %>%
      # Sumarizar os diferentes tipos de venda por Unidade Territorial e Ano
      group_by(Ano, Nivel_Geo, Unidade_Territorial, across(any_of(c("Geo_Code_Orig")))) %>% # Agrupa por geo e código (se existir)
      summarise(
        Valor_Venda_Total = sum(Valor_Venda_Mil_Reais, na.rm = TRUE) * 1000, # Soma e converte para Reais
        .groups = 'drop' # Remove o agrupamento após sumarizar
      )
    
    # Add Unidade_Territorial for Brazil if needed
    if (geo_level == "Brazil" && !"Unidade_Territorial" %in% names(data_cleaned)){
      data_cleaned <- data_cleaned %>% mutate(Unidade_Territorial = "Brasil")
    }
    
    # Extract specific code
    if ("Geo_Code_Orig" %in% names(data_cleaned) && !is.na(target_code_col)) {
      data_cleaned <- data_cleaned %>%
        mutate(!!target_code_col := str_extract(Geo_Code_Orig, "\\d+")) %>%
        select(-Geo_Code_Orig)
    }
    
    # --- Final Distinct ---
    data_cleaned <- data_cleaned %>% distinct()
    
    if (nrow(data_cleaned) == 0) {
      message(paste("      -> Atenção Valor Venda: Nenhum dado retornado ou processado para Nível:", nivel_label))
    } else {
      message(paste("      -> Sucesso Valor Venda para Nível:", nivel_label, "-", nrow(data_cleaned), "linhas."))
    }
    return(data_cleaned)
    
  }, error = function(e) {
    message(paste("      -> ERRO Valor Venda para Nível:", nivel_label, "-", e$message))
    if(!is.null(data_raw)) {
      message("      -> Colunas Valor Venda retornadas pela API:")
      print(names(data_raw))
    } else {
      message("      -> Erro Valor Venda ocorreu antes de receber dados da API ou data_raw é NULL.")
    }
    return(NULL)
  })
}


# --- 2.2 Chamando a função revisada para buscar Valor da Venda ---
# (Código das chamadas como antes)
message("\n--- Executando Aquisição de Dados Valor da Venda (Censo Agro) ---")
valor_venda_nacional <- fetch_clean_censo_valor_venda("Brazil", nivel_label = "Brasil")
valor_venda_regional <- fetch_clean_censo_valor_venda("Region", nivel_label = "Região")
valor_venda_estadual <- fetch_clean_censo_valor_venda("State", nivel_label = "Estado")

# --- 2.3 Combinar dados Valor Venda ---
# (Código de combinação como antes)
message("\n--- Combinando Dados Valor da Venda ---")
lista_valor_venda_dfs <- list(valor_venda_nacional, valor_venda_regional, valor_venda_estadual)
lista_valor_venda_validos <- purrr::keep(lista_valor_venda_dfs, ~ !is.null(.x) && nrow(.x) > 0)

if(length(lista_valor_venda_validos) > 0) {
  dados_valor_venda_combinados <- bind_rows(lista_valor_venda_validos) %>% distinct()
  message(paste("Dados de Valor da Venda combinados com sucesso.", nrow(dados_valor_venda_combinados), "linhas no total."))
  print("Contagem de linhas Valor Venda por Nível Geográfico:")
  print(table(dados_valor_venda_combinados$Nivel_Geo))
  print("Amostra dos dados Valor Venda combinados:")
  dados_valor_venda_combinados <- dados_valor_venda_combinados %>% mutate(Ano = as.numeric(Ano))
  print(head(dados_valor_venda_combinados))
} else {
  message("ERRO: Nenhum dado de Valor da Venda foi carregado com sucesso.")
  dados_valor_venda_combinados <- NULL
}

# --- 3. Entrada de Dados Manuais (Custos CONAB) ---
# --- Carregar Pacotes Adicionais (se ainda não carregados) ---
# (Garantir que tidyverse, lubridate e readr estão carregados do início do script)
library(tidyverse)
library(lubridate)
library(readr)

message("\n--- 3. Criação de Dataframes com Dados de Preços (Substituindo Input Manual) ---")

# 3.1 Preço Boi Gordo (@ 15kg) - Bahia (Fonte: CONAB)
message("   -> Criando dataframe: preco_boi_gordo_conab_ba")
tryCatch({
  preco_boi_gordo_conab_ba <- tibble(
    Periodo = c("01/2022", "02/2022", "03/2022", "04/2022", "05/2022", "06/2022",
                "07/2022", "08/2022", "09/2022", "10/2022", "11/2022", "12/2022",
                "01/2023", "02/2023", "03/2023", "04/2023", "05/2023", "06/2023",
                "07/2023", "08/2023", "09/2023", "10/2023", "11/2023", "12/2023",
                "01/2024", "02/2024", "03/2024", "04/2024", "05/2024", "06/2024",
                "07/2024", "08/2024", "09/2024", "10/2024", "11/2024", "12/2024",
                "01/2025"),
    Preco_String = c("R$ 317,34", "R$ 306,58", "R$ 301,21", "R$ 294,03", "R$ 282,02", "R$ 276,48",
                     "R$ 282,66", "R$ 279,13", "R$ 275,81", "R$ 275,03", "R$ 275,64", "R$ 283,63",
                     "R$ 280,43", "R$ 260,99", "R$ 256,08", "R$ 258,69", "R$ 239,54", "R$ 221,97",
                     "R$ 217,22", "R$ 209,52", "R$ 203,85", "R$ 217,43", "R$ 216,72", "R$ 214,39",
                     "R$ 230,20", "R$ 237,39", "R$ 226,16", "R$ 219,04", "R$ 212,62", "R$ 203,23",
                     "R$ 200,97", "R$ 207,20", "R$ 231,54", "R$ 258,37", "R$ 296,56", "R$ 296,43",
                     "R$ 290,87")
  ) %>%
    mutate(
      Data = my(Periodo), # Converte MM/YYYY para Data (YYYY-MM-01)
      # Usa parse_number para converter string "R$ XXX,YY" para numérico
      Preco_Medio_RS = readr::parse_number(Preco_String, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")),
      UF = "BA",
      Produto = "BOI GORDO (@ 15kg)",
      Fonte = "CONAB/Siagro",
      Nivel = "PRODUTOR"
    ) %>%
    select(Data, UF, Produto, Nivel, Fonte, Preco_Medio_RS) # Seleciona e reordena colunas
  
  print("Amostra - Preço Boi Gordo CONAB BA:")
  print(head(preco_boi_gordo_conab_ba, 3))
  print(tail(preco_boi_gordo_conab_ba, 3))
  # summary(preco_boi_gordo_conab_ba) # Opcional
  
}, error = function(e) {
  message("      -> ERRO ao criar dataframe preco_boi_gordo_conab_ba: ", e$message)
  preco_boi_gordo_conab_ba <- NULL
})


# 3.2 Preço Indicador Boi Gordo - Brasil (Fonte: CEPEA/ESALQ)
message("   -> Criando dataframe: preco_boi_gordo_cepea_br")
tryCatch({
  preco_boi_gordo_cepea_br <- tibble(
    Data_Str = c("01/2022", "02/2022", "03/2022", "04/2022", "05/2022", "06/2022",
                 "07/2022", "08/2022", "09/2022", "10/2022", "11/2022", "12/2022",
                 "01/2023", "02/2023", "03/2023", "04/2023", "05/2023", "06/2023",
                 "07/2023", "08/2023", "09/2023", "10/2023", "11/2023", "12/2023",
                 "01/2024", "02/2024", "03/2024", "04/2024", "05/2024", "06/2024",
                 "07/2024", "08/2024", "09/2024", "10/2024", "11/2024", "12/2024",
                 "01/2025", "02/2025", "03/2025"),
    Valor_Str = c("338,46", "340,29", "344,71", "335,06", "323,10", "317,96",
                  "324,41", "313,39", "303,35", "296,74", "283,35", "292,10",
                  "285,97", "289,72", "281,80", "285,81", "263,83", "248,80",
                  "250,81", "220,36", "212,52", "237,84", "234,87", "248,63",
                  "249,65", "237,84", "232,81", "230,51", "226,92", "220,70",
                  "229,27", "235,07", "255,45", "301,13", "338,76", "320,33",
                  "324,95", "319,21", "311,25")
  ) %>%
    mutate(
      Data = my(Data_Str),
      # A imagem não tem ponto como separador de milhar, só vírgula decimal
      Preco_Medio_RS = readr::parse_number(Valor_Str, locale = readr::locale(decimal_mark = ",")),
      UF = "BR (Indicador)",
      Produto = "BOI GORDO (@)", # Unidade @ (arroba)
      Fonte = "CEPEA/ESALQ"
    ) %>%
    select(Data, UF, Produto, Fonte, Preco_Medio_RS)
  
  print("Amostra - Preço Boi Gordo CEPEA BR:")
  print(head(preco_boi_gordo_cepea_br, 3))
  print(tail(preco_boi_gordo_cepea_br, 3))
  # summary(preco_boi_gordo_cepea_br) # Opcional
  
}, error = function(e) {
  message("      -> ERRO ao criar dataframe preco_boi_gordo_cepea_br: ", e$message)
  preco_boi_gordo_cepea_br <- NULL
})


# 3.3 Preço Indicador Bezerro - Mato Grosso do Sul (Fonte: CEPEA/ESALQ)
message("   -> Criando dataframe: preco_bezerro_cepea_ms")
tryCatch({
  preco_bezerro_cepea_ms <- tibble(
    Data_Str = c("01/2022", "02/2022", "03/2022", "04/2022", "05/2022", "06/2022",
                 "07/2022", "08/2022", "09/2022", "10/2022", "11/2022", "12/2022",
                 "01/2023", "02/2023", "03/2023", "04/2023", "05/2023", "06/2023",
                 "07/2023", "08/2023", "09/2023", "10/2023", "11/2023", "12/2023",
                 "01/2024", "02/2024", "03/2024", "04/2024", "05/2024", "06/2024",
                 "07/2024", "08/2024", "09/2024", "10/2024", "11/2024", "12/2024",
                 "01/2025", "02/2025", "03/2025"),
    Valor_RS_Str = c("2.907,14", "2.854,01", "2.800,89", "2.733,91", "2.696,08", "2.565,10",
                     "2.698,99", "2.681,29", "2.534,49", "2.451,40", "2.450,75", "2.421,95",
                     "2.395,32", "2.377,47", "2.372,33", "2.429,54", "2.280,33", "2.186,70",
                     "2.176,60", "2.070,35", "2.005,35", "2.058,34", "2.059,01", "2.109,33",
                     "2.085,75", "2.053,89", "2.058,89", "2.086,96", "2.091,29", "2.063,02",
                     "2.020,67", "2.063,22", "2.096,12", "2.205,46", "2.575,60", "2.672,61",
                     "2.633,84", "2.667,65", "2.677,07")
  ) %>%
    mutate(
      Data = my(Data_Str),
      # Usa parse_number especificando separador decimal e de milhar
      Preco_Medio_RS = readr::parse_number(Valor_RS_Str, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")),
      UF = "MS",
      Produto = "BEZERRO (Unidade)",
      Fonte = "CEPEA/ESALQ"
    ) %>%
    select(Data, UF, Produto, Fonte, Preco_Medio_RS)
  
  print("Amostra - Preço Bezerro CEPEA MS:")
  print(head(preco_bezerro_cepea_ms, 3))
  print(tail(preco_bezerro_cepea_ms, 3))
  # summary(preco_bezerro_cepea_ms) # Opcional
  
}, error = function(e) {
  message("      -> ERRO ao criar dataframe preco_bezerro_cepea_ms: ", e$message)
  preco_bezerro_cepea_ms <- NULL
})

# 3.4 (Opcional) Combinar Dataframes de Boi Gordo para Comparação
if (!is.null(preco_boi_gordo_conab_ba) && !is.null(preco_boi_gordo_cepea_br)) {
  message("   -> Combinando dataframes de preço do Boi Gordo (BA e BR)")
  # Adiciona coluna 'Local' para diferenciar antes de combinar
  df_ba <- preco_boi_gordo_conab_ba %>% mutate(Local = UF) %>% select(Data, Local, Fonte, Preco_Medio_RS)
  df_br <- preco_boi_gordo_cepea_br %>% mutate(Local = UF) %>% select(Data, Local, Fonte, Preco_Medio_RS)
  
  preco_boi_gordo_combinado <- bind_rows(df_ba, df_br)
  
  print("Amostra - Preço Boi Gordo Combinado (BA vs BR):")
  print(head(preco_boi_gordo_combinado))
  print(tail(preco_boi_gordo_combinado))
} else {
  message("   -> Não foi possível combinar preços do Boi Gordo (BA/BR), um ou ambos dataframes não foram criados.")
  preco_boi_gordo_combinado <- NULL
}


message("\n--- Fim da Parte 3: Criação de Dataframes de Preços ---")

# Remover dataframes raw intermediários, se desejar
#rm(preco_boi_gordo_conab_ba_raw, preco_boi_gordo_cepea_br_raw, preco_bezerro_cepea_ms_raw, df_ba, df_br)message("\n--- Fim da Parte 2 e 3: Aquisição Dados Valor Venda e Custos ---")

# --- 4. Análise e Cálculos Econômicos (CORRIGIDO - Erro across) ---
message("\n--- Iniciando Parte 4: Análise e Cálculos Econômicos ---")

# Verificar se os dataframes necessários existem
if (is.null(dados_efetivo_combinados)) {
  stop("ERRO FATAL: Dataframe 'dados_efetivo_combinados' não foi criado ou está vazio. Verifique a Parte 1.")
}
if (is.null(dados_valor_venda_combinados)) {
  message("AVISO: Dataframe 'dados_valor_venda_combinados' não foi criado ou está vazio. Cálculos relacionados ao valor da venda não serão realizados.")
}

# --- 4.1 Sumarizar Efetivo (Ano mais recente) ---
message("   -> Sumarizando dados de efetivo por Unidade Territorial (ano mais recente)...")
ano_efetivo_recente <- max(dados_efetivo_combinados$Ano, na.rm = TRUE)

# Correção no group_by: usar nomes explícitos ou all_of para robustez
# Pegar todos os nomes exceto Cabecas e Animal para agrupar
grouping_vars <- setdiff(names(dados_efetivo_combinados), c("Cabecas", "Animal"))

efetivo_sumarizado_recente <- dados_efetivo_combinados %>%
  filter(Ano == ano_efetivo_recente) %>%
  group_by(across(all_of(grouping_vars))) %>% # Agrupa por todas as colunas de identificação
  summarise(
    Cabecas_Total_Recente = sum(Cabecas, na.rm = TRUE),
    .groups = 'drop'
  )

if (nrow(efetivo_sumarizado_recente) > 0) {
  message(paste("      -> Efetivo sumarizado para o ano", ano_efetivo_recente))
  print("Amostra Efetivo Sumarizado Recente:")
  print(head(efetivo_sumarizado_recente, 5))
} else {
  message("      -> Não foi possível sumarizar o efetivo para o ano mais recente.")
  efetivo_sumarizado_recente <- NULL
}

# --- 4.2 Preparar Dados para Junção (Efetivo Bovinos 2017) ---
message("   -> Preparando dados de efetivo de Bovinos para o ano 2017...")
# Verificar se dados de 2017 existem em dados_efetivo_combinados
if (2017 %in% unique(dados_efetivo_combinados$Ano)) {
  efetivo_bovinos_2017 <- dados_efetivo_combinados %>%
    filter(Ano == 2017, Animal == "Bovino") %>%
    # ----- CORREÇÃO 1: Usar any_of() diretamente no select -----
  select(
    Nivel_Geo,
    Unidade_Territorial,
    Cabecas_Bovinos_2017 = Cabecas,
    any_of(c("Cod_UF", "Cod_Meso")) # Seleciona se existirem
  )
  # -------------------------------------------------------
  
  if (nrow(efetivo_bovinos_2017) == 0) {
    message("      -> Atenção: Nenhum dado de efetivo de Bovinos encontrado para 2017.")
    efetivo_bovinos_2017 <- NULL # Garantir que seja NULL se vazio
  } else {
    message(paste("      -> Dados de efetivo de Bovinos para 2017 preparados."))
    print("Amostra Efetivo Bovinos 2017:")
    print(head(efetivo_bovinos_2017, 5))
  }
} else {
  message("      -> Atenção: Ano 2017 não encontrado nos dados de efetivo combinados.")
  efetivo_bovinos_2017 <- NULL
}


# --- 4.3 Juntar Efetivo (2017, Bovinos) e Valor da Venda (2017, Bovinos) ---
message("   -> Juntando Efetivo (Bovinos, 2017) com Valor da Venda (Bovinos, 2017)...")
dados_combinados_2017 <- NULL # Inicializar
# Prosseguir apenas se ambos os dataframes de 2017 existem e não são vazios
if (!is.null(dados_valor_venda_combinados) && !is.null(efetivo_bovinos_2017)) {
  
  # Define as colunas de junção comuns, priorizando códigos
  by_cols <- c("Nivel_Geo", "Unidade_Territorial")
  common_code_cols <- intersect(names(efetivo_bovinos_2017), names(dados_valor_venda_combinados))
  code_cols_to_join <- common_code_cols[startsWith(common_code_cols, "Cod_")]
  if (length(code_cols_to_join) > 0) {
    by_cols <- c(by_cols, code_cols_to_join)
  }
  # Garantir que Ano seja adicionado se não estiver nas colunas de join
  if (!"Ano" %in% by_cols) by_cols <- c(by_cols, "Ano")
  
  # Adicionar Ano=2017 ao dataframe de valor de venda antes do join
  dados_valor_venda_join <- dados_valor_venda_combinados %>% mutate(Ano = 2017)
  
  dados_combinados_2017 <- efetivo_bovinos_2017 %>%
    mutate(Ano=2017) %>% # Adicionar Ano aqui também
    full_join(dados_valor_venda_join, by = by_cols) %>%
    mutate(
      Cabecas_Bovinos_2017 = ifelse(is.na(Cabecas_Bovinos_2017), 0, Cabecas_Bovinos_2017),
      Valor_Venda_Total = ifelse(is.na(Valor_Venda_Total), 0, Valor_Venda_Total),
      Ano = 2017 # Redundante, mas garante
    ) %>%
    mutate(Venda_por_Cabeca_2017 = ifelse(Cabecas_Bovinos_2017 > 0, Valor_Venda_Total / Cabecas_Bovinos_2017, 0))
  
  message("      -> Dados de 2017 juntados e 'Venda por Cabeça' calculado.")
  print("Amostra Dados Combinados 2017:")
  print(head(dados_combinados_2017, 5))
  
} else {
  message("      -> Não foi possível juntar dados de 2017 (Efetivo Bovinos 2017 e/ou Valor da Venda ausentes/vazios).")
}


# --- 4.4 Calcular Participação Percentual ---
message("   -> Calculando participação percentual...")

# a) Participação no Efetivo (Ano mais recente)
participacao_efetivo <- NULL
if (!is.null(efetivo_sumarizado_recente)) {
  total_brasil_efetivo_recente <- efetivo_sumarizado_recente %>%
    filter(Nivel_Geo == "Brasil") %>%
    pull(Cabecas_Total_Recente)
  
  if(length(total_brasil_efetivo_recente) == 1 && total_brasil_efetivo_recente > 0){
    participacao_efetivo <- efetivo_sumarizado_recente %>%
      mutate(
        Participacao_Efetivo_Perc = (Cabecas_Total_Recente / total_brasil_efetivo_recente) * 100
      ) %>%
      # ----- CORREÇÃO 1: Usar any_of() diretamente no select -----
    select(
      Ano, Nivel_Geo, Unidade_Territorial, Participacao_Efetivo_Perc,
      any_of(c("Cod_UF", "Cod_Meso")) # Seleciona se existirem
    )
    # -------------------------------------------------------
    message(paste("      -> Participação no Efetivo (", ano_efetivo_recente, ") calculada."))
    print("Amostra Participação Efetivo:")
    print(head(participacao_efetivo %>% filter(Nivel_Geo=="Estado"), 5))
  } else {
    message("      -> Não foi possível calcular participação no Efetivo (total nacional não encontrado ou zero).")
  }
} else {
  message("      -> Não foi possível calcular participação no Efetivo (dados sumarizados recentes ausentes).")
}

# b) Participação no Valor da Venda (2017, Bovinos)
participacao_valor_venda <- NULL
if (!is.null(dados_combinados_2017)) {
  total_brasil_valor_venda_2017 <- dados_combinados_2017 %>%
    filter(Nivel_Geo == "Brasil") %>%
    pull(Valor_Venda_Total)
  
  if(length(total_brasil_valor_venda_2017) == 1 && total_brasil_valor_venda_2017 > 0){
    participacao_valor_venda <- dados_combinados_2017 %>%
      mutate(
        Participacao_Valor_Venda_Perc = (Valor_Venda_Total / total_brasil_valor_venda_2017) * 100
      ) %>%
      # ----- CORREÇÃO 1: Usar any_of() diretamente no select -----
    select(
      Ano, Nivel_Geo, Unidade_Territorial, Participacao_Valor_Venda_Perc,
      any_of(c("Cod_UF", "Cod_Meso")) # Seleciona se existirem
    )
    # -------------------------------------------------------
    message("      -> Participação no Valor da Venda (2017, Bovinos) calculada.")
    print("Amostra Participação Valor Venda:")
    print(head(participacao_valor_venda %>% filter(Nivel_Geo=="Estado"), 5))
  } else {
    message("      -> Não foi possível calcular participação no Valor da Venda (total nacional 2017 não encontrado ou zero).")
  }
} else {
  message("      -> Não foi possível calcular participação no Valor da Venda (dados combinados 2017 ausentes).")
}

# --- 4.5 Consolidar Resultados da Análise (Revisado para robustez) ---
message("   -> Consolidando resultados da análise...")

dados_analise_final <- NULL # Inicializar

if (!is.null(efetivo_sumarizado_recente)) {
  dados_analise_final <- efetivo_sumarizado_recente
  message("      -> Iniciando consolidação com efetivo recente.")
  
  # Juntar Participação Efetivo
  if (!is.null(participacao_efetivo)) {
    message("      -> Juntando participação no efetivo...")
    by_cols_part_efet <- c("Nivel_Geo", "Unidade_Territorial")
    common_code_cols_part_efet <- intersect(names(dados_analise_final), names(participacao_efetivo))
    code_cols_to_join_part_efet <- common_code_cols_part_efet[startsWith(common_code_cols_part_efet, "Cod_")]
    if (length(code_cols_to_join_part_efet) > 0) { by_cols_part_efet <- c(by_cols_part_efet, code_cols_to_join_part_efet)}
    part_efetivo_join <- participacao_efetivo %>% select(all_of(by_cols_part_efet), Participacao_Efetivo_Perc)
    dados_analise_final <- dados_analise_final %>% left_join(part_efetivo_join, by = by_cols_part_efet)
  } else {
    message("      -> Adicionando coluna NA para participação no efetivo.")
    dados_analise_final <- dados_analise_final %>% mutate(Participacao_Efetivo_Perc = NA_real_)
  }
  
  # Juntar Efetivo Total 2017
  if (!is.null(efetivo_total_2017)) {
    message("      -> Juntando efetivo total 2017...")
    by_cols_efet_2017 <- c("Nivel_Geo", "Unidade_Territorial")
    common_code_cols_efet_2017 <- intersect(names(dados_analise_final), names(efetivo_total_2017))
    code_cols_to_join_efet_2017 <- common_code_cols_efet_2017[startsWith(common_code_cols_efet_2017, "Cod_")]
    if (length(code_cols_to_join_efet_2017) > 0) { by_cols_efet_2017 <- c(by_cols_efet_2017, code_cols_to_join_efet_2017)}
    efet_2017_join <- efetivo_total_2017 %>% select(all_of(by_cols_efet_2017), Cabecas_Total_2017)
    dados_analise_final <- dados_analise_final %>% left_join(efet_2017_join, by = by_cols_efet_2017)
  } else {
    message("      -> Adicionando coluna NA para efetivo total 2017.")
    dados_analise_final <- dados_analise_final %>% mutate(Cabecas_Total_2017 = NA_real_)
  }
  
  # Juntar Dados de Valor Venda 2017 (se existirem)
  if (!is.null(dados_valor_venda_combinados)) {
    message("      -> Juntando valor da venda 2017...")
    by_cols_venda_2017 <- c("Nivel_Geo", "Unidade_Territorial")
    common_code_cols_venda_2017 <- intersect(names(dados_analise_final), names(dados_valor_venda_combinados))
    code_cols_to_join_venda_2017 <- common_code_cols_venda_2017[startsWith(common_code_cols_venda_2017, "Cod_")]
    if (length(code_cols_to_join_venda_2017) > 0) { by_cols_venda_2017 <- c(by_cols_venda_2017, code_cols_to_join_venda_2017)}
    # Adicionar Ano se não estiver nas colunas de join
    #if (!"Ano" %in% by_cols_venda_2017) by_cols_venda_2017 <- c(by_cols_venda_2017, "Ano") # Ano é 2017 fixo
    
    venda_2017_join <- dados_valor_venda_combinados %>%
      #filter(Ano == 2017) %>% # Já filtrado, mas garantir
      select(all_of(by_cols_venda_2017), Valor_Venda_Total)
    
    dados_analise_final <- dados_analise_final %>%
      left_join(venda_2017_join, by = by_cols_venda_2017)
    
  } else {
    message("      -> Adicionando coluna NA para valor da venda 2017.")
    dados_analise_final <- dados_analise_final %>% mutate(Valor_Venda_Total = NA_real_)
  }
  
  # Calcular Venda/Cabeca 2017 onde ambos os dados existem
  dados_analise_final <- dados_analise_final %>%
    mutate(Venda_por_Cabeca_2017 = ifelse(!is.na(Cabecas_Total_2017) & Cabecas_Total_2017 > 0 & !is.na(Valor_Venda_Total),
                                          Valor_Venda_Total / Cabecas_Total_2017,
                                          NA_real_)) # Manter NA se algum dado faltar
  
  # Limpeza final
  dados_analise_final <- dados_analise_final %>%
    mutate(across(starts_with("Participacao_"), ~replace_na(., 0)),
           # Decide se quer NA ou 0 para cabeças quando não há dados
           Cabecas_Total_Recente = replace_na(Cabecas_Total_Recente, 0),
           Cabecas_Total_2017 = replace_na(Cabecas_Total_2017, 0)
    )
  
  message("      -> Consolidação dos resultados da análise finalizada.")
  print("Amostra Dados Análise Final (Top 10 Estados):")
  print(head(dados_analise_final %>% filter(Nivel_Geo=="Estado"), 10))
  
} else {
  message("ERRO: Não foi possível iniciar a consolidação.")
}

message("\n--- Fim da Parte 4: Análise e Cálculos ---")


# --- PARTE 5 CORRIGIDA: Junção com Dados Espaciais ---
message("\n--- Refazendo Parte 5: Junção com Dados Espaciais ---")

if (is.null(dados_analise_final)) {
  stop("ERRO FATAL: Dataframe 'dados_analise_final' é NULL. A junção espacial não pode continuar.")
}

# --- 5.1 Mapa Estados ---
message("   -> Carregando mapa dos estados e juntando 'dados_analise_final'...")
mapa_estados_dados <- NULL
tryCatch({
  mapa_brasil_estados <- read_state(code_state = "all", year = ano_referencia_mapas)
  dados_join_estado <- dados_analise_final %>% filter(Nivel_Geo == "Estado")
  
  if ("Cod_UF" %in% names(dados_join_estado) && "code_state" %in% names(mapa_brasil_estados)) {
    mapa_estados_dados <- mapa_brasil_estados %>%
      mutate(code_state = as.character(code_state)) %>%
      left_join(dados_join_estado %>% mutate(Cod_UF = as.character(Cod_UF)), by = c("code_state" = "Cod_UF"))
  } else {
    mapa_estados_dados <- mapa_brasil_estados %>%
      left_join(dados_join_estado, by = c("name_state" = "Unidade_Territorial"))
  }
  if(!is.null(mapa_estados_dados) && "Cabecas_Total_Recente" %in% names(mapa_estados_dados)) message("      -> Junção espacial ESTADOS OK.") else {message(" -> AVISO/ERRO: Junção espacial ESTADOS falhou."); mapa_estados_dados <- mapa_brasil_estados}
}, error = function(e) { message(" -> ERRO ao carregar/juntar mapa dos estados: ", e$message); mapa_estados_dados <- NULL })

# --- 5.2 Mapa Regiões ---
message("\n   -> Carregando mapa das regiões e juntando 'dados_analise_final'...")
mapa_regioes_dados <- NULL
tryCatch({
  mapa_brasil_regioes <- read_region(year = ano_referencia_mapas)
  dados_join_regiao <- dados_analise_final %>% filter(Nivel_Geo == "Região")
  mapa_regioes_dados <- mapa_brasil_regioes %>% left_join(dados_join_regiao, by = c("name_region" = "Unidade_Territorial"))
  if(!is.null(mapa_regioes_dados) && "Cabecas_Total_Recente" %in% names(mapa_regioes_dados)) message("      -> Junção espacial REGIÕES OK.") else {message(" -> AVISO/ERRO: Junção espacial REGIÕES falhou."); mapa_regioes_dados <- mapa_brasil_regioes}
}, error = function(e) { message(" -> ERRO ao carregar/juntar mapa das regiões: ", e$message); mapa_regioes_dados <- NULL })

# --- 5.3 Mapa Mesorregiões da Bahia ---
message("\n   -> Carregando mapa das mesorregiões da Bahia e juntando 'dados_analise_final'...")
mapa_meso_dados_bahia <- NULL
# Tentar baixar o mapa dentro de um tryCatch separado
mapa_bahia_meso_base <- NULL
tryCatch({
  mapa_bahia_meso_base <- read_meso_region(code_meso = cod_bahia, year = ano_referencia_mapas)
  message("      -> Mapa base das mesorregiões da Bahia carregado.")
}, error = function(e){
  message("      -> ERRO ao baixar mapa base das mesorregiões da Bahia: ", e$message)
  message("         -> Mapa das mesorregiões não será gerado.")
})

# Prosseguir com a junção apenas se o mapa base foi carregado e há dados
if(!is.null(mapa_bahia_meso_base) && any(dados_analise_final$Nivel_Geo == "Mesorregião (BA)")) {
  tryCatch({
    dados_join_meso <- dados_analise_final %>% filter(Nivel_Geo == "Mesorregião (BA)")
    
    if ("Cod_Meso" %in% names(dados_join_meso) && "code_meso" %in% names(mapa_bahia_meso_base)) {
      mapa_meso_dados_bahia <- mapa_bahia_meso_base %>%
        mutate(code_meso = as.character(code_meso)) %>%
        left_join(dados_join_meso %>% mutate(Cod_Meso = as.character(Cod_Meso)), by = c("code_meso" = "Cod_Meso"))
    } else {
      mapa_meso_dados_bahia <- mapa_bahia_meso_base %>%
        left_join(dados_join_meso, by = c("name_meso" = "Unidade_Territorial"))
    }
    if(!is.null(mapa_meso_dados_bahia) && "Cabecas_Total_Recente" %in% names(mapa_meso_dados_bahia)) message("      -> Junção espacial MESORREGIÕES (BA) OK.") else {message(" -> AVISO/ERRO: Junção espacial MESORREGIÕES (BA) falhou."); mapa_meso_dados_bahia <- mapa_bahia_meso_base}
  }, error = function(e) { message(" -> ERRO na junção dos dados das mesorregiões: ", e$message); mapa_meso_dados_bahia <- NULL })
} else if (is.null(mapa_bahia_meso_base)) {
  message("   -> AVISO: Mapa base das mesorregiões não pôde ser carregado.")
} else {
  message("   -> AVISO: Não há dados de Mesorregião (BA) em 'dados_analise_final' para juntar.")
}

message("\n--- Fim da Parte 5: Junção com Dados Espaciais ---")


# --- 6. Visualizações (Revisão 3 - Incluindo Mesorregiões e Novos Gráficos) ---
message("\n--- Iniciando Parte 6: Visualizações (Revisão 3) ---")

# --- Verificações Iniciais ---
# (Manter verificações anteriores)
if (is.null(mapa_estados_dados)) message("AVISO: Objeto 'mapa_estados_dados' é NULL. Mapa de Estados não será gerado.")
if (is.null(mapa_regioes_dados)) message("AVISO: Objeto 'mapa_regioes_dados' é NULL. Mapa de Regiões não será gerado.")
# Não verificamos mapa_meso_dados_bahia aqui, pois ele é recriado se possível
if (is.null(preco_boi_gordo_conab_ba)) message("AVISO: DF 'preco_boi_gordo_conab_ba' ausente.")
if (is.null(preco_boi_gordo_cepea_br)) message("AVISO: DF 'preco_boi_gordo_cepea_br' ausente.")
if (is.null(preco_bezerro_cepea_ms)) message("AVISO: DF 'preco_bezerro_cepea_ms' ausente.")
if (is.null(dados_analise_final)) message("AVISO: DF 'dados_analise_final' ausente.")

# Definir cores
cores_mapa_efetivo <- "viridis"
direcao_cores <- -1
ano_plot_recente <- ano_efetivo_recente # Ou defina manualmente se necessário

# --- 6.1 e 6.2: Mapas Brasil (Estados e Regiões) - Efetivo 2023 ---
# (Manter código da versão anterior - já estavam corretos)
# ... (código para mapa_plot_efetivo_estado e mapa_plot_efetivo_regiao) ...
if (!is.null(mapa_estados_dados) && "Participacao_Efetivo_Perc" %in% names(mapa_estados_dados) && "Cabecas_Total_Recente" %in% names(mapa_estados_dados)) {
  message("   -> Gerando Mapa Brasil (Estados): Efetivo ", ano_plot_recente, "...")
  tryCatch({
    mapa_plot_efetivo_estado <- ggplot(mapa_estados_dados) +
      geom_sf(aes(fill = Participacao_Efetivo_Perc), color = "white", linewidth = 0.1) +
      geom_sf_text( aes(label = paste0(sprintf("%.1f%%", Participacao_Efetivo_Perc), "\n", scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = " Mi")(Cabecas_Total_Recente) )), color = "white", size = 3.5, fun.geometry = sf::st_centroid, check_overlap = TRUE ) +
      scale_fill_viridis_c( option = cores_mapa_efetivo, direction = direcao_cores, name = paste0("Participação (%)\nEfetivo ", ano_plot_recente), labels = scales::percent_format(scale = 1, accuracy=1), guide = guide_colorbar(barwidth = 0.8, barheight = 10) ) +
      labs( title = paste("Participação e Efetivo Total (Bovinos+Bubalinos) por Estado -", ano_plot_recente), subtitle = "Rótulos: Participação (%) e Efetivo (Milhões de Cabeças)", caption = paste("Fonte: IBGE/SIDRA Tabela 3939. Mapa:", ano_referencia_mapas) ) +
      theme_void() + theme( plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 9), legend.position = "right", legend.title = element_text(size = 8), legend.text = element_text(size = 7) )
    #print(mapa_plot_efetivo_estado)
    ggsave("mapa_brasil_estados_efetivo_recente.png", mapa_plot_efetivo_estado, width = 10, height = 9, dpi = 300)
    message("      -> Mapa Brasil (Estados) OK.")
  }, error = function(e){ message("      -> ERRO ao gerar Mapa Brasil (Estados): ", e$message) })
} else { message("   -> Pular Mapa Brasil (Estados): Objeto espacial ou colunas necessárias ausentes.")}

if (!is.null(mapa_regioes_dados) && "Participacao_Efetivo_Perc" %in% names(mapa_regioes_dados) && "Cabecas_Total_Recente" %in% names(mapa_regioes_dados)) {
  message("\n   -> Gerando Mapa Brasil (Regiões): Efetivo ", ano_plot_recente,"...")
  tryCatch({
    mapa_regioes_dados_plot <- mapa_regioes_dados %>% mutate(Participacao_Efetivo_Perc = replace_na(Participacao_Efetivo_Perc, 0), Cabecas_Total_Recente = replace_na(Cabecas_Total_Recente, 0))
    mapa_plot_efetivo_regiao <- ggplot(mapa_regioes_dados_plot) +
      geom_sf(aes(fill = Participacao_Efetivo_Perc), color = "black", linewidth = 0.5) +
      geom_sf_text( aes(label = ifelse(Cabecas_Total_Recente > 0, paste0(name_region,"\n", sprintf("%.1f%%", Participacao_Efetivo_Perc), "\n", scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = " Mi")(Cabecas_Total_Recente)), name_region )), color = "white", size = 3.5, fontface = "bold", fun.geometry = sf::st_centroid, check_overlap = FALSE ) +
      scale_fill_viridis_c( option = cores_mapa_efetivo, direction = direcao_cores, name = paste0("Participação (%)\nEfetivo ", ano_plot_recente), labels = scales::percent_format(scale = 1, accuracy=1), guide = guide_colorbar(barwidth = 0.8, barheight = 10) ) +
      labs( title = paste("Participação e Efetivo Total (Bovinos+Bubalinos) por Grande Região -", ano_plot_recente), subtitle = "Rótulos: Nome, Participação (%) e Efetivo (Milhões de Cabeças)", caption = paste("Fonte: IBGE/SIDRA Tabela 3939. Mapa:", ano_referencia_mapas) ) +
      theme_void() + theme( plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 9), legend.position = "right", legend.title = element_text(size = 8), legend.text = element_text(size = 7) )
    #print(mapa_plot_efetivo_regiao)
    ggsave("mapa_brasil_regioes_efetivo_recente.png", mapa_plot_efetivo_regiao, width = 10, height = 8, dpi = 300)
    message("      -> Mapa Brasil (Regiões) OK.")
  }, error = function(e){ message("      -> ERRO ao gerar Mapa Brasil (Regiões): ", e$message) })
} else { message("   -> Pular Mapa Brasil (Regiões): Objeto espacial ou colunas necessárias ausentes.")}


# --- 6.3 Gráfico Bahia (Mesorregiões) - Efetivo (Revisado SEM Mapas) ---
# --- 6.3 Gráficos Bahia (Mesorregiões) - Foco em Proporções 2023 (Revisão) ---

message("\n--- Iniciando Seção 6.3: Visualização Mesorregiões da Bahia (Gráficos Proporcionais) ---")

# Dataframes base necessários (verificar existência)
if (is.null(dados_efetivo_combinados)) {
  stop("ERRO FATAL: Dataframe 'dados_efetivo_combinados' não foi criado ou está vazio.")
}
if (is.null(dados_analise_final) || !any(dados_analise_final$Nivel_Geo == "Mesorregião (BA)")) {
  message("AVISO: Dados de mesorregião não encontrados em 'dados_analise_final'. Gráficos da Seção 6.3 serão pulados.")
  # Define variáveis como NULL para pular os plots
  dados_plot_prop_meso_total <- NULL
  dados_plot_prop_intra_meso <- NULL
} else {
  # --- PREPARAR DADOS ---
  message("   -> Preparando dados proporcionais para Mesorregiões BA (", ano_plot_recente, ")...")
  
  # 1. Dados para Proporção da Mesorregião no Total BA (Bov+Bub)
  dados_meso_total_recente <- dados_analise_final %>%
    filter(Nivel_Geo == "Mesorregião (BA)") %>%
    select(Mesorregiao = Unidade_Territorial, Efetivo_Total = Cabecas_Total_Recente) %>%
    filter(Efetivo_Total > 0) # Considerar apenas mesorregiões com efetivo
  
  if(nrow(dados_meso_total_recente) > 0) {
    # Calcula a participação percentual no total da BA (que já está em dados_analise_final)
    total_bahia_recente <- dados_analise_final %>% filter(Nivel_Geo=="Estado", Unidade_Territorial=="Bahia") %>% pull(Cabecas_Total_Recente)
    if(length(total_bahia_recente)==1 && total_bahia_recente > 0){
      dados_plot_prop_meso_total <- dados_meso_total_recente %>%
        mutate(Participacao_BA_Perc = (Efetivo_Total / total_bahia_recente) * 100,
               Label = paste0(sprintf("%.1f%%", Participacao_BA_Perc), "\n(", scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = " Mi")(Efetivo_Total), ")"))
      message("      -> Dados para gráfico de proporção MESO no TOTAL BA preparados.")
    } else {message("      -> AVISO: Total da Bahia não encontrado para calcular proporção das mesorregiões.")}
  } else {message("      -> AVISO: Nenhum dado de efetivo total encontrado para mesorregiões em ", ano_plot_recente)}
  
  
  # 2. Dados para Proporção Bovino vs Bubalino DENTRO de cada Mesorregião
  dados_meso_por_animal_recente <- dados_efetivo_combinados %>%
    filter(Nivel_Geo == "Mesorregião (BA)", Ano == ano_plot_recente) %>%
    select(Mesorregiao = Unidade_Territorial, Animal, Efetivo_Animal = Cabecas) %>%
    filter(Efetivo_Animal > 0) # Manter apenas onde há animais daquela espécie
  
  if(nrow(dados_meso_por_animal_recente) > 0) {
    # Calcula a proporção DENTRO de cada mesorregião
    dados_plot_prop_intra_meso <- dados_meso_por_animal_recente %>%
      group_by(Mesorregiao) %>%
      mutate(Total_Meso = sum(Efetivo_Animal, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(Total_Meso > 0) %>% # Garante que a mesorregião tem algum animal
      mutate(Proporcao_Intra_Meso = (Efetivo_Animal / Total_Meso) * 100,
             Label_Num = ifelse(Efetivo_Animal > 1000,
                                scales::label_number(accuracy = 0.1, scale = 1e-3, suffix = " k")(Efetivo_Animal),
                                scales::label_number(accuracy = 1)(Efetivo_Animal)),
             Label_Perc = ifelse(Proporcao_Intra_Meso > 1, # Só mostra % se for relevante
                                 sprintf("%.1f%%", Proporcao_Intra_Meso),
                                 "") # Senão, vazio para não poluir
      )
    message("      -> Dados para gráfico de proporção INTRA-MESORREGIÃO preparados.")
  } else {message("      -> AVISO: Nenhum dado de efetivo por animal encontrado para mesorregiões em ", ano_plot_recente)}
}

# --- GERAR GRÁFICOS ---

# 1. Gráfico: Proporção do Efetivo Total (Bov+Bub) por Mesorregião no Total da BA
if (!is.null(dados_plot_prop_meso_total)) {
  message("\n   -> Gerando Gráfico: Proporção Efetivo Total Mesorregião no Total BA (",ano_plot_recente,")")
  tryCatch({
    plot_prop_meso_total <- ggplot(dados_plot_prop_meso_total, aes(x = "", y = Participacao_BA_Perc, fill = reorder(Mesorregiao, -Participacao_BA_Perc))) +
      geom_col(width = 1, color = "white") + # Barras empilhadas preenchendo 100%
      # Adiciona texto com nome, % e valor absoluto
      geom_text(aes(label = paste(Mesorregiao, Label, sep="\n")),
                position = position_stack(vjust = 0.5), size = 3, color="white", fontface="bold") +
      coord_polar(theta = "y", start=0) + # Transforma em "pizza" (ou rosca se tiver buraco)
      scale_fill_viridis_d(option = "turbo", name="Mesorregião") + # Paleta discreta
      labs(
        title = paste("Distribuição Proporcional do Efetivo Total (Bov+Bub) - Mesorregiões da Bahia", ano_plot_recente),
        subtitle = "Rótulos: Nome da Mesorregião, Participação (%) no Total BA e Efetivo (Milhões)",
        caption = paste("Fonte: IBGE/SIDRA Tabela 3939"),
        x = NULL, y = NULL # Remove rótulos dos eixos
      ) +
      theme_void() + # Remove fundo, eixos, etc.
      theme(legend.position = "none", # Remove legenda (info está no gráfico)
            plot.title = element_text(hjust = 0.5, face = "bold", size=12),
            plot.subtitle = element_text(hjust = 0.5, size = 9))
    
    #print(plot_prop_meso_total)
    ggsave("proporcao_efetivo_total_mesorregioes_ba_recente.png", plot_prop_meso_total, width = 8, height = 8, dpi = 300)
    message("      -> Gráfico de Proporção Total Mesorregião salvo.")
    
  }, error = function(e){ message("      -> ERRO ao gerar Gráfico de Proporção Total Mesorregião: ", e$message) })
} else {
  message("   -> Pular Gráfico de Proporção Total Mesorregião: Dados não preparados.")
}


# 2. Gráfico: Proporção Bovino vs Bubalino DENTRO de cada Mesorregião
if (!is.null(dados_plot_prop_intra_meso)) {
  message("\n   -> Gerando Gráfico: Proporção Intra-Mesorregião (Bovino vs Bubalino) BA (",ano_plot_recente,")")
  tryCatch({
    plot_prop_intra_meso <- ggplot(dados_plot_prop_intra_meso, aes(x = reorder(Mesorregiao, -Total_Meso), y = Proporcao_Intra_Meso, fill = Animal)) +
      geom_col(position = "fill", width = 0.8, color="grey80", linewidth=0.2) + # position="fill" cria a barra 100%
      # Adiciona texto com % DENTRO da barra
      geom_text(aes(label = Label_Perc), # Usa o label percentual calculado
                position = position_fill(vjust = 0.5), size = 2.8, color="white", fontface="bold") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) + # Eixo Y como percentual
      scale_fill_manual(values = c("Bovino" = "darkblue", "Bubalino" = "lightblue"), name="Espécie") +
      labs(
        title = paste("Composição do Efetivo (Bovino vs Bubalino) por Mesorregião - Bahia", ano_plot_recente),
        subtitle = "Barras representam 100% do efetivo em cada mesorregião. Rótulos indicam a % da espécie.",
        x = "Mesorregião", y = "Proporção do Efetivo na Mesorregião" ) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            panel.grid.major.x = element_blank(), # Remove linhas de grade verticais
            panel.grid.minor.y = element_blank()) # Remove linhas de grade menores
    
    #print(plot_prop_intra_meso)
    ggsave("proporcao_intra_mesorregiao_ba_recente.png", plot_prop_intra_meso, width = 10, height = 7, dpi = 300)
    message("      -> Gráfico de Proporção Intra-Mesorregião salvo.")
    
  }, error = function(e){ message("      -> ERRO ao gerar Gráfico de Proporção Intra-Mesorregião: ", e$message) })
} else {
  message("   -> Pular Gráfico de Proporção Intra-Mesorregião: Dados não preparados.")
}

message("\n--- Fim da Seção 6.3 ---")


# --- 6.4 Gráfico Série Histórica: Preço @ Boi Gordo (BA vs BR) - CORRIGIDO Escala ---
# (Código mantido como na Revisão 3 - já estava corrigido para não dividir por 15)
if (!is.null(preco_boi_gordo_combinado)) {
  message("\n   -> Gerando Gráfico Série Histórica: Preço Boi Gordo (BA vs BR) - Escalas Corrigidas...")
  # ... (código do plot_serie_boi como na Revisão 3) ...
  tryCatch({
    df_ba_plot <- preco_boi_gordo_conab_ba %>% mutate(Local_Legenda = "BA (CONAB R$/15kg)")
    df_br_plot <- preco_boi_gordo_cepea_br %>% mutate(Local_Legenda = "BR (CEPEA R$/@)")
    preco_boi_gordo_combinado_plot <- bind_rows(df_ba_plot, df_br_plot)
    plot_serie_boi <- ggplot(preco_boi_gordo_combinado_plot, aes(x = Data, y = Preco_Medio_RS, color = Local_Legenda)) +
      geom_line(linewidth = 1) + geom_point(size = 1.5) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ")) + scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
      scale_color_manual(values = c("BA (CONAB R$/15kg)" = "orange", "BR (CEPEA R$/@)" = "darkred")) +
      labs( title = "Série Histórica - Preço Médio do Boi Gordo", subtitle = "Comparativo Bahia (CONAB, R$/15kg) vs. Brasil (Indicador CEPEA, R$/@)", x = "Mês/Ano", y = "Preço Médio (R$)", color = "Local/Fonte (Unidades Diferentes!)" ) +
      theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
    #print(plot_serie_boi)
    ggsave("serie_historica_preco_boi_gordo_corrigido.png", plot_serie_boi, width = 10, height = 6, dpi = 300)
    message("      -> Gráfico Série Histórica Boi Gordo (corrigido) OK.")
  }, error = function(e){ message("      -> ERRO ao gerar Gráfico Série Histórica Boi Gordo (corrigido): ", e$message) })
} else { message("   -> Pular Gráfico Série Histórica Boi Gordo: Dataframe combinado ausente.")}

# --- 6.5 Gráfico Série Histórica: Preço Bezerro MS ---
# (Manter código da versão anterior)
# ... (código do plot_serie_bezerro) ...
if (!is.null(preco_bezerro_cepea_ms)) {
  message("\n   -> Gerando Gráfico Série Histórica: Preço Bezerro MS...")
  # ... (código ggplot como antes) ...
  tryCatch({
    plot_serie_bezerro <- ggplot(preco_bezerro_cepea_ms, aes(x = Data, y = Preco_Medio_RS)) +
      geom_line(color = "blue", linewidth = 1) + geom_point(color = "blue", size = 1.5) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ")) + scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
      labs( title = "Série Histórica - Preço Médio do Bezerro (Unidade)", subtitle = "Indicador CEPEA/ESALQ - Mato Grosso do Sul (MS)", x = "Mês/Ano", y = "Preço Médio (R$/Unidade)" ) +
      theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    #print(plot_serie_bezerro)
    ggsave("serie_historica_preco_bezerro_ms.png", plot_serie_bezerro, width = 10, height = 5, dpi = 300)
    message("      -> Gráfico Série Histórica Bezerro OK.")
  }, error = function(e){ message("      -> ERRO ao gerar Gráfico Série Histórica Bezerro: ", e$message) })
} else { message("   -> Pular Gráfico Série Histórica Bezerro: Dataframe ausente.")}


# --- 6.6 Mapa Brasil (Estados): Valor da Venda Bovinos 2017 (% e Numérico) - CORRIGIDO % ---
if (!is.null(mapa_estados_dados) && "Valor_Venda_Total" %in% names(mapa_estados_dados) && !all(is.na(mapa_estados_dados$Valor_Venda_Total))) {
  message("\n   -> Gerando Mapa Brasil (Estados): Valor Venda Bovinos 2017 (Percentual Corrigido)...")
  
  # --- CORREÇÃO: Usar o valor total Brasil REAL de dados_analise_final ---
  valor_venda_total_br_real_2017 <- dados_analise_final %>%
    filter(Nivel_Geo == "Brasil") %>%
    pull(Valor_Venda_Total)
  
  # Calcular participação CORRETA apenas se o total Brasil existir
  if(length(valor_venda_total_br_real_2017) == 1 && valor_venda_total_br_real_2017 > 0) {
    mapa_estados_dados_venda <- mapa_estados_dados %>%
      mutate(
        # Calcular participação correta
        Participacao_Venda_2017_Perc_CORRETA = (Valor_Venda_Total / valor_venda_total_br_real_2017) * 100,
        # Tratar NAs que podem surgir se Valor_Venda_Total era NA
        Participacao_Venda_2017_Perc_CORRETA = replace_na(Participacao_Venda_2017_Perc_CORRETA, 0),
        Valor_Venda_Total = replace_na(Valor_Venda_Total, 0)
      )
    
    tryCatch({
      mapa_plot_venda_estado_2017 <- ggplot(mapa_estados_dados_venda) +
        # Usar a coluna CORRETA para o fill
        geom_sf(aes(fill = Participacao_Venda_2017_Perc_CORRETA), color = "white", linewidth = 0.1) +
        geom_sf_text(
          aes(label = ifelse(Valor_Venda_Total > 0,
                             paste0(sprintf("%.1f%%", Participacao_Venda_2017_Perc_CORRETA), # Usar a coluna CORRETA
                                    "\n",
                                    scales::label_number(accuracy = 0.1, scale = 1e-9, suffix = " Bi")(Valor_Venda_Total)),
                             "")),
          color = "green4", size = 3.5, fun.geometry = sf::st_centroid, check_overlap = TRUE ) +
        scale_fill_viridis_c(
          option = "rocket", direction = direcao_cores,
          name = paste0("Participação (%)\nValor Venda\nBovinos 2017"),
          labels = scales::percent_format(scale = 1, accuracy=1),
          guide = guide_colorbar(barwidth = 0.8, barheight = 10) ) +
        labs(
          title = "Participação e Valor da Venda de Bovinos por Estado - 2017",
          subtitle = "Rótulos: Participação (% do Total BR) e Valor (Bilhões R$)",
          caption = paste("Fonte: IBGE Censo Agropecuário 2017 (Tabela 8986/8987). Mapa:", ano_referencia_mapas) ) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "right", legend.title = element_text(size = 8), legend.text = element_text(size = 7) )
      
      #print(mapa_plot_venda_estado_2017)
      ggsave("mapa_brasil_estados_venda_bovinos_2017_corrigido.png", mapa_plot_venda_estado_2017, width = 10, height = 9, dpi = 300)
      message("      -> Mapa Valor Venda 2017 (Estados, % corrigido) salvo.")
    }, error = function(e){ message("      -> ERRO ao gerar Mapa Valor Venda 2017 (Estados, % corrigido): ", e$message) })
    
  } else {
    message("   -> Pular Mapa Valor Venda 2017 (Estados): Total Brasil não encontrado ou zero.")
  }
  
} else { message("   -> Pular Mapa Valor Venda 2017 (Estados): Objeto espacial ou coluna Valor_Venda_Total ausente/NA.")}


# --- 6.7 Gráfico Comparativo Venda 2017 - Bahia vs Total BR vs Média BR ---
# (Manter código da versão anterior - já estava correto)
# ... (código plot_comp_venda_media_2017 como antes) ...
if (!is.null(dados_analise_final) && "Valor_Venda_Total" %in% names(dados_analise_final) && !all(is.na(dados_analise_final$Valor_Venda_Total))) {
  message("\n   -> Gerando Gráfico Comparativo: Valor Venda 2017 (Bahia vs Total BR vs Média Estados)...")
  # ... (código ggplot como antes) ...
  tryCatch({
    valor_venda_bahia_2017 <- dados_analise_final %>% filter(Nivel_Geo == "Estado" & Unidade_Territorial == "Bahia" & !is.na(Valor_Venda_Total)) %>% pull(Valor_Venda_Total)
    valor_venda_total_br_2017 <- dados_analise_final %>% filter(Nivel_Geo == "Brasil" & !is.na(Valor_Venda_Total)) %>% pull(Valor_Venda_Total)
    valor_venda_media_estados_2017 <- dados_analise_final %>% filter(Nivel_Geo == "Estado" & !is.na(Valor_Venda_Total)) %>% summarise(Media = mean(Valor_Venda_Total, na.rm = TRUE)) %>% pull(Media)
    if(length(valor_venda_bahia_2017)==1 && length(valor_venda_total_br_2017)==1 && length(valor_venda_media_estados_2017)==1){
      dados_comp_venda_media_2017 <- tibble( Local = factor(c("Bahia", "Média Estados", "Brasil Total"), levels=c("Bahia", "Média Estados", "Brasil Total")), Valor = c(valor_venda_bahia_2017, valor_venda_media_estados_2017, valor_venda_total_br_2017) ) %>% mutate(Label = scales::dollar(Valor, scale=1e-9, accuracy=0.1, suffix=" Bi"))
      plot_comp_venda_media_2017 <- ggplot(dados_comp_venda_media_2017, aes(x = Local, y = Valor, fill = Local)) + geom_col(show.legend = FALSE) + geom_text(aes(label = Label), vjust = -0.5, size = 3.5) + scale_y_continuous(labels = scales::dollar_format(scale = 1e-9, suffix = " Bilhões")) + scale_fill_manual(values = c("Bahia" = "orange", "Média Estados" = "lightblue", "Brasil Total" = "darkred")) + labs( title = "Valor da Venda de Bovinos - 2017", subtitle = "Comparação: Bahia vs. Média dos Estados vs. Total Brasil", x = "", y = "Valor da Venda (R$)" ) + theme_minimal(base_size = 12) + theme( plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 10) )
      #print(plot_comp_venda_media_2017)
      ggsave("comparativo_venda_bovinos_2017_ba_br_media.png", plot_comp_venda_media_2017, width = 7, height = 5, dpi = 300)
      message("      -> Gráfico Comparativo de Venda 2017 (com média) OK.")
    } else { message("      -> AVISO: Não foi possível gerar gráfico comparativo com média (Dados insuficientes).") }
  }, error = function(e){ message("      -> ERRO ao gerar Gráfico Comparativo de Venda 2017 (com média): ", e$message) })
} else { message("   -> Pular Gráfico Comparativo de Venda 2017 (com média): Dados de valor da venda ausentes ou NA.")}

# --- Gráfico Comparativo Efetivo Mesorregiões (6.8) ---
# (Manter código da versão anterior - já estava correto)
# ... (código plot_efetivo_meso_comp como antes) ...
if(plotar_comp_meso) { # Reutiliza a flag definida em 6.3
  message("\n   -> Gerando Gráfico Comparativo: Efetivo Mesorregiões BA (2017 vs ",ano_plot_recente,")")
  # ... (código ggplot como antes) ...
  tryCatch({
    plot_efetivo_meso_comp <- ggplot(dados_plot_efetivo_meso_comp, aes(x = reorder(Mesorregiao, -Efetivo), y = Efetivo, fill = Ano)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = Label, y = Efetivo), position = position_dodge(width = 0.8), vjust = -0.3, size = 2.8) +
      scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " Milhões")) +
      scale_fill_manual(values = c("2017" = "grey70", !!as.character(ano_plot_recente) := "steelblue")) +
      labs( title = paste("Comparativo Efetivo Total (Bov+Bub) por Mesorregião da Bahia:", ano_plot_recente, "vs 2017"), x = "Mesorregião", y = "Efetivo Total (Cabeças)", fill = "Ano" ) +
      theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
    #print(plot_efetivo_meso_comp)
    ggsave("comparativo_efetivo_mesorregioes_ba_2017_vs_recente.png", plot_efetivo_meso_comp, width = 11, height = 7, dpi = 300)
    message("      -> Gráfico Comparativo Efetivo Mesorregiões BA OK.")
  }, error = function(e){ message("      -> ERRO ao gerar Gráfico Comparativo Efetivo Mesorregiões BA: ", e$message) })
} else { message("   -> Pular Gráfico Comparativo Efetivo Mesorregiões BA: Dados ausentes.")}


message("\n--- Fim da Parte 6: Visualizações ---")

#Infos do sidra para futuras verificações

print(info_sidra(8986))
print(info_sidra(8987))
print(info_sidra(8985))
