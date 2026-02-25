# ==============================================================================
# SCRIPT: BIBLIOMETRIA AVANCADA E RELATORIO AUTOMATIZADO
# Versao: 8.1 (Dicionario completo + dupla codificacao visual cor/forma)
# ==============================================================================

rm(list = ls())
graphics.off()
options(stringsAsFactors = FALSE)

# ==============================================================================
# 1. CONFIGURACAO
# ==============================================================================

CONFIG <- list(
  interativo        = TRUE,
  objetivo_estudo   = "Estimativas de herdabilidade para caracteristicas de",
  parametro_av      = "longevidade",
  especie_estudo    = "bovinos",
  tipo_producao     = "leiteiros",
  arquivo_dados     = "",
  col_foco          = "raca_agrupada",
  caminho_base      = "E:/Documentos/UFBA/PASTA IC/PROJETOS/LONGEVIDADE RUM/PLANILHAS",
  dpi               = 600,
  largura_dupla     = 180,
  largura_unica     = 85,
  max_autores_bar   = 20,
  max_autores_tl    = 15,
  max_legend_grupos = 12,
  wrap_titulo       = 55,
  wrap_subtitulo    = 75
)

# ==============================================================================
# 2. LABELS (Unicode escapes)
# ==============================================================================

L <- list(
  console_titulo    = "BIBLIOMETRIA - REVIS\u00c3O SISTEM\u00c1TICA",
  console_objetivo  = "Objetivo (ex: Estimativas de herdabilidade para caracter\u00edsticas de): ",
  console_parametro = "Par\u00e2metro avaliado (ex: longevidade): ",
  console_especie   = "Esp\u00e9cie (ex: bovinos): ",
  console_producao  = "Sistema de produ\u00e7\u00e3o (ex: leiteiros): ",
  console_arquivo   = "Digite o nome do arquivo: ",
  console_variavel  = "Vari\u00e1vel de agrupamento: ",
  step_importacao   = "IMPORTA\u00c7\u00c3O DE DADOS",
  step_padronizacao = "PADRONIZA\u00c7\u00c3O GEOGR\u00c1FICA",
  step_variavel     = "VARI\u00c1VEL DE AGRUPAMENTO",
  step_figuras      = "GERANDO FIGURAS",
  step_autoria      = "AN\u00c1LISE DE AUTORIA",
  step_tabelas      = "GERANDO TABELAS",
  step_relatorio    = "GERANDO RELAT\u00d3RIO",
  step_concluido    = "AN\u00c1LISE CONCLU\u00cdDA",
  msg_arq_disp      = "Arquivos dispon\u00edveis:",
  msg_pasta         = "Pasta de busca:",
  msg_paises_pri    = "Pa\u00edses \u00fanicos (prim\u00e1rio):",
  msg_paises_exp    = "Pa\u00edses \u00fanicos (expandido):",
  msg_sem_corresp   = "Sem correspond\u00eancia no mapa:",
  msg_saida         = "Sa\u00edda:",
  msg_cat           = "categorias",
  msg_reg           = "registros,",
  msg_var           = "vari\u00e1veis",
  msg_agrup         = "Agrupamento:",
  msg_nao_enc       = "n\u00e3o encontrada. Usando",
  msg_arq_enc       = "Arquivo encontrado:",
  msg_cam_nf        = "Caminho n\u00e3o encontrado. Usando pasta atual.",
  msg_sem_arq       = "Nenhum arquivo .xlsx/.xls/.csv encontrado em: ",
  msg_arq_nf        = "Arquivo n\u00e3o encontrado: ",
  msg_verifique     = "Verifique o nome e tente novamente.",
  msg_sem_var       = "Nenhuma vari\u00e1vel de agrupamento encontrada.",
  msg_verif_pais    = "Verifique a grafia na coluna 'pais'.",
  sub_prefixo       = "Revis\u00e3o Sistem\u00e1tica: ",
  sub_em            = " em ",
  fig1_title   = "Distribui\u00e7\u00e3o Geogr\u00e1fica da Produ\u00e7\u00e3o Cient\u00edfica",
  fig1_legend  = "N\u00ba de Artigos",
  fig1_caption = "Estudos multi-pa\u00eds contabilizados para cada pa\u00eds contribuinte",
  fig1_log     = "Mapa de distribui\u00e7\u00e3o geogr\u00e1fica...",
  fig2_title   = "Evolu\u00e7\u00e3o Temporal das Publica\u00e7\u00f5es",
  fig2_y_anual = "Publica\u00e7\u00f5es Anuais",
  fig2_y_acum  = "Total Acumulado",
  fig2_x       = "Ano de Publica\u00e7\u00e3o",
  fig2_caption = "Superior: publica\u00e7\u00f5es anuais | Inferior: total acumulado",
  fig2_log     = "Evolu\u00e7\u00e3o temporal...",
  fig3_pre     = "Tend\u00eancias Temporais por ",
  fig3_x       = "Ano de Publica\u00e7\u00e3o",
  fig3_y       = "N\u00famero de Publica\u00e7\u00f5es",
  fig3_caption = "Dupla codifica\u00e7\u00e3o: cor + forma geom\u00e9trica por grupo",
  fig3_log     = "Tend\u00eancias por grupo...",
  fig4_pre     = "Composi\u00e7\u00e3o das Publica\u00e7\u00f5es por ",
  fig4_x       = "Ano de Publica\u00e7\u00e3o",
  fig4_y       = "N\u00famero de Publica\u00e7\u00f5es",
  fig4_caption = "\u00c1rea proporcional ao volume de cada grupo",
  fig4_log     = "Composi\u00e7\u00e3o temporal...",
  fig5_title   = "Autores com Maior Produ\u00e7\u00e3o Cient\u00edfica",
  fig5_x       = "Primeiro Autor",
  fig5_y       = "N\u00famero de Publica\u00e7\u00f5es",
  fig5_caption = "Baseado no sobrenome do primeiro autor",
  fig5_log     = "Ranking de autores...",
  fig6_title   = "Per\u00edodo de Atividade dos Principais Autores",
  fig6_x       = "Ano de Publica\u00e7\u00e3o",
  fig6_y       = "Primeiro Autor",
  fig6_caption = "Verde: 1\u00aa pub | Vermelho: \u00faltima | n = total de artigos",
  fig6_log     = "Timeline de autores...",
  outros       = "Outros"
)

# ==============================================================================
# 3. PACOTES
# ==============================================================================

pacotes <- c("readxl", "openxlsx", "dplyr", "tidyr", "ggplot2",
             "stringr", "scales", "RColorBrewer", "maps", "patchwork")

invisible(lapply(pacotes, function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}))

# ==============================================================================
# 4. FUNCOES AUXILIARES
# ==============================================================================

# --- 4.1 Tema para publicacao ---
tema_pub <- function(base_size = 10, rotacao_x = 0) {
  t <- theme_bw(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.2), hjust = 0,
                                      margin = margin(b = 4)),
      plot.subtitle    = element_text(size = rel(0.9), face = "italic",
                                      color = "gray30", hjust = 0,
                                      margin = margin(b = 8)),
      plot.caption     = element_text(size = rel(0.75), face = "italic",
                                      hjust = 0, margin = margin(t = 8)),
      axis.title       = element_text(size = rel(1.0), face = "bold"),
      axis.text        = element_text(size = rel(0.9)),
      legend.title     = element_text(size = rel(0.9), face = "bold"),
      legend.text      = element_text(size = rel(0.8)),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = rel(1.0)),
      plot.margin      = margin(8, 8, 8, 8, "mm")
    )
  if (rotacao_x != 0) {
    t <- t + theme(axis.text.x = element_text(
      angle = rotacao_x, hjust = if (rotacao_x > 0) 1 else 0))
  }
  t
}

# --- 4.2 Salvar figura ---
salvar_fig <- function(plot, filename, dir_saida,
                       width = CONFIG$largura_dupla, height = 130) {
  fp   <- file.path(dir_saida, filename)
  args <- list(filename = fp, plot = plot, width = width, height = height,
               units = "mm", dpi = CONFIG$dpi, bg = "white")
  if (grepl("\\.tiff?$", filename, ignore.case = TRUE)) args$compression <- "lzw"
  do.call(ggsave, args)
  cat("    [OK]", filename, "\n")
}

# --- 4.3 Operacoes seguras ---
safe_min   <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else min(x) }
safe_max   <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else max(x) }
safe_mean  <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else mean(x) }
safe_sd    <- function(x) { x <- x[is.finite(x)]; if (length(x) < 2) NA_real_ else sd(x) }
safe_index <- function(v, i, d = "N/D") {
  if (length(v) >= i && !is.na(v[i])) as.character(v[i]) else d
}

# --- 4.4 Resumo estatistico ---
resumo_grupo <- function(df, gcol, h2col = "h2", idcol = "autoria") {
  df %>%
    filter(!is.na(.data[[gcol]])) %>%
    group_by(Group = .data[[gcol]]) %>%
    summarise(
      N_Articles  = n_distinct(.data[[idcol]]),
      N_Estimates = n(),
      Mean_h2 = round(safe_mean(.data[[h2col]]), 3),
      SD_h2   = round(safe_sd(.data[[h2col]]),   3),
      Min_h2  = round(safe_min(.data[[h2col]]),   3),
      Max_h2  = round(safe_max(.data[[h2col]]),   3),
      .groups = "drop"
    ) %>%
    arrange(desc(N_Articles))
}

# --- 4.5 Breaks quinquenais ---
breaks_5y <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (any(!is.finite(r))) return(waiver())
  seq(floor(r[1] / 5) * 5, ceiling(r[2] / 5) * 5, by = 5)
}

# --- 4.6 Paleta de cores escalavel ---
paleta_esc <- function(n, nome = "Set1") {
  mx <- brewer.pal.info[nome, "maxcolors"]
  if (n <= mx) brewer.pal(max(n, 3), nome)[seq_len(n)]
  else colorRampPalette(brewer.pal(mx, nome))(n)
}

# --- 4.7 Formas geometricas distintas (dupla codificacao visual) ---
FORMAS_DISTINTAS <- c(
  16,  # circulo preenchido
  17,  # triangulo preenchido
  15,  # quadrado preenchido
  18,  # losango preenchido
  8,  # asterisco
  3,  # cruz +
  4,  # cruz x
  7,  # quadrado com x
  9,  # losango com +
  10,  # circulo com +
  11,  # estrela (triangulos)
  13,  # circulo com x
  14,  # quadrado com triangulo
  0,  # quadrado vazio
  1,  # circulo vazio
  2,  # triangulo vazio
  5,  # losango vazio
  6,  # triangulo invertido
  12   # quadrado com +
)

formas_esc <- function(n) rep(FORMAS_DISTINTAS, length.out = n)

# --- 4.8 Dicionario COMPLETO de paises ---
#    Chaves: formas comuns em publicacoes (ingles, portugues, espanhol, codigos)
#    Valores: nomes compativeis com map_data("world")$region

DICT_PAISES <- c(
  
  # --- AMERICA DO NORTE ---
  "US" = "USA", "U.S." = "USA", "U.S.A." = "USA",
  "EUA" = "USA", "United States" = "USA",
  "United States of America" = "USA",
  "Estados Unidos" = "USA",
  
  "Canad\u00e1" = "Canada", "CA" = "Canada",
  
  "M\u00e9xico" = "Mexico", "Mexico" = "Mexico", "MX" = "Mexico",
  
  # --- AMERICA CENTRAL E CARIBE ---
  "Costa Rica" = "Costa Rica", "CR" = "Costa Rica",
  "Panam\u00e1" = "Panama", "Panama" = "Panama", "PA" = "Panama",
  "Cuba" = "Cuba", "CU" = "Cuba",
  "Honduras" = "Honduras", "HN" = "Honduras",
  "Guatemala" = "Guatemala", "GT" = "Guatemala",
  "El Salvador" = "El Salvador", "SV" = "El Salvador",
  "Nicaragua" = "Nicaragua", "NI" = "Nicaragua",
  "Jamaica" = "Jamaica", "JM" = "Jamaica",
  "Trinidad and Tobago" = "Trinidad", "Trinidad" = "Trinidad",
  "Rep\u00fablica Dominicana" = "Dominican Republic",
  "Dominican Republic" = "Dominican Republic",
  "Haiti" = "Haiti", "Hait\u00ed" = "Haiti",
  
  # --- AMERICA DO SUL ---
  "Brasil" = "Brazil", "BR" = "Brazil",
  "AR" = "Argentina",
  "CL" = "Chile",
  "Col\u00f4mbia" = "Colombia", "CO" = "Colombia",
  "UY" = "Uruguay",
  "Per\u00fa" = "Peru", "Peru" = "Peru", "PE" = "Peru",
  "VE" = "Venezuela",
  "Equador" = "Ecuador", "EC" = "Ecuador",
  "Bol\u00edvia" = "Bolivia", "BO" = "Bolivia",
  "Paraguai" = "Paraguay", "PY" = "Paraguay",
  "Guiana" = "Guyana", "GY" = "Guyana",
  "Suriname" = "Suriname", "SR" = "Suriname",
  
  # --- EUROPA OCIDENTAL ---
  "United Kingdom" = "UK", "Great Britain" = "UK", "England" = "UK",
  "Reino Unido" = "UK", "GB" = "UK", "GBR" = "UK",
  "Scotland" = "UK", "Wales" = "UK",
  
  "Deutschland" = "Germany", "Alemanha" = "Germany",
  "Alemania" = "Germany", "DE" = "Germany",
  
  "Fran\u00e7a" = "France", "Francia" = "France", "FR" = "France",
  
  "Italia" = "Italy", "It\u00e1lia" = "Italy", "IT" = "Italy",
  
  "Espa\u00f1a" = "Spain", "Espanha" = "Spain", "ES" = "Spain",
  
  "PT" = "Portugal",
  
  "Holland" = "Netherlands", "Holanda" = "Netherlands",
  "Pa\u00edses Baixos" = "Netherlands", "NL" = "Netherlands",
  "The Netherlands" = "Netherlands",
  
  "B\u00e9lgica" = "Belgium", "Belgi\u00eb" = "Belgium",
  "Belgique" = "Belgium", "BE" = "Belgium",
  
  "Su\u00ed\u00e7a" = "Switzerland", "Suiza" = "Switzerland",
  "Suisse" = "Switzerland", "Schweiz" = "Switzerland", "CH" = "Switzerland",
  
  "\u00d6sterreich" = "Austria", "\u00c1ustria" = "Austria", "AT" = "Austria",
  
  "Luxemburgo" = "Luxembourg", "LU" = "Luxembourg",
  
  # --- ILHAS BRITANICAS ---
  "Irlanda" = "Ireland", "IE" = "Ireland",
  "\u00c9ire" = "Ireland",
  
  # --- ESCANDINAVIA ---
  "Su\u00e9cia" = "Sweden", "Sverige" = "Sweden", "SE" = "Sweden",
  "Noruega" = "Norway", "Norge" = "Norway", "NO" = "Norway",
  "Dinamarca" = "Denmark", "Danmark" = "Denmark", "DK" = "Denmark",
  "Finl\u00e2ndia" = "Finland", "Suomi" = "Finland", "FI" = "Finland",
  "Isl\u00e2ndia" = "Iceland", "Islandia" = "Iceland", "IS" = "Iceland",
  
  # --- EUROPA CENTRAL E ORIENTAL ---
  "Pol\u00f4nia" = "Poland", "Polonia" = "Poland",
  "Polska" = "Poland", "PL" = "Poland",
  
  "Czech Republic" = "Czech Republic", "Czech Rep." = "Czech Republic",
  "Czechia" = "Czech Republic", "CZ" = "Czech Republic",
  "Rep\u00fablica Tcheca" = "Czech Republic",
  "Rep\u00fablica Checa" = "Czech Republic",
  
  "Eslov\u00e1quia" = "Slovakia", "Eslovaquia" = "Slovakia", "SK" = "Slovakia",
  "Hungria" = "Hungary", "HU" = "Hungary",
  "Rom\u00eania" = "Romania", "Rumania" = "Romania",
  "Ruman\u00eda" = "Romania", "RO" = "Romania",
  "Bulg\u00e1ria" = "Bulgaria", "BG" = "Bulgaria",
  "Eslov\u00eania" = "Slovenia", "Eslovenia" = "Slovenia", "SI" = "Slovenia",
  "Cro\u00e1cia" = "Croatia", "Croacia" = "Croatia", "HR" = "Croatia",
  "S\u00e9rvia" = "Serbia", "RS" = "Serbia",
  "Bosnia and Herzegovina" = "Bosnia and Herzegovina",
  "B\u00f3snia" = "Bosnia and Herzegovina",
  
  "Ucr\u00e2nia" = "Ukraine", "Ucrania" = "Ukraine", "UA" = "Ukraine",
  "R\u00fassia" = "Russia", "Rusia" = "Russia", "RU" = "Russia",
  "Russian Federation" = "Russia",
  "Bielorr\u00fassia" = "Belarus", "Bielorrusia" = "Belarus",
  "BY" = "Belarus",
  
  "Litu\u00e2nia" = "Lithuania", "Lituania" = "Lithuania", "LT" = "Lithuania",
  "Let\u00f4nia" = "Latvia", "Letonia" = "Latvia", "LV" = "Latvia",
  "Est\u00f4nia" = "Estonia", "EE" = "Estonia",
  
  # --- EUROPA MERIDIONAL ---
  "Gr\u00e9cia" = "Greece", "Grecia" = "Greece", "GR" = "Greece",
  "Turquia" = "Turkey", "T\u00fcrkiye" = "Turkey",
  "Turqu\u00eda" = "Turkey", "TR" = "Turkey",
  "Chipre" = "Cyprus", "CY" = "Cyprus",
  
  # --- ASIA ORIENTAL ---
  "South Korea" = "South Korea", "S. Korea" = "South Korea",
  "Korea" = "South Korea", "KR" = "South Korea",
  "Coreia do Sul" = "South Korea", "Corea del Sur" = "South Korea",
  "Republic of Korea" = "South Korea",
  "North Korea" = "North Korea", "Coreia do Norte" = "North Korea",
  
  "Jap\u00e3o" = "Japan", "Jap\u00f3n" = "Japan", "JP" = "Japan",
  "CN" = "China",
  "Taiwan" = "Taiwan", "TW" = "Taiwan",
  "Mong\u00f3lia" = "Mongolia",
  
  # --- ASIA MERIDIONAL E SUDESTE ---
  "\u00cdndia" = "India", "IN" = "India",
  "Paquist\u00e3o" = "Pakistan", "Pakist\u00e1n" = "Pakistan", "PK" = "Pakistan",
  "BD" = "Bangladesh",
  "NP" = "Nepal",
  "Tail\u00e2ndia" = "Thailand", "Tailandia" = "Thailand", "TH" = "Thailand",
  "Vietn\u00e3" = "Vietnam", "VN" = "Vietnam",
  "Indon\u00e9sia" = "Indonesia", "ID" = "Indonesia",
  "Mal\u00e1sia" = "Malaysia", "Malasia" = "Malaysia", "MY" = "Malaysia",
  "Filipinas" = "Philippines", "PH" = "Philippines",
  
  # --- ORIENTE MEDIO ---
  "Ir\u00e3" = "Iran", "IR" = "Iran",
  "Iraque" = "Iraq", "IQ" = "Iraq",
  "IL" = "Israel",
  "Ar\u00e1bia Saudita" = "Saudi Arabia", "Arabia Saudita" = "Saudi Arabia",
  "SA" = "Saudi Arabia",
  "Emirados \u00c1rabes Unidos" = "United Arab Emirates",
  "UAE" = "United Arab Emirates",
  "Jord\u00e2nia" = "Jordan", "Jordania" = "Jordan", "JO" = "Jordan",
  "L\u00edbano" = "Lebanon", "Libano" = "Lebanon", "LB" = "Lebanon",
  "Om\u00e3" = "Oman",
  "Catar" = "Qatar", "QA" = "Qatar",
  
  # --- OCEANIA ---
  "Austr\u00e1lia" = "Australia", "AU" = "Australia",
  "Nova Zel\u00e2ndia" = "New Zealand", "Nueva Zelanda" = "New Zealand",
  "NZ" = "New Zealand",
  
  # --- AFRICA ---
  "\u00c1frica do Sul" = "South Africa", "Sudafrica" = "South Africa",
  "Sud\u00e1frica" = "South Africa", "ZA" = "South Africa",
  
  "Qu\u00eania" = "Kenya", "Kenia" = "Kenya", "KE" = "Kenya",
  "Eti\u00f3pia" = "Ethiopia", "Etiop\u00eda" = "Ethiopia", "ET" = "Ethiopia",
  "Nig\u00e9ria" = "Nigeria", "NG" = "Nigeria",
  "Egito" = "Egypt", "Egipto" = "Egypt", "EG" = "Egypt",
  "Marrocos" = "Morocco", "Marruecos" = "Morocco", "MA" = "Morocco",
  "Tun\u00edsia" = "Tunisia", "T\u00fanez" = "Tunisia", "TN" = "Tunisia",
  "Arg\u00e9lia" = "Algeria", "Argelia" = "Algeria", "DZ" = "Algeria",
  "Gana" = "Ghana", "GH" = "Ghana",
  "Tanz\u00e2nia" = "Tanzania", "TZ" = "Tanzania",
  "UG" = "Uganda",
  "Camar\u00f5es" = "Cameroon", "Camer\u00fan" = "Cameroon", "CM" = "Cameroon",
  "Mo\u00e7ambique" = "Mozambique", "MZ" = "Mozambique",
  "Zimb\u00e1bue" = "Zimbabwe", "Zimbabue" = "Zimbabwe", "ZW" = "Zimbabwe",
  "Costa do Marfim" = "Ivory Coast", "C\u00f4te d'Ivoire" = "Ivory Coast",
  "Cote d'Ivoire" = "Ivory Coast",
  "SN" = "Senegal",
  "Namib\u00eda" = "Namibia", "Namibia" = "Namibia", "NA" = "Namibia",
  "Botsuana" = "Botswana", "BW" = "Botswana",
  "Mad" = "Madagascar", "Madagascar" = "Madagascar"
)

# --- 4.9 Padronizar pais (match exato + fuzzy fallback) ---
padronizar_pais <- function(pais) {
  pais <- str_trim(pais)
  if (is.na(pais) || pais == "") return(NA_character_)
  
  # 1) Match exato case-insensitive no dicionario
  idx <- match(tolower(pais), tolower(names(DICT_PAISES)))
  if (!is.na(idx)) return(unname(DICT_PAISES[idx]))
  
  # 2) Match exato case-insensitive nos nomes do mapa (ja esta correto)
  regioes <- .regioes_mapa_cache
  idx2 <- match(tolower(pais), tolower(regioes))
  if (!is.na(idx2)) return(regioes[idx2])
  
  # 3) Match parcial (ex: "Czech" encontra "Czech Republic")
  parcial <- grep(pais, regioes, ignore.case = TRUE, value = TRUE)
  if (length(parcial) == 1) return(parcial)
  
  # 4) Retorna original
  return(pais)
}

# --- 4.10 Expandir estudos multi-pais ---
expandir_paises <- function(df, col = "pais") {
  df %>%
    mutate(.raw = .data[[col]]) %>%
    mutate(.lista = str_split(.raw, "[,;&/]|\\band\\b")) %>%
    unnest(.lista) %>%
    mutate(.lista = str_trim(.lista)) %>%
    filter(!is.na(.lista) & .lista != "") %>%
    mutate(pais_expandido = sapply(.lista, padronizar_pais)) %>%
    select(-.raw, -.lista)
}

# --- 4.11 Extrair primeiro autor (com particulas) ---
extrair_primeiro_autor <- function(autoria) {
  if (is.na(autoria) || autoria == "") return(NA_character_)
  
  particulas <- c("van", "von", "de", "del", "della", "di", "da", "das",
                  "dos", "du", "la", "le", "el", "al", "bin", "ibn",
                  "den", "der", "het", "ten", "ter", "st")
  
  limpo <- autoria %>%
    str_remove_all("[0-9]+$") %>%
    str_remove_all("[\\*\\+#\\$\\^~]") %>%
    str_remove_all("\\(.*?\\)|\\[.*?\\]") %>%
    str_remove("(?i)\\s+et\\s+al\\.?") %>%
    str_trim()
  
  if (nchar(limpo) == 0) return(NA_character_)
  
  if (str_detect(limpo, ",")) {
    sobrenome <- str_trim(str_extract(limpo, "^[^,]+"))
  } else if (str_detect(limpo, "^([A-Z]\\.?\\s+)+[A-Z][a-z]")) {
    sobrenome <- str_remove(limpo, "^(\\s*[A-Z]\\.?\\s+)+") %>% str_trim()
  } else {
    palavras <- str_split(limpo, "\\s+")[[1]]
    palavras <- palavras[nchar(palavras) > 0]
    if (length(palavras) <= 1) {
      sobrenome <- limpo
    } else {
      idx <- length(palavras)
      while (idx > 1 && tolower(palavras[idx - 1]) %in% particulas) idx <- idx - 1
      sobrenome <- paste(palavras[idx:length(palavras)], collapse = " ")
    }
  }
  
  sobrenome <- str_remove_all(sobrenome, "[^A-Za-z\\s\\-']")
  sobrenome <- str_trim(sobrenome)
  if (nchar(sobrenome) < 2) return(NA_character_)
  tools::toTitleCase(tolower(sobrenome))
}

# --- 4.12 Escrever UTF-8 ---
escrever_utf8 <- function(texto, filepath) {
  con <- file(filepath, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(texto, con, useBytes = FALSE)
}

# --- 4.13 Colapsar grupos pequenos ---
colapsar_ts <- function(df, top_n = 12, label = "Outros") {
  ranking <- df %>%
    group_by(Group) %>%
    summarise(total = sum(n_articles, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total))
  if (nrow(ranking) <= top_n) return(df)
  top <- head(ranking$Group, top_n)
  df %>%
    mutate(Group = ifelse(Group %in% top, Group, label)) %>%
    group_by(Year, Group) %>%
    summarise(n_articles = sum(n_articles, na.rm = TRUE), .groups = "drop")
}

# --- 4.14 Nome seguro para pasta ---
nome_seguro <- function(x) make.names(gsub("[^A-Za-z0-9]", ".", x))

# ==============================================================================
# 5. FUNCOES DE FIGURAS
# ==============================================================================

# --- 5.1 Mapa global ---
fig_mapa <- function(df_mapa, subtitulo, L, config) {
  if (nrow(df_mapa) == 0) return(NULL)
  
  mj <- map_data("world") %>%
    left_join(df_mapa, by = c("region" = "pais_expandido"))
  
  ggplot(mj, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = n_articles), color = "gray70", linewidth = 0.1) +
    scale_fill_gradientn(
      colors   = c("#F7FBFF", "#A6DBEF", "#6BAED6", "#2171B5", "#084594"),
      na.value = "#F0F0F0",
      name     = L$fig1_legend,
      breaks   = pretty_breaks(n = 5),
      guide    = guide_colorbar(
        direction = "horizontal", title.position = "top", title.hjust = 0.5,
        barwidth = unit(60, "mm"), barheight = unit(3, "mm"))
    ) +
    coord_fixed(1.3, xlim = c(-180, 180), ylim = c(-60, 85)) +
    labs(title    = str_wrap(L$fig1_title, config$wrap_titulo),
         subtitle = subtitulo,
         caption  = L$fig1_caption) +
    theme_void(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 12, hjust = 0.5,
                                      margin = margin(b = 3)),
      plot.subtitle    = element_text(size = 9, hjust = 0.5, face = "italic",
                                      color = "gray40", margin = margin(b = 8)),
      plot.caption     = element_text(size = 7, hjust = 0.5, face = "italic",
                                      color = "gray50", margin = margin(t = 4)),
      legend.position  = "bottom",
      legend.title     = element_text(size = 8, face = "bold"),
      legend.text      = element_text(size = 7),
      plot.margin      = margin(5, 5, 5, 5, "mm"),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#F0F8FF", color = NA)
    )
}

# --- 5.2 Evolucao temporal (dois paineis) ---
fig_temporal <- function(df_tempo, subtitulo, L, config) {
  if (nrow(df_tempo) < 3) return(NULL)
  
  p1 <- ggplot(df_tempo, aes(ano, n_articles)) +
    geom_col(fill = "#4292C6", alpha = 0.85, width = 0.7) +
    scale_x_continuous(breaks = breaks_5y(df_tempo$ano)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = L$fig2_y_anual) +
    tema_pub(rotacao_x = 45)
  
  p2 <- ggplot(df_tempo, aes(ano, acumulado)) +
    geom_line(color = "#D7301F", linewidth = 1) +
    geom_point(color = "#D7301F", size = 1.5) +
    scale_x_continuous(breaks = breaks_5y(df_tempo$ano)) +
    labs(x = L$fig2_x, y = L$fig2_y_acum) +
    tema_pub(rotacao_x = 45)
  
  p1 / p2 +
    plot_annotation(
      title    = str_wrap(L$fig2_title, config$wrap_titulo),
      subtitle = subtitulo,
      caption  = L$fig2_caption,
      theme    = theme(
        plot.title    = element_text(face = "bold", size = 12, hjust = 0),
        plot.subtitle = element_text(size = 9, face = "italic", color = "gray30"),
        plot.caption  = element_text(size = 8, face = "italic", hjust = 0)
      )
    )
}

# --- 5.3 Tendencias por grupo (DUPLA CODIFICACAO: cor + forma) ---
fig_tendencias <- function(df, col_titulo, subtitulo, L, config) {
  if (nrow(df) == 0) return(NULL)
  
  df   <- colapsar_ts(df, config$max_legend_grupos, L$outros)
  ng   <- n_distinct(df$Group)
  nleg <- ceiling(ng / 4)
  
  cores  <- paleta_esc(ng, "Set1")
  formas <- formas_esc(ng)
  
  ordem <- df %>% count(Group, wt = n_articles, sort = TRUE) %>% pull(Group)
  df$Group <- factor(df$Group, levels = ordem)
  
  ggplot(df, aes(Year, n_articles, color = Group, shape = Group)) +
    geom_line(aes(linetype = Group), linewidth = 0.8) +
    geom_point(size = 2.8, stroke = 0.6) +
    scale_x_continuous(breaks = breaks_5y(df$Year)) +
    scale_color_manual(values = cores, name = col_titulo) +
    scale_shape_manual(values = formas, name = col_titulo) +
    scale_linetype_manual(
      values = rep(c("solid", "dashed", "dotted", "dotdash",
                     "longdash", "twodash"), length.out = ng),
      name = col_titulo
    ) +
    labs(title    = str_wrap(paste0(L$fig3_pre, col_titulo), config$wrap_titulo),
         subtitle = subtitulo,
         x = L$fig3_x, y = L$fig3_y,
         caption  = L$fig3_caption) +
    tema_pub(rotacao_x = 45) +
    guides(
      color    = guide_legend(nrow = nleg, override.aes = list(size = 3)),
      shape    = guide_legend(nrow = nleg),
      linetype = guide_legend(nrow = nleg)
    )
}

# --- 5.4 Composicao temporal (area empilhada, paleta contrastante) ---
fig_composicao <- function(df, col_titulo, subtitulo, L, config) {
  if (nrow(df) == 0) return(NULL)
  
  df   <- colapsar_ts(df, config$max_legend_grupos, L$outros)
  ng   <- n_distinct(df$Group)
  nleg <- ceiling(ng / 4)
  
  # Paleta com alto contraste entre areas adjacentes
  cores <- if (ng <= 8) {
    brewer.pal(max(ng, 3), "Set2")[seq_len(ng)]
  } else if (ng <= 12) {
    brewer.pal(max(ng, 3), "Set3")[seq_len(ng)]
  } else {
    paleta_esc(ng, "Set3")
  }
  
  ordem <- df %>% count(Group, wt = n_articles, sort = TRUE) %>% pull(Group)
  # Intercalar para maximizar contraste visual entre areas adjacentes
  idx_pares   <- seq(2, length(ordem), by = 2)
  idx_impares <- seq(1, length(ordem), by = 2)
  ordem_intercalada <- ordem[c(idx_impares, rev(idx_pares))]
  df$Group <- factor(df$Group, levels = ordem_intercalada)
  
  ggplot(df, aes(Year, n_articles, fill = Group)) +
    geom_area(alpha = 0.85, position = "stack", color = "white", linewidth = 0.3) +
    scale_x_continuous(breaks = breaks_5y(df$Year)) +
    scale_fill_manual(values = cores, name = col_titulo) +
    labs(title    = str_wrap(paste0(L$fig4_pre, col_titulo), config$wrap_titulo),
         subtitle = subtitulo,
         x = L$fig4_x, y = L$fig4_y,
         caption  = L$fig4_caption) +
    tema_pub(rotacao_x = 45) +
    guides(fill = guide_legend(nrow = nleg))
}

# --- 5.5 Ranking de autores ---
fig_ranking <- function(df, subtitulo, L, config) {
  if (nrow(df) < 3) return(NULL)
  df <- head(df, config$max_autores_bar)
  
  ggplot(df, aes(reorder(Author, N_Articles), N_Articles)) +
    geom_col(fill = "#2171B5", alpha = 0.9, width = 0.7) +
    geom_text(aes(label = N_Articles), hjust = -0.3, size = 3, fontface = "bold") +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title    = str_wrap(L$fig5_title, config$wrap_titulo),
         subtitle = subtitulo,
         x = L$fig5_x, y = L$fig5_y,
         caption  = L$fig5_caption) +
    tema_pub() +
    theme(panel.grid.major.y = element_blank())
}

# --- 5.6 Timeline de autores (expansao percentual) ---
fig_timeline <- function(df, subtitulo, L, config) {
  if (nrow(df) < 5) return(NULL)
  df <- head(df, config$max_autores_tl) %>%
    mutate(Author = factor(Author, levels = rev(Author)))
  
  ggplot(df, aes(y = Author)) +
    geom_segment(aes(x = First_Pub, xend = Last_Pub, yend = Author),
                 linewidth = 4.5, color = "#6BAED6", alpha = 0.8,
                 lineend = "round") +
    geom_point(aes(x = First_Pub), color = "#238B45", size = 3.5) +
    geom_point(aes(x = Last_Pub),  color = "#CB181D", size = 3.5) +
    geom_text(aes(x = Last_Pub, label = paste0(" n=", N_Articles)),
              hjust = 0, size = 2.8, fontface = "italic", nudge_x = 0.5) +
    scale_x_continuous(
      name   = L$fig6_x,
      breaks = breaks_5y(c(df$First_Pub, df$Last_Pub)),
      expand = expansion(mult = c(0.05, 0.15))
    ) +
    labs(title    = str_wrap(L$fig6_title, config$wrap_titulo),
         subtitle = subtitulo,
         y = L$fig6_y,
         caption  = L$fig6_caption) +
    tema_pub() +
    theme(panel.grid.major.y = element_blank())
}

# ==============================================================================
# 6. FUNCAO DE TABELAS
# ==============================================================================

gerar_tabelas <- function(DB, DB_geo, col_foco, df_autores_top, dir_out) {
  tab_pais <- resumo_grupo(DB_geo, "pais_expandido") %>%
    rename(Country = Group) %>%
    left_join(
      DB_geo %>%
        filter(!is.na(pais_expandido) & !is.na(ano)) %>%
        group_by(Country = pais_expandido) %>%
        summarise(Period = paste0(min(ano), "-", max(ano)), .groups = "drop"),
      by = "Country"
    )
  tab_grupo <- resumo_grupo(DB, col_foco)
  tab_det <- DB %>%
    distinct(Authors = autoria, Year = ano, Country = pais_padronizado) %>%
    arrange(Country, Year, Authors)
  
  write.xlsx(
    list("Table_1_By_Country" = tab_pais,
         "Table_2_By_Group"   = tab_grupo,
         "Table_3_Authors"    = df_autores_top,
         "Table_S1_Articles"  = tab_det),
    file.path(dir_out, "Tables_Results.xlsx")
  )
  cat("    [OK] Tables_Results.xlsx\n")
  list(pais = tab_pais, grupo = tab_grupo)
}

# ==============================================================================
# 7. FUNCAO DE RELATORIO
# ==============================================================================

gerar_relatorio <- function(DB, DB_geo, tab_pais, tab_grupo, df_autores_top,
                            col_foco, subtitulo, config, pkgs, dir_out) {
  total_art <- n_distinct(DB$autoria)
  total_est <- nrow(DB)
  n_paises  <- n_distinct(DB_geo$pais_expandido, na.rm = TRUE)
  periodo   <- paste(safe_min(DB$ano), "to", safe_max(DB$ano))
  
  si  <- safe_index
  tp1 <- si(tab_pais$Country, 1);    tn1 <- si(tab_pais$N_Articles, 1, "0")
  tp2 <- si(tab_pais$Country, 2);    tn2 <- si(tab_pais$N_Articles, 2, "0")
  tp3 <- si(tab_pais$Country, 3);    tn3 <- si(tab_pais$N_Articles, 3, "0")
  pct <- if (total_art > 0) round(as.numeric(tn1) / total_art * 100, 1) else 0
  
  tg1  <- si(tab_grupo$Group, 1);      tgn <- si(tab_grupo$N_Articles, 1, "0")
  tgh2 <- si(tab_grupo$Mean_h2, 1);    tgsd <- si(tab_grupo$SD_h2, 1)
  tgmn <- si(tab_grupo$Min_h2, 1);     tgmx <- si(tab_grupo$Max_h2, 1)
  
  ta1  <- si(df_autores_top$Author, 1)
  tan1 <- si(df_autores_top$N_Articles, 1, "0")
  taf1 <- si(df_autores_top$First_Pub, 1)
  tal1 <- si(df_autores_top$Last_Pub, 1)
  ta2  <- si(df_autores_top$Author, 2); tan2 <- si(df_autores_top$N_Articles, 2, "0")
  ta3  <- si(df_autores_top$Author, 3); tan3 <- si(df_autores_top$N_Articles, 3, "0")
  
  cf <- gsub("_", " ", col_foco)
  
  txt <- paste0(
    "================================================================================\n",
    "                    SYSTEMATIC REVIEW - BIBLIOMETRIC ANALYSIS\n",
    "================================================================================\n\n",
    subtitulo, "\n",
    "Generated: ", format(Sys.time(), "%B %d, %Y at %H:%M"), "\n\n",
    "--------------------------------------------------------------------------------\n",
    "ABSTRACT\n",
    "--------------------------------------------------------------------------------\n",
    "This systematic review analyzed ", total_art, " scientific articles published\n",
    "between ", periodo, ", comprising ", total_est, " parameter estimates from\n",
    n_paises, " countries. Focus: ", config$parametro_av, " traits in\n",
    config$especie_estudo, " (", config$tipo_producao, ").\n\n",
    "NOTE: Multi-country studies were expanded so each contributing country\n",
    "receives credit. Country totals are therefore not additive.\n\n",
    "--------------------------------------------------------------------------------\n",
    "KEY FINDINGS\n",
    "--------------------------------------------------------------------------------\n\n",
    "GEOGRAPHIC DISTRIBUTION\n",
    "  ", tp1, " led with ", tn1, " articles (", pct, "%), followed by\n",
    "  ", tp2, " (", tn2, ") and ", tp3, " (", tn3, ").\n\n",
    "ANALYSIS BY ", toupper(cf), "\n",
    "  '", tg1, "' had the most publications (n = ", tgn, "),\n",
    "  mean h2 = ", tgh2, " +/- ", tgsd, " (range: ", tgmn, "-", tgmx, ").\n\n",
    "MOST PRODUCTIVE AUTHORS\n",
    "  ", ta1, ": ", tan1, " publications (", taf1, "-", tal1, ").\n",
    "  ", ta2, ": ", tan2, " | ", ta3, ": ", tan3, ".\n\n",
    "--------------------------------------------------------------------------------\n",
    "OUTPUTS\n",
    "--------------------------------------------------------------------------------\n",
    "FIGURES:\n",
    "  1. Global distribution map\n",
    "  2. Temporal evolution (dual panel)\n",
    "  3. Temporal trends by ", cf, " (color + shape + linetype)\n",
    "  4. Composition by ", cf, "\n",
    "  5. Author productivity (Top ", config$max_autores_bar, ")\n",
    "  6. Author timeline (Top ", config$max_autores_tl, ")\n\n",
    "TABLES (Tables_Results.xlsx):\n",
    "  1. By country | 2. By ", cf, " | 3. Authors | S1. Articles\n\n",
    "--------------------------------------------------------------------------------\n",
    "METHODOLOGY\n",
    "--------------------------------------------------------------------------------\n",
    "R v", R.version$major, ".", R.version$minor, " | ",
    config$dpi, " DPI | TIFF LZW | ", config$largura_dupla, "mm width.\n",
    "Packages: ", paste(pkgs, collapse = ", "), ".\n",
    "================================================================================\n"
  )
  
  escrever_utf8(txt, file.path(dir_out, "Scientific_Report.txt"))
  cat("    [OK] Scientific_Report.txt\n")
}

# ==============================================================================
# 8. EXECUCAO PRINCIPAL
# ==============================================================================

# --- 8.1 Entrada do usuario ---
cat("\n=============================================================")
cat("\n      ", L$console_titulo)
cat("\n=============================================================\n\n")

if (CONFIG$interativo) {
  CONFIG$objetivo_estudo <- readline(L$console_objetivo)
  CONFIG$parametro_av    <- readline(L$console_parametro)
  CONFIG$especie_estudo  <- readline(L$console_especie)
  CONFIG$tipo_producao   <- readline(L$console_producao)
}

subtitulo_padrao <- paste0(
  L$sub_prefixo, CONFIG$objetivo_estudo, " ",
  CONFIG$parametro_av, L$sub_em,
  CONFIG$especie_estudo, " ", CONFIG$tipo_producao
)
subtitulo_wrap <- str_wrap(subtitulo_padrao, width = CONFIG$wrap_subtitulo)
cat("[INFO]", subtitulo_padrao, "\n")

# --- 8.2 Importacao ---
cat("\n[1/7]", L$step_importacao, "\n")

caminho_base <- if (CONFIG$caminho_base != "" && dir.exists(CONFIG$caminho_base)) {
  CONFIG$caminho_base
} else {
  cat("[AVISO]", L$msg_cam_nf, "\n"); getwd()
}
cat(L$msg_pasta, caminho_base, "\n")

arquivos <- list.files(caminho_base, pattern = "\\.(xlsx|xls|csv)$",
                       full.names = FALSE, recursive = TRUE)
arquivos <- arquivos[!grepl("^~\\$", arquivos)]
if (length(arquivos) == 0) stop("[ERRO] ", L$msg_sem_arq, caminho_base)

cat("\n", L$msg_arq_disp, "\n")
for (i in seq_along(arquivos)) cat("  [", i, "]", arquivos[i], "\n")

input_arq <- if (CONFIG$interativo || CONFIG$arquivo_dados == "") {
  readline(paste0("\n", L$console_arquivo))
} else {
  CONFIG$arquivo_dados
}

candidatos <- list.files(caminho_base, full.names = TRUE, recursive = TRUE)
full_path  <- candidatos[grepl(input_arq, basename(candidatos), fixed = TRUE)][1]
if (is.na(full_path) || !file.exists(full_path)) {
  stop("[ERRO] ", L$msg_arq_nf, input_arq, "\n       ", L$msg_verifique)
}
cat("[OK]", L$msg_arq_enc, basename(full_path), "\n")

cols_ok <- c("autoria", "ano", "periodo", "pais", "hemisferio", "grupo",
             "sistema", "clima", "descricao", "doi", "raca_agrupada",
             "rebanho", "idade", "n", "metodo", "software", "trait",
             "conceito", "type", "mean", "sd", "h2", "type_h2", "se",
             "hpd_lower", "hpd_upper", "cluster_category")

DB_RAW <- if (grepl("\\.xlsx?$", full_path, ignore.case = TRUE)) {
  read_excel(full_path)
} else {
  read.csv(full_path, fileEncoding = "UTF-8", check.names = FALSE)
}

DB <- DB_RAW %>% select(all_of(intersect(cols_ok, names(DB_RAW))))
DB$ano <- suppressWarnings(as.numeric(as.character(DB$ano)))
DB$h2  <- suppressWarnings(as.numeric(as.character(DB$h2)))
cat("[OK]", nrow(DB), L$msg_reg, ncol(DB), L$msg_var, "\n")

# --- 8.3 Padronizacao geografica ---
cat("\n[2/7]", L$step_padronizacao, "\n")

# Cache de regioes do mapa (evita recalcular dentro de sapply)
.regioes_mapa_cache <- unique(map_data("world")$region)

DB$pais_padronizado <- sapply(DB$pais, function(p) {
  if (is.na(p) || p == "") return(NA_character_)
  padronizar_pais(str_trim(str_split(p, "[,;&/]|\\band\\b")[[1]][1]))
})

DB_geo <- expandir_paises(DB, "pais")

cat("[OK]", L$msg_paises_pri, n_distinct(DB$pais_padronizado, na.rm = TRUE), "\n")
cat("[OK]", L$msg_paises_exp, n_distinct(DB_geo$pais_expandido, na.rm = TRUE), "\n")

sem_match <- setdiff(na.omit(unique(DB_geo$pais_expandido)), .regioes_mapa_cache)
if (length(sem_match) > 0) {
  cat("[AVISO]", L$msg_sem_corresp, paste(sem_match, collapse = ", "), "\n")
  cat("       ", L$msg_verif_pais, "\n")
}

# --- 8.4 Diretorio de saida ---
dir_out <- file.path(getwd(), paste0(
  format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
  nome_seguro(CONFIG$especie_estudo), "_",
  nome_seguro(CONFIG$tipo_producao), "_",
  nome_seguro(CONFIG$parametro_av)
))
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
cat("[OK]", L$msg_saida, dir_out, "\n")

# --- 8.5 Variavel de agrupamento ---
cat("\n[3/7]", L$step_variavel, "\n")

vars_disp <- intersect(
  c("raca_agrupada", "hemisferio", "grupo", "sistema", "clima", "cluster_category"),
  names(DB)
)
if (length(vars_disp) == 0) stop("[ERRO] ", L$msg_sem_var)
for (v in vars_disp) {
  cat("  *", v, "(", n_distinct(DB[[v]], na.rm = TRUE), L$msg_cat, ")\n")
}

col_foco <- if (CONFIG$interativo) readline(L$console_variavel) else CONFIG$col_foco
if (!col_foco %in% names(DB)) {
  cat("[AVISO] '", col_foco, "'", L$msg_nao_enc, "'", vars_disp[1], "'.\n")
  col_foco <- vars_disp[1]
}
DB[[col_foco]] <- as.character(DB[[col_foco]])
col_foco_titulo <- tools::toTitleCase(gsub("_", " ", col_foco))
cat("[OK]", L$msg_agrup, col_foco,
    "(", n_distinct(DB[[col_foco]], na.rm = TRUE), L$msg_cat, ")\n")

# --- 8.6 Preparacao de dados ---
cat("\n[4/7]", L$step_figuras, "\n")

df_mapa <- DB_geo %>%
  filter(!is.na(pais_expandido)) %>%
  group_by(pais_expandido) %>%
  summarise(n_articles = n_distinct(autoria), .groups = "drop")

df_tempo <- DB %>%
  filter(!is.na(ano)) %>%
  group_by(ano) %>%
  summarise(n_articles = n_distinct(autoria), .groups = "drop") %>%
  arrange(ano) %>%
  mutate(acumulado = cumsum(n_articles))

df_tempo_grupo <- DB %>%
  filter(!is.na(ano) & !is.na(.data[[col_foco]])) %>%
  group_by(Year = ano, Group = .data[[col_foco]]) %>%
  summarise(n_articles = n_distinct(autoria), .groups = "drop")

DB$first_author <- sapply(DB$autoria, extrair_primeiro_autor)

df_autores <- DB %>%
  filter(!is.na(first_author) & nchar(first_author) > 1) %>%
  group_by(Author = first_author) %>%
  summarise(
    N_Articles   = n_distinct(autoria),
    N_Estimates  = n(),
    First_Pub    = safe_min(ano),
    Last_Pub     = safe_max(ano),
    Active_Years = safe_max(ano) - safe_min(ano),
    Countries    = paste(unique(na.omit(pais_padronizado)), collapse = "; "),
    .groups      = "drop"
  ) %>%
  filter(!is.na(First_Pub)) %>%
  arrange(desc(N_Articles), desc(Active_Years))

df_autores_top <- head(df_autores, 30)

# --- 8.7 Gerar e salvar figuras ---
cat("    Figura 1:", L$fig1_log, "\n")
p <- fig_mapa(df_mapa, subtitulo_wrap, L, CONFIG)
if (!is.null(p)) salvar_fig(p, "Figure_1_Global_Map.tiff", dir_out, height = 100)

cat("    Figura 2:", L$fig2_log, "\n")
p <- fig_temporal(df_tempo, subtitulo_wrap, L, CONFIG)
if (!is.null(p)) salvar_fig(p, "Figure_2_Temporal_Growth.tiff", dir_out, height = 150)

cat("    Figura 3:", L$fig3_log, "\n")
p <- fig_tendencias(df_tempo_grupo, col_foco_titulo, subtitulo_wrap, L, CONFIG)
if (!is.null(p)) salvar_fig(p, "Figure_3_Temporal_Trends.tiff", dir_out, height = 130)

cat("    Figura 4:", L$fig4_log, "\n")
p <- fig_composicao(df_tempo_grupo, col_foco_titulo, subtitulo_wrap, L, CONFIG)
if (!is.null(p)) salvar_fig(p, "Figure_4_Composition.tiff", dir_out, height = 130)

cat("\n[5/7]", L$step_autoria, "\n")
cat("    Top 5 autores:\n")
print(head(df_autores_top %>% select(Author, N_Articles, First_Pub, Last_Pub), 5))

cat("    Figura 5:", L$fig5_log, "\n")
p <- fig_ranking(df_autores_top, subtitulo_wrap, L, CONFIG)
if (!is.null(p)) salvar_fig(p, "Figure_5_Author_Productivity.tiff", dir_out, height = 160)

cat("    Figura 6:", L$fig6_log, "\n")
p <- fig_timeline(df_autores_top, subtitulo_wrap, L, CONFIG)
if (!is.null(p)) salvar_fig(p, "Figure_6_Author_Timeline.tiff", dir_out, height = 150)

# --- 8.8 Tabelas ---
cat("\n[6/7]", L$step_tabelas, "\n")
tabs <- gerar_tabelas(DB, DB_geo, col_foco, df_autores_top, dir_out)

# --- 8.9 Relatorio ---
cat("\n[7/7]", L$step_relatorio, "\n")
gerar_relatorio(DB, DB_geo, tabs$pais, tabs$grupo, df_autores_top,
                col_foco, subtitulo_padrao, CONFIG, pacotes, dir_out)

# --- 8.10 Finalizacao ---
gerados <- list.files(dir_out)
cat("\n================================================================================")
cat("\n   ", L$step_concluido, "-", length(gerados), "arquivos em:")
cat("\n   ", dir_out)
cat("\n================================================================================\n")
for (f in gerados) cat("  *", f, "\n")
cat("================================================================================\n")