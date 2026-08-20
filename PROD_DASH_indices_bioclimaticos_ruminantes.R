library(shiny)
#Ruminantes

# Interface do Usuário (UI)
ui <- fluidPage(
  titlePanel("Calculadora de Índices Bioclimáticos para Ruminantes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("especie", "Espécie Animal:", choices = c("Bovinos", "Caprinos", "Ovinos")),
      numericInput("TBS", "Temperatura do Bulbo Seco (°C):", 26, min = 0, max = 50),
      numericInput("UR", "Umidade Relativa (%):", 65, min = 0, max = 100),
      numericInput("VV", "Velocidade do Vento (m/s):", 2.0, min = 0, max = 10),
      numericInput("TGN", "Temperatura do Globo Negro (°C):", 32, min = 0, max = 50),
      actionButton("calcular", "Calcular Índices"),
      helpText("Insira os valores climáticos e selecione a espécie para avaliar.")
    ),
    mainPanel(
      h3("Resultados dos Índices Bioclimáticos"),
      htmlOutput("To"),  # Ponto de Orvalho
      htmlOutput("ITU"), # Índice de Temperatura e Umidade
      htmlOutput("ITG"), # Índice de Temperatura do Globo Negro
      htmlOutput("CTR"), # Carga Térmica Radiante
      htmlOutput("ICT")  # Índice de Conforto Térmico (apenas ovinos)
    )
  ),
  tags$style(HTML("
    .highlight { font-size: 1.5em; color: #007bff; font-weight: bold; }
  "))
)

# Lógica do Servidor
server <- function(input, output, session) {
  observeEvent(input$calcular, {
    # Função para calcular o Ponto de Orvalho (To)
    calcular_ponto_orvalho <- function(TBS, UR) {
      es <- 0.611 * exp((7.5 * TBS) / (237.3 + TBS))  # Pressão de saturação (kPa)
      ea <- (UR / 100) * es  # Pressão parcial
      To <- (237.3 * log(ea / 0.611)) / (7.5 - log(ea / 0.611))
      return(To)
    }
    
    # Função para calcular a TRM (Temperatura Radiante Média)
    calcular_TRM <- function(TGN, TBS, VV) {
      TGN_K <- TGN + 273.15  # Converter para Kelvin
      TBS_K <- TBS + 273.15
      TRM <- 100 * ((2.51 * sqrt(VV) * (TGN_K - TBS_K)) + (TGN_K / 100)^4)^0.25
      return(TRM)
    }
    
    # Cálculos dos índices
    To <- calcular_ponto_orvalho(input$TBS, input$UR)
    ITU <- 0.8 * input$TBS + (input$UR * (input$TBS - 14.3) / 100) + 46.3  # Fórmula de Buffington
    TGN_K <- input$TGN + 273.15  # Temperatura do Globo Negro em Kelvin
    ITG <- TGN_K + 0.36 * (To + 273.15) - 330.08  # Fórmula precisa do ITG
    TRM <- calcular_TRM(input$TGN, input$TBS, input$VV)
    CTR <- 5.67e-8 * (TRM)^4  # Carga Térmica Radiante (W/m²)
    
    # ICT apenas para ovinos
    ICT <- if (input$especie == "Ovinos") {
      0.659 * input$TBS + 0.511 * (input$UR / 100 * 101.3) + 0.550 * input$TGN - 0.042 * input$VV
    } else {
      NA
    }
    
    # Funções de Avaliação
    avaliar_ITU <- function(especie, itu) {
      if (especie == "Bovinos") {
        if (itu < 70) "Conforto térmico"
        else if (itu < 72) "Crítico (limiar de desconforto)"
        else if (itu < 78) "Alerta (redução na produção)"
        else if (itu < 82) "Perigo"
        else "Emergência"
      } else if (especie %in% c("Caprinos", "Ovinos")) {
        if (itu < 70) "Conforto térmico"
        else if (itu < 79) "Estresse moderado"
        else "Estresse severo"
      }
    }
    
    avaliar_ITG <- function(especie, itg) {
      if (especie == "Bovinos") {
        if (itg < 74) "Conforto térmico"
        else if (itg < 78) "Cautela"
        else if (itg < 84) "Perigoso"
        else "Emergência"
      } else if (especie == "Caprinos") {
        if (itg < 74) "Conforto térmico"
        else if (itg < 82) "Cautela"
        else if (itg < 90) "Perigoso"
        else "Emergência"
      } else if (especie == "Ovinos") {
        if (itg < 74) "Conforto térmico"
        else if (itg < 78) "Cautela"
        else if (itg < 84) "Perigoso"
        else "Emergência"
      }
    }
    
    avaliar_ICT <- function(especie, ict) {
      if (especie == "Ovinos" && !is.na(ict)) {
        if (ict >= 20 && ict <= 37) "Normal"
        else "Fora da zona de conforto"
      } else {
        "Não aplicável"
      }
    }
    
    # Exibição dos Resultados
    output$To <- renderText(paste("Ponto de Orvalho (To):", round(To, 2), "°C"))
    output$ITU <- renderText(paste("ITU:", round(ITU, 2), " - Avaliação para", input$especie, ":", avaliar_ITU(input$especie, ITU)))
    
    # Destaque visual do ITG para bovinos
    if (input$especie == "Bovinos") {
      output$ITG <- renderText(paste("<span class='highlight'>ITG (Índice de Temperatura do Globo Negro):", round(ITG, 2), " - Avaliação para Bovinos:", avaliar_ITG(input$especie, ITG), "</span>"))
    } else {
      output$ITG <- renderText(paste("ITG:", round(ITG, 2), " - Avaliação para", input$especie, ":", avaliar_ITG(input$especie, ITG)))
    }
    
    output$CTR <- renderText(paste("CTR (Carga Térmica Radiante):", round(CTR, 2), "W/m²"))
    output$ICT <- renderText(paste("ICT (Índice de Conforto Térmico):", if (!is.na(ICT)) round(ICT, 2) else "Não aplicável", " - Avaliação para", input$especie, ":", avaliar_ICT(input$especie, ICT)))
  })
}

# Executar o aplicativo
shinyApp(ui, server)

#Bovinos
library(shiny)

# Interface do Usuário (UI)
ui <- fluidPage(
  titlePanel("Calculadora de Índices Bioclimáticos para Bovinos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tipo_bovino", "Tipo de Bovino:", choices = c("Corte", "Leiteiro")),
      numericInput("TBS", "Temperatura do Bulbo Seco (°C):", 26, min = 0, max = 50),
      numericInput("UR", "Umidade Relativa (%):", 65, min = 0, max = 100),
      numericInput("VV", "Velocidade do Vento (m/s):", 2.0, min = 0, max = 10),
      numericInput("TGN", "Temperatura do Globo Negro (°C):", 32, min = 0, max = 50),
      actionButton("calcular", "Calcular Índices"),
      helpText("Insira os valores climáticos e selecione o tipo de bovino para avaliar.")
    ),
    mainPanel(
      h3("Resultados dos Índices Bioclimáticos"),
      textOutput("applicability_ITU"),
      textOutput("ITU"),
      textOutput("applicability_ITGU"),
      textOutput("ITGU"),
      textOutput("applicability_CTR"),
      textOutput("CTR"),
      textOutput("applicability_ICT"),
      textOutput("ICT"),
      textOutput("perigo"),
      textOutput("estrategias")
    )
  )
)

# Lógica do Servidor
server <- function(input, output, session) {
  observeEvent(input$calcular, {
    # Função para calcular o Ponto de Orvalho (To)
    calcular_ponto_orvalho <- function(TBS, UR) {
      es <- 0.611 * exp((7.5 * TBS) / (237.3 + TBS))  # Pressão de saturação (kPa)
      ea <- (UR / 100) * es  # Pressão parcial
      To <- (237.3 * log(ea / 0.611)) / (7.5 - log(ea / 0.611))
      return(To)
    }
    
    # Função para calcular a TRM (Temperatura Radiante Média)
    calcular_TRM <- function(TGN, TBS, VV) {
      TGN_K <- TGN + 273.15  # Converter para Kelvin
      TBS_K <- TBS + 273.15
      TRM <- 100 * ((2.51 * sqrt(VV) * (TGN_K - TBS_K)) + (TGN_K / 100)^4)^0.25
      return(TRM)
    }
    
    # Cálculo dos índices
    To <- calcular_ponto_orvalho(input$TBS, input$UR)
    ITU <- 0.8 * input$TBS + (input$UR * (input$TBS - 14.3) / 100) + 46.3  # Fórmula de Buffington
    TGN_K <- input$TGN + 273.15  # Temperatura do Globo Negro em Kelvin
    ITGU <- TGN_K + 0.36 * (To + 273.15) - 330.08  # Fórmula do ITGU
    TRM <- calcular_TRM(input$TGN, input$TBS, input$VV)
    CTR <- 5.67e-8 * (TRM)^4  # Carga Térmica Radiante (W/m²)
    ICT <- NA  # Não aplicável para bovinos
    
    # Avaliação do ITU
    avaliar_ITU <- function(tipo_bovino, itu) {
      if (tipo_bovino == "Corte") {
        if (itu < 70) "Conforto"
        else if (itu < 79) "Alerta"
        else if (itu < 88) "Perigo"
        else "Emergência"
      } else {  # Leiteiro
        if (itu < 70) "Conforto"
        else if (itu < 72) "Crítico"
        else if (itu < 78) "Alerta"
        else if (itu < 82) "Perigo"
        else "Emergência"
      }
    }
    
    # Avaliação do ITGU
    avaliar_ITGU <- function(tipo_bovino, itgu) {
      if (tipo_bovino == "Corte") {
        if (itgu < 79) "Conforto"
        else if (itgu < 84) "Perigo"
        else "Emergência"
      } else {  # Leiteiro
        if (itgu < 74) "Conforto"
        else if (itgu < 78) "Cautela"
        else if (itgu < 84) "Perigoso"
        else "Emergência"
      }
    }
    
    # Avaliação do CTR (apenas para corte)
    avaliar_CTR <- function(tipo_bovino, ctr) {
      if (tipo_bovino == "Corte") {
        if (ctr < 571) "Conforto"
        else "Perigo"
      } else {
        "Não Aplicável"
      }
    }
    
    # Avaliação do ICT (não aplicável)
    avaliar_ICT <- function(tipo_bovino, ict) {
      "Não Aplicável"
    }
    
    # Identificação de condições de perigo
    avaliar_perigo <- function(tipo_bovino, itu, itgu, ctr, tbs, ur) {
      if (tipo_bovino == "Corte") {
        if (itu > 79 || itgu > 84 || ctr > 571 || (tbs > 30 & ur > 60)) {
          "Perigo: Condições térmicas extremas detectadas."
        } else {
          "Condições térmicas dentro do conforto."
        }
      } else {  # Leiteiro
        if (itu > 78 || (tbs > 27 & ur > 70)) {
          "Perigo: Condições térmicas extremas detectadas."
        } else {
          "Condições térmicas dentro do conforto."
        }
      }
    }
    
    # Estratégias de mitigação
    estrategias_mitigacao <- function(tipo_bovino, perigo) {
      if (perigo == "Perigo: Condições térmicas extremas detectadas.") {
        if (tipo_bovino == "Corte") {
          "Recomenda-se o uso de sombreamento natural, como pequenos bosques de árvores de Guajuvira, para reduzir o estresse térmico."
        } else {  # Leiteiro
          "Recomenda-se ventilação, aspersão de água e sombreamento para mitigar o estresse térmico."
        }
      } else {
        "Não encontrada recomendação."
      }
    }
    
    # Exibição dos resultados
    output$applicability_ITU <- renderText("Aplicável para: Bovinos de Corte e Bovinos Leiteiros")
    output$ITU <- renderText(paste("ITU:", round(ITU, 2), " - Avaliação para", input$tipo_bovino, ":", avaliar_ITU(input$tipo_bovino, ITU)))
    
    output$applicability_ITGU <- renderText(ifelse(input$tipo_bovino == "Corte", 
                                                   "Aplicável para: Bovinos de Corte", 
                                                   "Aplicável para: Bovinos Leiteiros (com ajustes genéricos)"))
    output$ITGU <- renderText(paste("ITGU:", round(ITGU, 2), " - Avaliação para", input$tipo_bovino, ":", avaliar_ITGU(input$tipo_bovino, ITGU)))
    
    output$applicability_CTR <- renderText(ifelse(input$tipo_bovino == "Corte", 
                                                  "Aplicável para: Bovinos de Corte", 
                                                  "Não Aplicável para: Bovinos Leiteiros"))
    output$CTR <- renderText(ifelse(input$tipo_bovino == "Corte", 
                                    paste("CTR:", round(CTR, 2), "W/m² - Avaliação para Corte:", avaliar_CTR(input$tipo_bovino, CTR)), 
                                    "Não Aplicável"))
    
    output$applicability_ICT <- renderText("Não Aplicável para: Bovinos de Corte e Bovinos Leiteiros")
    output$ICT <- renderText("Não Aplicável")
    
    output$perigo <- renderText({
      avaliar_perigo(input$tipo_bovino, ITU, ITGU, CTR, input$TBS, input$UR)
    })
    
    output$estrategias <- renderText({
      perigo <- avaliar_perigo(input$tipo_bovino, ITU, ITGU, CTR, input$TBS, input$UR)
      estrategias_mitigacao(input$tipo_bovino, peligro)
    })
  })
}

# Executar o aplicativo
shinyApp(ui, server)
