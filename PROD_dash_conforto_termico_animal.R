# Lista de pacotes não nativos
pacotes <- c("readr", "dplyr", "lubridate", "rvest", "ggplot2", "lme4", "shiny",
             "rmarkdown", "caret", "randomForest", "xgboost", "neuralnet", "sf",
             "raster", "leaflet", "shinydashboard")

# Loop para instalar cada pacote
for (pacote in pacotes) {
  if (!requireNamespace(pacote, quietly = TRUE)) {
    install.packages(pacote)
  }
}

library(pacote)


library(shiny)

# Interface do Usuário
ui <- fluidPage(
  titlePanel("Calculadora de Conforto Térmico para Animais"),
  sidebarLayout(
    sidebarPanel(
      selectInput("especie", "Espécie Animal:", choices = c("Aves", "Suínos")),
      selectInput("categoria", "Categoria/Idade:", choices = NULL),
      numericInput("TBS", "Temperatura do Bulbo Seco (°C):", 26, min = -10, max = 50),
      numericInput("UR", "Umidade Relativa (%):", 65, min = 0, max = 100),
      numericInput("TGN", "Temperatura de Globo Negro (°C):", 28, min = -10, max = 50),
      numericInput("VV", "Velocidade do Vento (m/s):", 0.5, min = 0, max = 10),
      actionButton("calcular", "Calcular Índices", class = "btn-primary"),
      helpText("Insira os valores climáticos e selecione a espécie/categoria.")
    ),
    mainPanel(
      h3("Resultados do Conforto Térmico"),
      h4("Condições Climáticas"),
      textOutput("Temperatura_Umidade"),
      textOutput("Velocidade_Vento"),
      h4("Índices de Conforto Térmico"),
      strong(textOutput("ITGU_resultado")),  # Destacando ITGU
      textOutput("ITGU_avaliacao"),
      textOutput("CTR_resultado"),
      strong(textOutput("IAPfc_resultado")),  # Destacando IPAfc
      textOutput("IAPfc_avaliacao"),
      h4("Efeitos e Recomendações"),
      textOutput("Efeitos_Estrategias")
    )
  )
)

# Lógica do Servidor
server <- function(input, output, session) {
  # Atualizar categorias dinamicamente
  observe({
    if (input$especie == "Aves") {
      updateSelectInput(session, "categoria", choices = c("0 dias", "7 dias", "14 dias", "21 dias", "28 dias", "35 dias", "42 dias", "49 dias", "56 dias"))
    } else if (input$especie == "Suínos") {
      updateSelectInput(session, "categoria", choices = c("Recém-nascidos", "Desmamados", "Crescimento", "Terminação", "Gestantes", "Lactação"))
    }
  })
  
  observeEvent(input$calcular, {
    # Avaliação de Temperatura e Umidade (com base na imagem para aves)
    avaliar_temperatura_umidade <- function(especie, categoria, tbs, ur) {
      if (especie == "Aves") {
        faixas <- data.frame(
          idade = c("0 dias", "7 dias", "14 dias", "21 dias", "28 dias", "35 dias", "42 dias", "49 dias", "56 dias"),
          ur_min = c(30, 40, 50, 50, 50, 50, 50, 50, 50),
          ur_max = c(50, 60, 60, 60, 65, 70, 70, 70, 70),
          temp_min = c(32, 29, 27, 24, 21, 19, 18, 17, 16),
          temp_max = c(33, 30, 28, 26, 23, 21, 18, 17, 16)
        )
        faixa <- faixas[faixas$idade == categoria, ]
        temp_min <- faixa$temp_min
        temp_max <- faixa$temp_max
        ur_min <- faixa$ur_min
        ur_max <- faixa$ur_max
        
        if (tbs < temp_min || tbs > temp_max) {
          temp_status <- "Desconforto"
        } else {
          temp_status <- "Conforto"
        }
        
        if (ur < ur_min || ur > ur_max) {
          ur_status <- "Desconforto"
        } else {
          ur_status <- "Conforto"
        }
        
        return(list(status = paste("Temperatura:", temp_status, "- Umidade Relativa:", ur_status),
                    temp_min = temp_min, temp_max = temp_max, ur_min = ur_min, ur_max = ur_max))
      } else if (especie == "Suínos") {
        if (categoria == "Recém-nascidos") { temp_min <- 32; temp_max <- 34; ur_min <- 40; ur_max <- 70 }
        else if (categoria == "Desmamados") { temp_min <- 22; temp_max <- 26; ur_min <- 40; ur_max <- 70 }
        else if (categoria == "Crescimento") { temp_min <- 18; temp_max <- 20; ur_min <- 40; ur_max <- 70 }
        else if (categoria == "Terminação") { temp_min <- 12; temp_max <- 21; ur_min <- 40; ur_max <- 70 }
        else if (categoria == "Gestantes") { temp_min <- 16; temp_max <- 19; ur_min <- 40; ur_max <- 70 }
        else if (categoria == "Lactação") { temp_min <- 12; temp_max <- 16; ur_min <- 40; ur_max <- 70 }
        
        if (especie == "Suínos" && (tbs < 0 || tbs > 35)) {
          temp_status <- "Morte"
        } else if (tbs >= temp_min && tbs <= temp_max) {
          temp_status <- "Conforto"
        } else {
          temp_status <- "Desconforto"
        }
        
        if (ur >= ur_min && ur <= ur_max) {
          ur_status <- "Conforto"
        } else {
          ur_status <- "Desconforto"
        }
        
        return(list(status = paste("Temperatura:", temp_status, "- Umidade Relativa:", ur_status),
                    temp_min = temp_min, temp_max = temp_max, ur_min = ur_min, ur_max = ur_max))
      }
    }
    
    # Avaliação de Velocidade do Vento
    avaliar_velocidade_vento <- function(especie, categoria, vv) {
      if (especie == "Aves") {
        idade <- as.numeric(gsub(" dias", "", categoria))
        if (idade >= 0 && idade <= 14) { vv_max <- 0 }
        else if (idade >= 15 && idade <= 21) { vv_max <- 0.5 }
        else if (idade >= 22 && idade <= 28) { vv_max <- 0.875 }
        else { vv_max <- 2.5 }
        
        if (vv > vv_max) {
          return(list(status = paste("Velocidade do Vento: Desconforto (máximo recomendado:", vv_max, "m/s)"),
                      vv_max = vv_max))
        } else {
          return(list(status = paste("Velocidade do Vento: Conforto (máximo recomendado:", vv_max, "m/s)"),
                      vv_max = vv_max))
        }
      } else {
        return(list(status = "Velocidade do Vento: Não avaliada para suínos",
                    vv_max = NULL))
      }
    }
    
    # Funções de cálculo dos índices
    calcular_ITGU <- function(tgn, tpo) {
      return(tgn + 0.36 * tpo + 41.5)
    }
    
    calcular_CTR <- function(tgn, tbs, vv) {
      sigma <- 5.67 * 10^-8
      tmr <- ((tgn + 273) / 100)^4 + 2.5 * 10^8 * vv^0.5 * (tgn - tbs)
      return(sigma * tmr^4)
    }
    
    calcular_IAPfc <- function(tbs, ur, vv) {
      return(45.6026 - 2.31072 * tbs - 0.368331 * ur + 9.70922 * vv + 0.0549243 * tbs^2 + 0.0012183 * ur^2 + 0.66329 * vv^2 + 0.0128968)
    }
    
    calcular_ponto_orvalho <- function(TBS, UR) {
      es <- 0.611 * exp((7.5 * TBS) / (237.3 + TBS))
      ea <- (UR / 100) * es
      To <- (237.3 * log(ea / 0.611)) / (7.5 - log(ea / 0.611))
      return(To)
    }
    
    # Funções de avaliação dos índices
    avaliar_ITGU <- function(itgu) {
      if (itgu > 75) {
        return("Desconforto térmico. Recomenda-se ventilação forçada e resfriamento evaporativo.")
      } else {
        return("Conforto térmico adequado.")
      }
    }
    
    avaliar_IAPfc <- function(iapfc) {
      if (iapfc >= 21 && iapfc <= 24) {
        return("Conforto térmico adequado.")
      } else if (iapfc >= 19 && iapfc <= 20 || iapfc >= 25 && iapfc <= 27) {
        return("Moderadamente confortável. Recomenda-se ajustes finos na ventilação e temperatura.")
      } else if (iapfc < 19 || iapfc >= 28 && iapfc <= 30) {
        return("Desconfortável. Recomenda-se ventilação forçada e resfriamento evaporativo.")
      } else if (iapfc >= 31 && iapfc <= 34) {
        return("Extremamente desconfortável. Recomenda-se medidas urgentes de resfriamento.")
      } else if (iapfc >= 35) {
        return("Perigoso. Recomenda-se medidas extremas de resfriamento e avaliação veterinária.")
      } else {
        return("Valor de IAPfc inválido.")
      }
    }
    
    # Efeitos e Estratégias combinados
    efeitos_estrategias <- function(especie, categoria, status_clima, vv_status, itgu_avaliacao, iapfc_avaliacao, temp_min, temp_max, ur_min, ur_max) {
      efeitos <- ""
      estrategias <- ""
      
      if (especie == "Aves") {
        if (grepl("Desconforto", status_clima) || grepl("Desconforto", vv_status)) {
          efeitos <- "Condições fora da faixa ideal podem afetar o desempenho e a saúde das aves."
          if (status_clima == "Temperatura: Desconforto - Umidade Relativa: Conforto" || status_clima == "Temperatura: Desconforto - Umidade Relativa: Desconforto") {
            if (input$UR < ur_min) {
              estrategias <- "Aumentar a faixa de temperatura de 0,5°C a 1°C devido à umidade abaixo da faixa ideal."
            } else if (input$UR > ur_max) {
              estrategias <- "Reduzir a faixa de temperatura de 0,5°C a 1°C devido à umidade acima da faixa ideal."
            }
          }
          if (grepl("Desconforto", vv_status)) {
            estrategias <- paste(estrategias, "Ajustar ventilação para manter velocidade do vento ???", vv_status$vv_max, "m/s.")
          }
          estrategias <- paste(estrategias, itgu_avaliacao, iapfc_avaliacao)
        } else {
          efeitos <- "Condições de conforto. Monitorar a temperatura efetiva e a atividade das aves."
          estrategias <- "Manter ventilação e umidade dentro das faixas ideais."
        }
      } else if (especie == "Suínos") {
        if (grepl("Morte", status_clima)) {
          efeitos <- "Temperatura extrema pode levar à morte dos suínos."
          estrategias <- "Use aquecimento ou resfriamento urgente para evitar perdas."
        } else if (grepl("Desconforto", status_clima)) {
          efeitos <- "Calor reduz ganho de peso; frio causa letargia em leitões."
          estrategias <- paste(itgu_avaliacao, iapfc_avaliacao, "Ventilação, aspersão ou pisos aquecidos conforme necessário.")
        } else {
          efeitos <- "Condições de conforto. Mantenha ventilação e umidade ideal."
          estrategias <- "Monitorar índices para ajustes preventivos."
        }
      }
      return(paste("Efeitos:", efeitos, "\nRecomendações:", estrategias))
    }
    
    # Cálculos
    tpo <- calcular_ponto_orvalho(input$TBS, input$UR)
    itgu <- calcular_ITGU(input$TGN, tpo)
    ctr <- calcular_CTR(input$TGN, input$TBS, input$VV)
    iapfc <- calcular_IAPfc(input$TBS, input$UR, input$VV)
    
    # Avaliações
    clima_result <- avaliar_temperatura_umidade(input$especie, input$categoria, input$TBS, input$UR)
    vv_result <- avaliar_velocidade_vento(input$especie, input$categoria, input$VV)
    itgu_avaliacao <- avaliar_ITGU(itgu)
    iapfc_avaliacao <- avaliar_IAPfc(iapfc)
    
    # Exibir resultados
    output$Temperatura_Umidade <- renderText({
      paste("Avaliação para", input$especie, "(", input$categoria, "):", clima_result$status)
    })
    
    output$Velocidade_Vento <- renderText({
      vv_result$status
    })
    
    output$ITGU_resultado <- renderText({
      paste("ITGU:", round(itgu, 2))
    })
    
    output$ITGU_avaliacao <- renderText({
      itgu_avaliacao
    })
    
    output$CTR_resultado <- renderText({
      paste("CTR:", round(ctr, 2))
    })
    
    output$IAPfc_resultado <- renderText({
      paste("IAPfc:", round(iapfc, 2))
    })
    
    output$IAPfc_avaliacao <- renderText({
      iapfc_avaliacao
    })
    
    output$Efeitos_Estrategias <- renderText({
      efeitos_estrategias(input$especie, input$categoria, clima_result$status, vv_result, itgu_avaliacao, iapfc_avaliacao,
                          clima_result$temp_min, clima_result$temp_max, clima_result$ur_min, clima_result$ur_max)
    })
  })
}

# Executar o aplicativo
shinyApp(ui, server)
