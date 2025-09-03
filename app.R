############################################################
# App mínima Shiny: slider de TEA que actualiza los gráficos
# Paquetes: tidyverse, scales (ya usabas) + shiny (nuevo, necesario)
############################################################

library(tidyverse)
library(scales)
library(shiny)

# ======== Helpers (idénticos a tu script) =================
tna_a_tea <- function(tna, k = 12) (1 + tna/k)^k - 1
tea_a_efectiva_horizonte <- function(tea, meses) (1 + tea)^(meses/12) - 1

modelo_rendimiento_fx <- function(capital_inicial_ars,
                                  tea,
                                  horizonte_meses,
                                  tc_inicial,
                                  tc_final_min,
                                  tc_final_max,
                                  pasos = 41) {
  stopifnot(capital_inicial_ars > 0,
            tea > -1, horizonte_meses > 0,
            tc_inicial > 0, tc_final_min > 0,
            tc_final_max > tc_final_min, pasos >= 2)
  r_total <- tea_a_efectiva_horizonte(tea, horizonte_meses)
  capital_final_ars <- capital_inicial_ars * (1 + r_total)
  tc_final_vec <- seq(tc_final_min, tc_final_max, length.out = pasos)
  capital_inicial_usd <- capital_inicial_ars / tc_inicial
  
  tabla <- tibble(
    horizonte_meses = horizonte_meses,
    tea = tea,
    r_total = r_total,
    tc_inicial = tc_inicial,
    tc_final = tc_final_vec,
    devaluacion_pct = (tc_final / tc_inicial - 1),
    capital_final_ars = capital_final_ars,
    capital_inicial_usd = capital_inicial_usd,
    capital_final_usd = capital_final_ars / tc_final,
    retorno_usd_pct = capital_final_usd / capital_inicial_usd - 1
  )
  
  tc_break_even <- tc_inicial * (1 + r_total)
  list(tabla = tabla, tc_break_even = tc_break_even, r_total = r_total)
}

simular_con_banda <- function(capital_inicial_ars,
                              tea,
                              horizonte_meses,
                              banda_min_0,
                              banda_max_0,
                              drift_mensual = 0.01,
                              pasos = 41,
                              tc_inicial = (banda_min_0 + banda_max_0)/2) {
  stopifnot(banda_min_0 > 0, banda_max_0 > banda_min_0, drift_mensual > -1)
  factor_drift <- (1 + drift_mensual) ^ horizonte_meses
  banda_min_T  <- banda_min_0 * factor_drift
  banda_max_T  <- banda_max_0 * factor_drift
  
  base <- modelo_rendimiento_fx(
    capital_inicial_ars = capital_inicial_ars,
    tea                 = tea,
    horizonte_meses     = horizonte_meses,
    tc_inicial          = tc_inicial,
    tc_final_min        = banda_min_T,
    tc_final_max        = banda_max_T,
    pasos               = pasos
  )
  
  bandas_mensuales <- tibble(
    mes = 0:horizonte_meses,
    banda_min = banda_min_0 * (1 + drift_mensual) ^ mes,
    banda_max = banda_max_0 * (1 + drift_mensual) ^ mes
  )
  
  mejor <- base$tabla %>% slice_min(tc_final, with_ties = FALSE)
  peor  <- base$tabla %>% slice_max(tc_final, with_ties = FALSE)
  posicion_be <- dplyr::case_when(
    base$tc_break_even < banda_min_T ~ "break-even por DEBAJO de la banda",
    base$tc_break_even > banda_max_T ~ "break-even por ENCIMA de la banda",
    TRUE                             ~ "break-even DENTRO de la banda"
  )
  
  list(
    tabla             = base$tabla,
    tc_break_even     = base$tc_break_even,
    r_total           = base$r_total,
    banda_min_T       = banda_min_T,
    banda_max_T       = banda_max_T,
    bandas_mensuales  = bandas_mensuales,
    mejor             = mejor,
    peor              = peor,
    posicion_be       = posicion_be
  )
}

# ======== Parámetros por defecto (editables) ==============
cap_ars     <- 1e6
meses       <- 12
banda_min_0 <- 1100
banda_max_0 <- 1400
drift       <- 0.01
n_pasos     <- 61
tc0_medio   <- (banda_min_0 + banda_max_0) / 2

# =================== UI ===================================
ui <- fluidPage(
  titlePanel("Retorno en USD vs. TC final (bandas con drift 1% mensual)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("tea_pct", "TEA anual (%)",
                  min = 0, max = 120, value = 39, step = 1),
      helpText("Mové el deslizador: la TEA se aplica a pesos y medimos el retorno en USD."),
      tags$hr(),
      p(strong("Parámetros fijos:"), br(),
        paste0("Capital: $", format(cap_ars, big.mark=".")),
        paste0(" | Horizonte: ", meses, " meses"),
        paste0(" | Banda inicial: [", banda_min_0, ", ", banda_max_0, "] ARS/USD"),
        paste0(" | Drift: ", percent(drift), " mensual"))
    ),
    mainPanel(
      plotOutput("plot_retornos", height = 420),
      plotOutput("plot_banda", height = 360)
    )
  )
)

# =================== SERVER ===============================
server <- function(input, output, session){
  
  res <- reactive({
    tea <- input$tea_pct / 100  # convertir del slider (%) a fracción
    simular_con_banda(
      capital_inicial_ars = cap_ars,
      tea                 = tea,
      horizonte_meses     = meses,
      banda_min_0         = banda_min_0,
      banda_max_0         = banda_max_0,
      drift_mensual       = drift,
      pasos               = n_pasos,
      tc_inicial          = tc0_medio
    )
  })
  
  output$plot_retornos <- renderPlot({
    r <- res()
    r$tabla %>%
      ggplot(aes(x = tc_final, y = retorno_usd_pct)) +
      annotate("rect",
               xmin = r$banda_min_T, xmax = r$banda_max_T,
               ymin = -Inf, ymax = Inf, alpha = 0.08) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = r$tc_break_even, linetype = "dotted") +
      geom_vline(xintercept = r$banda_min_T, linetype = "dotdash") +
      geom_vline(xintercept = r$banda_max_T, linetype = "dotdash") +
      annotate("label",
               x = r$tc_break_even, y = 0.02,
               label = paste0("Break-even ≈ ", number(r$tc_break_even, accuracy = 1)),
               size = 3) +
      scale_y_continuous(labels = percent) +
      scale_x_continuous(labels = number_format(accuracy = 1)) +
      labs(
        title = paste0("Retorno en USD vs. TC final dentro de la banda (TEA ", percent(input$tea_pct/100), ")"),
        subtitle = paste0("Capital $", format(cap_ars, big.mark = "."),
                          " | Horizonte ", meses, " meses",
                          " | Banda final [",
                          number(r$banda_min_T, accuracy = 1), ", ",
                          number(r$banda_max_T, accuracy = 1), "] ARS/USD"),
        x = "Tipo de cambio final (ARS por USD)",
        y = "Retorno en USD (%)"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$plot_banda <- renderPlot({
    r <- res()
    r$bandas_mensuales %>%
      mutate(tc_medio = (banda_min + banda_max) / 2) %>%
      ggplot(aes(x = mes)) +
      geom_ribbon(aes(ymin = banda_min, ymax = banda_max),
                  fill = "#2C7FB8", alpha = 0.18) +
      geom_line(aes(y = banda_min), linetype = "dashed") +
      geom_line(aes(y = banda_max), linetype = "dashed") +
      geom_line(aes(y = tc_medio), linewidth = 1) +
      labs(
        title = "Evolución de la banda cambiaria (+1% mensual)",
        subtitle = paste0("Banda inicial [", banda_min_0, ", ", banda_max_0, "] → ",
                          "Banda final [",
                          number(r$banda_min_T, accuracy = 1), ", ",
                          number(r$banda_max_T, accuracy = 1), "] ARS/USD"),
        x = "Mes", y = "Tipo de cambio (ARS/USD)"
      ) +
      scale_y_continuous(labels = number_format(accuracy = 1)) +
      theme_minimal(base_size = 12)
  })
}

# Lanzar la app
shinyApp(ui, server)
