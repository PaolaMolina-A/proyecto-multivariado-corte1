

library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(knitr)
library(patchwork)
library(writexl)

set.seed(2026)
options(scipen = 999)


base_pisa_post_corte1 <- read.csv("base_final_pisa.csv")

base_con_subregiones <- base_pisa_post_corte1 %>%
  mutate(
    Subregion = case_when(
      CNT %in% c("Canada", "United States", "CAN", "USA") ~ "Norteamerica",
      CNT %in% c("Brazil", "Peru", "Costa Rica", "BRA", "PER", "CRI") ~ "America_Latina",
      CNT %in% c("Belgium", "Netherlands", "Norway", "Denmark", "Austria", "Italy", "Spain", "Portugal",
                 "BEL", "NLD", "NOR", "DNK", "AUT", "ITA", "ESP", "PRT") ~ "Europa_Occidental",
      CNT %in% c("Czech Republic", "Poland", "Hungary", "Bulgaria", 
                 "CZE", "POL", "HUN", "BGR") ~ "Europa_Oriental",
      CNT %in% c("United Arab Emirates", "Saudi Arabia", "ARE", "SAU") ~ "Medio_Oriente",
      CNT %in% c("Malaysia", "MYS") ~ "Sudeste_Asiatico",
      TRUE ~ "Otro"
    )
  )

# ==============================================================================
# TÉCNICA 1: MUESTREO ALEATORIO SIMPLE (10,000 INDIVIDUOS)
# ==============================================================================


dir_t1 <- "corte_2/Bases_testeo/01_Aleatorio_Simple"
dir.create(dir_t1, recursive = TRUE, showWarnings = FALSE)


muestra_10k_aleatorio_simple <- base_con_subregiones %>%
  slice_sample(n = 10000)


ruta_salida_t1 <- file.path(dir_t1, "base_pisa_10k_aleatorio_simple.xlsx")
write_xlsx(muestra_10k_aleatorio_simple, ruta_salida_t1)


cat("✓ Técnica 1 exportada a Excel exitosamente en:\n  ", ruta_salida_t1, "\n\n")

cat("--- CONTEO DE OBSERVACIONES POR SUBREGIÓN ---\n")
print(table(muestra_10k_aleatorio_simple$Subregion))

cat("\n--- CONTEO DE OBSERVACIONES POR PAÍS (CNT) ---\n")
print(table(muestra_10k_aleatorio_simple$CNT))


# ==============================================================================

# graficas para elegir los intervalos de la variable puntaje academico

# ==============================================================================
# DIAGNÓSTICO ESTADÍSTICO Y DISCRETIZACIÓN DE PV1FLIT PARA EL ACS
# ==============================================================================




library(dplyr)
library(ggplot2)
library(patchwork)
library(knitr)
library(moments) # Para skewness y kurtosis

# ------------------------------------------------------------------------------
# 1. RESUMEN DE MEDIDAS DE TENDENCIA CENTRAL, DISPERSIÓN Y FORMA
# ------------------------------------------------------------------------------
resumen_pv1flit <- datos_c2 %>%
  summarise(
    Minimo     = min(PV1FLIT, na.rm = TRUE),
    Q1         = quantile(PV1FLIT, 0.25, na.rm = TRUE),
    Mediana    = median(PV1FLIT, na.rm = TRUE),
    Media      = mean(PV1FLIT, na.rm = TRUE),
    Q3         = quantile(PV1FLIT, 0.75, na.rm = TRUE),
    Maximo     = max(PV1FLIT, na.rm = TRUE),
    Desv_Est   = sd(PV1FLIT, na.rm = TRUE),
    IQR        = IQR(PV1FLIT, na.rm = TRUE),
    Asimetria  = skewness(PV1FLIT, na.rm = TRUE),
    Curtosis   = kurtosis(PV1FLIT, na.rm = TRUE)
  )

kable(resumen_pv1flit, digits = 2, caption = "Medidas Descriptivas y de Forma de PV1FLIT (N = 10,000)")

# ------------------------------------------------------------------------------
# 2. CÁLCULO DE CORTES SEGÚN DOS CRITERIOS METODOLÓGICOS
# ------------------------------------------------------------------------------

# Criterio A: Quintiles Equiprobables (5 grupos del 20% de la masa de datos)
cortes_quintiles <- quantile(datos_c2$PV1FLIT, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Criterio B: Niveles de Competencia Oficiales PISA OCDE para Alfabetización Financiera
# (Nivel 1 o inferior: <400, Nivel 2: 400-479, Nivel 3: 480-549, Nivel 4: 550-624, Nivel 5: >=625)
cortes_pisa <- c(-Inf, 400, 480, 550, 625, Inf)

cat("\n--- CORTES POR QUINTILES (EQUIPROBABLES - 20% C/U) ---\n")
print(round(cortes_quintiles, 2))

# ------------------------------------------------------------------------------
# 3. VISUALIZACIÓN DIAGNÓSTICA CON LÍNEAS DE CORTE
# ------------------------------------------------------------------------------

# Gráfico 1: Histograma + Densidad con Cortes por Quintiles
g_quintiles <- ggplot(datos_c2, aes(x = PV1FLIT)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35, fill = "#219EBC", color = "white", alpha = 0.7) +
  geom_density(color = "#023047", linewidth = 1) +
  geom_vline(xintercept = cortes_quintiles[2:5], color = "#FB8500", linetype = "dashed", linewidth = 0.9) +
  labs(
    title = "Opción A: Discretización por Quintiles (20% c/u)",
    subtitle = "Líneas punteadas representan los 5 rangos equiprobables",
    x = "Puntaje PV1FLIT", y = "Densidad"
  ) +
  theme_minimal()

# Gráfico 2: Histograma + Densidad con Cortes Oficiales PISA OCDE
g_pisa <- ggplot(datos_c2, aes(x = PV1FLIT)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35, fill = "#8ECAE6", color = "white", alpha = 0.7) +
  geom_density(color = "#023047", linewidth = 1) +
  geom_vline(xintercept = c(400, 480, 550, 625), color = "#C70039", linetype = "dashed", linewidth = 0.9) +
  labs(
    title = "Opción B: Discretización por Niveles PISA OCDE",
    subtitle = "Líneas rojas representan los umbrales de desempeño cognitivo",
    x = "Puntaje PV1FLIT", y = "Densidad"
  ) +
  theme_minimal()

# Despliegue en panel comparativo
g_quintiles / g_pisa



# ==============================================================================
# EXPLORACIÓN VISUAL DETALLADA DE PV1FLIT (HISTOGRAMA + DENSIDAD + BOXPLOT)
# ==============================================================================

library(ggplot2)
library(patchwork)
library(dplyr)
library(scales)

# 1. Definición de paleta corporativa
col_principal <- "#023047"
col_secundario <- "#219EBC"
col_acento     <- "#FB8500"
col_fondo      <- "#F8F9FA"

# 2. Histograma con curva Kernel y marcas de cuartiles
g_hist <- ggplot(datos_c2, aes(x = PV1FLIT)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, 
                 fill = col_secundario, color = "white", alpha = 0.8) +
  geom_density(color = col_principal, linewidth = 1.2) +
  # Líneas de Referencia: Media/Mediana y Cuartiles (Q1, Q3)
  geom_vline(xintercept = median(datos_c2$PV1FLIT, na.rm = TRUE), 
             color = col_acento, linetype = "solid", linewidth = 1) +
  geom_vline(xintercept = c(quantile(datos_c2$PV1FLIT, 0.25, na.rm = TRUE),
                            quantile(datos_c2$PV1FLIT, 0.75, na.rm = TRUE)), 
             color = col_principal, linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = 473, y = 0.0036, label = "Mediana = 473.2", 
           color = col_acento, fontface = "bold", size = 3.8, angle = 90, vjust = -0.5) +
  labs(
    title = "Distribución Continuo-Gaussiana de PV1FLIT",
    subtitle = "Histograma de densidad kernel con marcas de dispersión central (N = 10,000)",
    x = "Puntaje de Alfabetización Financiera",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = col_principal),
    plot.background = element_rect(fill = col_fondo, color = NA),
    panel.grid.minor = element_blank()
  )

# 3. Boxplot Horizontal con Marcas Numéricas de los 5 Números Resumen
resumen <- quantile(datos_c2$PV1FLIT, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

g_box <- ggplot(datos_c2, aes(x = PV1FLIT, y = "")) +
  geom_boxplot(fill = "#8ECAE6", color = col_principal, 
               outlier.color = "#E72929", outlier.alpha = 0.3, outlier.size = 1.5,
               width = 0.5, alpha = 0.9) +
  # Etiquetas de los 5 números sobre la gráfica
  annotate("text", x = resumen[1], y = 1.3, label = paste0("Mín: ", round(resumen[1], 1)), size = 3, fontface = "bold") +
  annotate("text", x = resumen[2], y = 0.6, label = paste0("Q1: ", round(resumen[2], 1)), size = 3, fontface = "bold") +
  annotate("text", x = resumen[3], y = 1.3, label = paste0("Mediana: ", round(resumen[3], 1)), size = 3, fontface = "bold", color = col_acento) +
  annotate("text", x = resumen[4], y = 0.6, label = paste0("Q3: ", round(resumen[4], 1)), size = 3, fontface = "bold") +
  annotate("text", x = resumen[5], y = 1.3, label = paste0("Máx: ", round(resumen[5], 1)), size = 3, fontface = "bold") +
  labs(
    title = "Diagrama de Caja (Boxplot) de PV1FLIT",
    subtitle = "Detección visual de simetría y dispersión intercuartílica (IQR = 152.06)",
    x = "Puntaje PV1FLIT",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = col_principal),
    plot.background = element_rect(fill = col_fondo, color = NA),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

# 4. Consolidación en un solo panel de inspección
panel_exploratorio <- g_hist / g_box + plot_layout(heights = c(2, 1))
print(panel_exploratorio)
