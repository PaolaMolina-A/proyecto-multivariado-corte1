

library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(knitr)
library(patchwork)
library(writexl)

set.seed(2026)
options(scipen = 999)

ruta_excel <- "C:/Users/paoan/Desktop/Mis cosas/Universidad/Multivariado/Proyecto/proyecto-multivariado-corte1/base_TOP30_PISA_alfabetizacion_financiera.xlsx"

base_pisa_post_corte1 <- read_excel(ruta_excel, sheet = "Datos")

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

