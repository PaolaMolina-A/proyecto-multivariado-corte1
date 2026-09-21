# ==============================================================================
# PROYECTO PISA 2022 - CORTE 1: ANÁLISIS UNIVARIADO Y BIVARIADO
# ==============================================================================

options(digits = 4, scipen = 999)
library(readxl)
library(dplyr)
library(ggplot2)


# Carga de la base de datos preseleccionada (40 variables)
datos <- read_excel("base_TOP20_PISA_alfabetizacion_financiera.xlsx", sheet = "Datos")
diccionario <- read_excel("base_TOP20_PISA_alfabetizacion_financiera.xlsx", sheet = "Diccionario_Variables")

# Confirmación de carga en consola
cat("Base cargada exitosamente para el Corte 1:", nrow(datos), "estudiantes y", ncol(datos), "columnas.\n")


summary(base_top20)

########################################################################################
  # 1. DIAGNÓSTICO GENERAL DE DATOS FALTANTES (MISSING VALUES / NA)


# Conteo de NAs por variable en toda la base
conteo_na <- colSums(is.na(datos))

pct_na <- (conteo_na / nrow(datos)) * 100

diagnostico_na <- data.frame(
  Variable = names(conteo_na),
  Total_NA = conteo_na,
  Porcentaje_NA = round(pct_na, 2)
)

print(diagnostico_na)

##########################################Análisis Univariado###############################################











##########################################Análisis Bivariado###############################################






