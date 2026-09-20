# ==============================================================================
# PROYECTO PISA 2022 - CORTE 1: ANÁLISIS UNIVARIADO Y BIVARIADO
# ==============================================================================


library(readxl)
library(dplyr)
library(ggplot2)


# Carga de la base de datos preseleccionada (40 variables)
datos <- read_excel("base_TOP40_PISA_alfabetizacion_financiera.xlsx", sheet = "Datos")
diccionario <- read_excel("base_TOP40_PISA_alfabetizacion_financiera.xlsx", sheet = "Diccionario_Variables")

# Confirmación de carga en consola
cat("Base cargada exitosamente para el Corte 1:", nrow(datos), "estudiantes y", ncol(datos), "columnas.\n")