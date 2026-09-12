############################### este documento será usado para el analisis de datos
install.packages("haven", "dplyr","labelled")
library(haven)
library(labelled)
library(dplyr)
ruta <- "F:/Estadistica multivariada/proyecto-multivariado-corte1/base_datos"

#cuestionario principal 
datos_cuestionario <- read_sas(paste0(ruta, "/", "CY08MSP_FLT_QQQ.SAS7BDAT"))

#  resultados cognitivos:
datos_cognitivos <- read_sas(paste0(ruta, "/", "CY08MSP_FLT_COG.SAS7BDAT"))
head(datos_cuestionario)


datos_reales <- as_factor(datos_cuestionario)

total_cualitativas <- datos_reales %>% 
  select(where(is.factor) | where(is.character)) %>% 
  ncol()

total_cuantitativas <- datos_reales %>% 
  select(where(is.numeric)) %>% ncol()


cat("\nCualitativas:", total_cualitativas, "\nCuantitativas:", total_cuantitativas, "\n")
