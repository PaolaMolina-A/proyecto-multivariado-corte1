############################### ANÁLISIS DE DATOS PISA 2022 ###############################

# 1. CARGA DE LIBRERÍAS
# Si algún paquete no está instalado, ejecuta una sola vez: install.packages(c("haven", "labelled", "dplyr", "writexl"))
library(haven)
library(labelled)
library(dplyr)
library(writexl)

# 2. RUTAS Y LECTURA DE BASES DE DATOS
ruta <- "base_datos"

datos_cuestionario <- read_sas(file.path(ruta, "CY08MSP_FLT_QQQ.SAS7BDAT"))
datos_cognitivos   <- read_sas(file.path(ruta, "CY08MSP_FLT_COG.SAS7BDAT"))

# 3. DIMENSIONES Y CLASIFICACIÓN TÉCNICA DE VARIABLES
n_qqq <- ncol(datos_cuestionario)
n_cog <- ncol(datos_cognitivos)

datos_reales <- as_factor(datos_cuestionario)

total_cualitativas <- datos_reales %>% 
  select(where(is.factor) | where(is.character)) %>% 
  ncol()

total_cuantitativas <- datos_reales %>% 
  select(where(is.numeric)) %>% 
  ncol()

# 4. GENERACIÓN DEL DICCIONARIO UNIFICADO (Creación automática condicional)
if (!file.exists("diccionario_UNIFICADO_PISA.xlsx")) {
  extraer_diccionario <- function(df, nombre_tabla) {
    df_factor <- as_factor(df)
    
    tibble(
      Tabla_Origen = nombre_tabla,
      Codigo_Variable = names(df),
      Descripcion_Real = sapply(df, function(x) {
        lbl <- attr(x, "label")
        if (is.null(lbl) || length(lbl) == 0) return("Sin descripción textual") else return(as.character(lbl))
      }),
      Tipo_Variable = sapply(df_factor, function(x) {
        if (is.factor(x) || is.character(x)) "Cualitativa (Texto)" else "Cuantitativa / Código Numérico"
      })
    )
  }
  
  dicc_qqq <- extraer_diccionario(datos_cuestionario, "Cuestionario (PDF)")
  dicc_cog <- extraer_diccionario(datos_cognitivos, "Notas Cognitivas (Examen)")
  
  diccionario_unificado <- bind_rows(dicc_qqq, dicc_cog)
  write_xlsx(diccionario_unificado, "diccionario_UNIFICADO_PISA.xlsx")
}

# 5. REPORTE EN CONSOLA
cat("\n==============================================",
    "\nDESGLOSE GENERAL DEL PROYECTO PISA 2022",
    "\n==============================================",
    "\n1. Cuestionario de Contexto (_QQQ):", n_qqq, "variables",
    "\n   - Cualitativas con texto directo:", total_cualitativas,
    "\n   - Códigos numéricos en el SAS:", total_cuantitativas,
    "\n2. Resultados Cognitivos (_COG):", n_cog, "variables",
    "\n----------------------------------------------",
    "\nTotal de variables disponibles:", n_qqq + n_cog, "variables",
    "\n==============================================\n")