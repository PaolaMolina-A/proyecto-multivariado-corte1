############################### ANÁLISIS DE DATOS PISA 2022 ###############################

# 1. CARGA EXPLÍCITA DE LIBRERÍAS
library(haven)
library(labelled)
library(dplyr)
library(writexl)

# 2. RUTAS Y LECTURA DE BASES DE DATOS
ruta <- "base_datos"

# Usamos haven::read_sas para asegurar que R encuentre la función sin fallar
datos_cuestionario <- haven::read_sas(file.path(ruta, "CY08MSP_FLT_QQQ.SAS7BDAT"))

# 3. DIMENSIONES Y CLASIFICACIÓN TÉCNICA DE VARIABLES
n_qqq <- ncol(datos_cuestionario)

datos_reales <- as_factor(datos_cuestionario)

total_cualitativas <- datos_reales %>% 
  select(where(is.factor) | where(is.character)) %>% 
  ncol()

total_cuantitativas <- datos_reales %>% 
  select(where(is.numeric)) %>% 
  ncol()

# GENERACIÓN DEL DICCIONARIO UNIFICADO
extraer_diccionario <- function(df, nombre_tabla) {
  tibble(
    Codigo_Variable = names(df),
    Descripcion_Real = sapply(df, function(x) {
      lbl <- attr(x, "label")
      if (is.null(lbl) || length(lbl) == 0) return("Sin descripción textual") else return(as.character(lbl))
    })
  )
}

dicc_qqq <- extraer_diccionario(datos_cuestionario, "Cuestionario (PDF)")

if (!file.exists("diccionario_PISA.xlsx")) {
  write_xlsx(dicc_qqq, "diccionario_PISA.xlsx")
}

cat("\n==============================================",
    "\nDESGLOSE GENERAL DEL PROYECTO PISA 2022",
    "\n==============================================",
    "\n1. Cuestionario de Contexto (_QQQ):", n_qqq, "variables",
    "\n   - Cualitativas con texto directo:", total_cualitativas,
    "\n   - Códigos numéricos en el SAS:", total_cuantitativas,
    "\n==============================================\n")

# 4. DEFINICIÓN DE LAS 30 VARIABLES DEFINITIVAS (15 CUANTITATIVAS + 15 CUALITATIVAS)
variables_cuantitativas_final <- c(
  "PV1FLIT", "PV1MATH", "PV1READ", "ESCS", "HOMEPOS",
  "FLCONFIN", "ACCESSFP", "FCFMLRTY", "FLSCHOOL", "FLFAMILY",
  "HISEI", "PAREDINT", "FRINFLFM", "CREATSCH", "FAMSUPSL"
)

variables_cualitativas_final <- c(
  "CNT", "ST004D01T", "ST255Q01JA", "FL161Q01HA", "FL161Q02HA",
  "FL169Q05JA", "FL159Q04HA", "FL171Q11JA", "FL171Q12JA", "ISCEDP",
  "FL170Q01JA", "FL163Q02HA", "ST268Q04JA", "IC172Q05JA", "IC172Q07JA"
)

variables_finales <- c(variables_cuantitativas_final, variables_cualitativas_final)

# Construcción de la base de trabajo definitiva (ID + 30 variables)
base_final <- datos_cuestionario %>%
  select(CNTSTUID, all_of(variables_finales))

cat("Base final estructurada con éxito:", nrow(base_final), "estudiantes y", ncol(base_final) - 1, "variables analíticas.\n")

# 5. CONSTRUCCIÓN DEL DICCIONARIO DE LAS 30 VARIABLES FINALES
diccionario_final <- dicc_qqq %>%
  filter(Codigo_Variable %in% variables_finales) %>%
  distinct(Codigo_Variable, .keep_all = TRUE) %>%
  mutate(Grupo = if_else(Codigo_Variable %in% variables_cuantitativas_final, "Cuantitativa", "Cualitativa")) %>%
  select(Codigo_Variable, Grupo, Descripcion_Real) %>%
  arrange(match(Codigo_Variable, variables_finales))

# 6. EXPORTAR A EXCEL CON 2 HOJAS (DATOS Y DICCIONARIO)
write_xlsx(
  list(
    "Datos" = base_final,
    "Diccionario_Variables" = diccionario_final
  ),
  path = "base_TOP30_PISA_alfabetizacion_financiera.xlsx"
)

cat("\n✅ Archivo exportado exitosamente: base_TOP30_PISA_alfabetizacion_financiera.xlsx\n")