############################### ANÁLISIS DE DATOS PISA 2022 ###############################

# 1. CARGA DE LIBRERÍAS
# Si algún paquete no está instalado, ejecuta una sola vez: install.packages(c("haven", "labelled", "dplyr", "writexl"))
# 1. CARGA DE LIBRERÍAS
paquetes <- c(
  "haven", "labelled", "dplyr", "writexl"
)
# Verificamos qué paquetes faltan. La instalación se hace por fuera de la
# compilación para evitar cambios inesperados en el entorno del estudiante.
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  stop(
    "Faltan paquetes: ", paste(pendientes, collapse = ", "),
    ". Instálelos con install.packages(c(",
    paste(sprintf('"%s"', pendientes), collapse = ", "), "))"
  )
}
invisible(lapply(paquetes, library, character.only = TRUE))

# 2. RUTAS Y LECTURA DE BASES DE DATOS
ruta <- "base_datos"

datos_cuestionario <- read_sas(file.path(ruta, "CY08MSP_FLT_QQQ.SAS7BDAT"))


# 3. DIMENSIONES Y CLASIFICACIÓN TÉCNICA DE VARIABLES
n_qqq <- ncol(datos_cuestionario)

datos_reales <- as_factor(datos_cuestionario)

total_cualitativas <- datos_reales %>% 
  select(where(is.factor) | where(is.character)) %>% 
  ncol()

total_cuantitativas <- datos_reales %>% 
  select(where(is.numeric)) %>% 
  ncol()

#  GENERACIÓN DEL DICCIONARIO UNIFICADO, para conocer las etiquetas de cada variable
if (!file.exists("diccionario_UNIFICADO_PISA.xlsx")) {
  extraer_diccionario <- function(df, nombre_tabla) {
    df_factor <- as_factor(df)

    tibble(
      Codigo_Variable = names(df),
      Descripcion_Real = sapply(df, function(x) {
        lbl <- attr(x, "label")
        if (is.null(lbl) || length(lbl) == 0) return("Sin descripción textual") else return(as.character(lbl))
      }),
      Tipo_Variable = sapply(df, function(x) {
        # Si el formato base es texto o factor, es cualitativa directa
        if (is.character(x) || is.factor(x)) {
          return("Cualitativa (Texto)")
        }
        # Si es numérica, analizamos su comportamiento estadístico real
        if (is.numeric(x)) {
          n_unicos <- length(unique(na.omit(x)))
          # Umbral de control: 15 valores o menos representan categorías disfrazadas
          if (n_unicos <= 15) {
            return("Cualitativa (Numérica Discreta/Ordinal)")
          } else {
            return("Cuantitativa (Continua)")
          }
        }
        return("Desconocido")
      })
    )
  }

  dicc_qqq <- extraer_diccionario(datos_cuestionario, "Cuestionario (PDF)")

  write_xlsx(dicc_qqq, "diccionario_PISA.xlsx")
}

cat("\n==============================================",
    "\nDESGLOSE GENERAL DEL PROYECTO PISA 2022",
    "\n==============================================",
    "\n1. Cuestionario de Contexto (_QQQ):", n_qqq, "variables",
    "\n   - Cualitativas con texto directo:", total_cualitativas,
    "\n   - Códigos numéricos en el SAS:", total_cuantitativas,
    "\n==============================================\n")



#______________________________________________________________________________________________________________
# ############################### construimos la base de datos final###############################
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

# Construcción de la base de trabajo definitiva
base_final <- datos_cuestionario %>%
  select(CNTSTUID, all_of(variables_finales))

cat("Base final estructurada con éxito:", nrow(base_final), "estudiantes y", ncol(base_final) - 1, "variables analíticas.\n")


# 4.3 Diccionario de las 20 variables finales
diccionario_final <- dicc_qqq %>%
  filter(Codigo_Variable %in% variables) %>%
  relocate(Tipo_Variable, .after = Codigo_Variable)
# 4.4 Exportar a Excel con 2 hojas
write_xlsx(
  list(
    "Datos" = datos_final,
    "Diccionario_Variables" = diccionario_final
  ),
  path = "base_TOP20_PISA_alfabetizacion_financiera.xlsx"
)


