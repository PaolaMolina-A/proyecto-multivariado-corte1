############################### ANÁLISIS DE DATOS PISA 2022 ###############################

# 1. CARGA DE LIBRERÍAS
# Si algún paquete no está instalado, ejecuta una sola vez: install.packages(c("haven", "labelled", "dplyr", "writexl"))
library(haven)
library(labelled)
library(dplyr)
library(writexl)
library(haven)

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


############################### PASO 1 (v2): JOIN LIMPIO + CLASIFICACIÓN ###############################

# 1.1 Antes de unir, nos quedamos SOLO con lo que aporta del Cognitivo:
#     la clave (CNTSTUID) + el pequeño rescate de variables de diseño/ruta.
#     Todo lo demás del Cognitivo (paradata, IDs duplicados) se descarta ANTES del join.
rescate_cog <- c("MS1_TEST","MS2_TEST","MS1_LEV","MS2_LEV","MS1_ItemSet","MS2_ItemSet",
                 "RCORE_TEST","RCORE_PERF","RS1_TEST","RS1_LEV","RS2_TEST","RS2_LEV",
                 "RCO1S_PERF","ISCEDP","RDESIGN","MPATH")
rescate_cog <- intersect(rescate_cog, names(datos_cognitivos))

datos_cog_reducido <- datos_cognitivos %>% select(CNTSTUID, all_of(rescate_cog))

# 1.2 Join limpio: ya no hay columnas duplicadas porque datos_cog_reducido no trae metadatos
datos_completos <- inner_join(datos_cuestionario, datos_cog_reducido, by = "CNTSTUID")
cat("Dimensiones de la base unificada:", nrow(datos_completos), "filas y", ncol(datos_completos), "columnas.\n\n")

# 1.3 Exclusión técnica AMPLIADA (ahora sin riesgo de .x/.y)
pesos_muestrales   <- grep("^W_FSTU|^UNIT$|^WVARSTRR$|^SENWT$", names(datos_completos), value = TRUE)
ids_puros          <- c("CNTSTUID","CNTSCHID","NatCen","STRATUM","SUBNATIO","CNTRYID","LANGN")
metadatos_tecnicos <- c("CYC","LANGTEST_QQQ","LANGTEST_COG","LANGTEST_PAQ","BOOKID","VER_DAT")
paradata_extras    <- grep("^Option_", names(datos_completos), value = TRUE)
pvs_secundarios    <- grep("^PV[2-9]|^PV10", names(datos_completos), value = TRUE)

variables_a_descartar <- unique(c(pesos_muestrales, ids_puros, metadatos_tecnicos, paradata_extras, pvs_secundarios))

# 1.4 Clasificación por tipo (numérica sin etiqueta = cuanti; con etiqueta o pocas categorías = cuali)
candidatas_cuanti <- names(datos_completos)[sapply(datos_completos, is.numeric)]
candidatas_cuanti <- setdiff(candidatas_cuanti, c(variables_a_descartar, rescate_cog))

n_unicos <- sapply(datos_completos[candidatas_cuanti], function(x) length(unique(na.omit(x))))
es_continua   <- (n_unicos > 15) | grepl("^PV1|^ESCS", candidatas_cuanti)
cuanti_reales <- candidatas_cuanti[es_continua]
cuali_numericas <- candidatas_cuanti[!es_continua]

candidatas_cuali_texto <- setdiff(names(datos_completos), c(candidatas_cuanti, variables_a_descartar, rescate_cog))
cuali_reales <- unique(c(candidatas_cuali_texto, cuali_numericas))

cat("Cuantitativas reales:", length(cuanti_reales), "\n")
cat("Cualitativas reales (Cuestionario):", length(cuali_reales), "\n")
cat("Rescate Cognitivo (bolsa aparte):", length(rescate_cog), "\n")

############################### PASO 2 (v3): DIAGNÓSTICO Y TOP 40 (con correcciones) ###############################

# 2.0 CORRECCIONES sobre las listas de candidatas (no borran nada de datos_completos,
#     solo ajustan qué nombres de columna vamos a considerar en el ranking)

# Corrección A: sacar REGION de las cuantitativas (es un código administrativo, no una medida continua)
cuanti_reales_ajustado <- setdiff(cuanti_reales, "REGION")

# Corrección B: colapsar la batería ST297 (5 preguntas de la misma pregunta madre) a solo 1 representante
bateria_st297 <- grep("^ST297", cuali_reales, value = TRUE)
cuali_reales_ajustado <- setdiff(cuali_reales, setdiff(bateria_st297, "ST297Q09JA"))

cat("Cuantitativas ajustadas:", length(cuanti_reales_ajustado), "\n")
cat("Cualitativas ajustadas: ", length(cuali_reales_ajustado), "\n")

# 2.1 Diagnóstico y ranking de CUANTITATIVAS (usando la lista ya corregida)
diag_cuanti <- data.frame(
  variable   = cuanti_reales_ajustado,
  pct_na     = sapply(datos_completos[cuanti_reales_ajustado], function(x) mean(is.na(x)) * 100),
  media      = sapply(datos_completos[cuanti_reales_ajustado], function(x) mean(x, na.rm = TRUE)),
  desviacion = sapply(datos_completos[cuanti_reales_ajustado], function(x) sd(x, na.rm = TRUE))
) %>%
  mutate(coef_variacion = abs(desviacion / media)) %>%
  filter(pct_na < 50) %>%
  arrange(pct_na, desc(coef_variacion))

top20_cuanti <- head(diag_cuanti, 20)

# 2.2 Diagnóstico y ranking de CUALITATIVAS (usando la lista ya corregida)
diag_cuali <- data.frame(
  variable     = cuali_reales_ajustado,
  pct_na       = sapply(datos_completos[cuali_reales_ajustado], function(x) mean(is.na(x)) * 100),
  n_categorias = sapply(datos_completos[cuali_reales_ajustado], function(x) length(unique(na.omit(x)))),
  pct_moda     = sapply(datos_completos[cuali_reales_ajustado], function(x) {
    tab <- table(x); if (length(tab)==0) return(NA); max(tab)/sum(tab)*100
  })
) %>%
  filter(pct_na < 50, n_categorias >= 2, n_categorias <= 12) %>%
  arrange(pct_na, pct_moda)

top20_cuali <- head(diag_cuali, 20)

# 2.3 Bono aparte del Cognitivo (diseño/ruta de aplicación) — ya lo tenías, sin cambios
diag_cog <- data.frame(
  variable     = rescate_cog,
  pct_na       = sapply(datos_completos[rescate_cog], function(x) mean(is.na(x)) * 100),
  n_categorias = sapply(datos_completos[rescate_cog], function(x) length(unique(na.omit(x))))
) %>% arrange(pct_na)

# 2.4 Reporte final
cat("\n== TOP 20 CUANTITATIVAS ==\n")
print(top20_cuanti[, c("variable", "pct_na", "media", "desviacion", "coef_variacion")])

cat("\n== TOP 20 CUALITATIVAS ==\n")
print(top20_cuali[, c("variable", "pct_na", "n_categorias", "pct_moda")])

cat("\n== BONO Cognitivo (diseño/ruta) ==\n")
print(diag_cog)

############################### PASO 2.5: LIMPIEZA FINAL DE REDUNDANCIAS EN CUALITATIVAS ###############################

# SKIPPING ya resume lo mismo que ST062Q01TA, y TARDYSD lo mismo que ST062Q03TA
# -> quitamos los ítems crudos duplicados y dejamos que entren las siguientes variables de la fila
redundantes <- c("ST062Q01TA", "ST062Q03TA")

top20_cuali_final <- diag_cuali %>%
  filter(!variable %in% redundantes) %>%
  head(20)

cat("\n== TOP 20 CUALITATIVAS (versión final, sin redundancias) ==\n")
print(top20_cuali_final[, c("variable", "pct_na", "n_categorias", "pct_moda")])



############################### PASO 2.6: DIAGNÓSTICO DE LAS 5 VARIABLES DE ALFABETIZACIÓN FINANCIERA ###############################

candidatas_fl <- c("FL169Q05JA", "FLCONFIN", "ACCESSFP", "FL161Q01HA", "WB164Q01HA")

# Verificamos primero que existan en la base unificada
candidatas_fl_existentes <- intersect(candidatas_fl, names(datos_completos))
cat("Variables encontradas en datos_completos:", length(candidatas_fl_existentes), "de", length(candidatas_fl), "\n")
print(candidatas_fl_existentes)

# Diagnóstico completo: % NA, clase de dato, y si aplica, categorías o estadísticas descriptivas
diag_fl <- data.frame(
  variable    = candidatas_fl_existentes,
  clase       = sapply(datos_completos[candidatas_fl_existentes], function(x) class(x)[1]),
  pct_na      = sapply(datos_completos[candidatas_fl_existentes], function(x) mean(is.na(x)) * 100),
  n_categorias = sapply(datos_completos[candidatas_fl_existentes], function(x) length(unique(na.omit(x)))),
  media       = sapply(datos_completos[candidatas_fl_existentes], function(x) if(is.numeric(x)) mean(x, na.rm=TRUE) else NA),
  desviacion  = sapply(datos_completos[candidatas_fl_existentes], function(x) if(is.numeric(x)) sd(x, na.rm=TRUE) else NA)
)

cat("\n== DIAGNÓSTICO DE LAS 5 CANDIDATAS DE ALFABETIZACIÓN FINANCIERA ==\n")
print(diag_fl)

# Bonus: para las categóricas, ver la distribución de frecuencias real (útil para tu justificación de por qué se eligieron)
for (v in candidatas_fl_existentes) {
  if (diag_fl$n_categorias[diag_fl$variable == v] <= 12) {
    cat("\nDistribución de", v, ":\n")
    print(table(as_factor(datos_completos[[v]]), useNA = "ifany"))
  }
}


############################### PASO 2.7: BUSCANDO REEMPLAZO PARA WB164Q01HA ###############################

candidatas_reemplazo <- c(
  "FL161Q02HA",  # tiene tarjeta débito (mismo bloque que FL161Q01HA, buen NA esperado)
  "ST258Q01JA",  # no comió por falta de dinero (ítem del núcleo ST, bajo NA esperado)
  "FL171Q08JA",  # con qué frecuencia revisa cuánto dinero tiene
  "FL169Q07JA",  # "ahorro solo si me sobra dinero" (actitud hacia el ahorro)
  "FRINFLFM"     # índice WLE: influencia de amigos en decisiones financieras
)

candidatas_reemplazo_existentes <- intersect(candidatas_reemplazo, names(datos_completos))
cat("Encontradas:", length(candidatas_reemplazo_existentes), "de", length(candidatas_reemplazo), "\n")

diag_reemplazo <- data.frame(
  variable     = candidatas_reemplazo_existentes,
  clase        = sapply(datos_completos[candidatas_reemplazo_existentes], function(x) class(x)[1]),
  pct_na       = sapply(datos_completos[candidatas_reemplazo_existentes], function(x) mean(is.na(x)) * 100),
  n_categorias = sapply(datos_completos[candidatas_reemplazo_existentes], function(x) length(unique(na.omit(x))))
) %>% arrange(pct_na)

cat("\n== DIAGNÓSTICO DE CANDIDATAS DE REEMPLAZO ==\n")
print(diag_reemplazo)


############################### PASO 2.8: AJUSTES FINALES SOBRE LAS LISTAS ###############################

# Quitamos mes de nacimiento y ejercicio; año de nacimiento SÍ se queda, pero lo verificamos primero
cat("Distribución de ST003D03T (año de nacimiento):\n")
print(table(as_factor(datos_completos$ST003D03T), useNA = "ifany"))



############################### PASO 2.9: REEMPLAZO PARA ST003D03T (buscando en el módulo financiero) ###############################

candidatas_fl2 <- c(
  "FL171Q11JA",  # ahorra dinero en una cuenta (banco/online)
  "FL171Q12JA",  # ahorra dinero en casa
  "FL170Q01JA",  # recibe mesada/dinero por hacer tareas del hogar
  "FL167Q03HA",  # habla con sus padres del presupuesto familiar
  "FL159Q04HA"   # es responsable de su propio dinero (evitar robos, etc.)
)

candidatas_fl2_existentes <- intersect(candidatas_fl2, names(datos_completos))
cat("Encontradas:", length(candidatas_fl2_existentes), "de", length(candidatas_fl2), "\n")

diag_fl2 <- data.frame(
  variable     = candidatas_fl2_existentes,
  pct_na       = sapply(datos_completos[candidatas_fl2_existentes], function(x) mean(is.na(x)) * 100),
  n_categorias = sapply(datos_completos[candidatas_fl2_existentes], function(x) length(unique(na.omit(x)))),
  pct_moda     = sapply(datos_completos[candidatas_fl2_existentes], function(x) {
    tab <- table(x); if (length(tab)==0) return(NA); max(tab)/sum(tab)*100
  })
) %>% arrange(pct_na)

cat("\n== DIAGNÓSTICO DE CANDIDATAS (reemplazo de ST003D03T) ==\n")
print(diag_fl2)




#################################Concluyendo la base########################################
############################### PASO 4: EXPORTACIÓN FINAL DEL TOP 40 (TODAS LAS FILAS) ###############################

# 4.1 Lista definitiva de las 40 variables (ya cerrada)
top20_cuanti_final <- c(
  "PV1MATH", "PV1READ", "PV1FLIT", "ESCS", "HISEI", "HOMEPOS", "BELONG",
  "FLCONFIN", "ACCESSFP", "CURIOAGR", "GROSAGR", "MATHEFF", "DISCLIM",
  "COGACRCO", "COGACMCO", "EXPOFA", "FCFMLRTY", "FLSCHOOL", "FLFAMILY",
  "ST059Q01TA"
)

top20_cuali_final <- c(
  "ST004D01T", "ST001D01T", "OECD", "ISCEDP", "ST019AQ01T", "ST019BQ01T",
  "ST019CQ01T", "ST022Q01TA", "ST125Q01NA", "ST226Q01JA", "ST230Q01JA",
  "ST255Q01JA", "SKIPPING", "TARDYSD", "STUDYHMW", "ST297Q09JA",
  "FL169Q05JA", "FL161Q01HA", "FL161Q02HA", "FL159Q04HA"
)

top40_final <- c(top20_cuanti_final, top20_cuali_final)

# Verificación de que las 40 existen en la base (para evitar sorpresas antes de exportar)
faltantes <- setdiff(top40_final, names(datos_completos))
if (length(faltantes) > 0) {
  cat("⚠️ OJO: estas variables no se encontraron en datos_completos:\n")
  print(faltantes)
} else {
  cat("✅ Las 40 variables existen correctamente en datos_completos.\n")
}

# 4.2 Base de datos final: ID + las 40 variables, CON TODAS LAS FILAS (sin filtrar NA todavía)
base_top40 <- datos_completos %>%
  select(CNTSTUID, all_of(top40_final))

cat("Dimensiones de la base final:", nrow(base_top40), "filas x", ncol(base_top40), "columnas.\n")

# 4.3 Diccionario de las 40 variables, tomado de tu propio diccionario_unificado
#     (agrego una columna "Grupo" para que quede claro qué es cuanti y qué es cuali)
diccionario_top40 <- diccionario_unificado %>%
  filter(Codigo_Variable %in% top40_final) %>%
  distinct(Codigo_Variable, .keep_all = TRUE) %>%
  mutate(Grupo = if_else(Codigo_Variable %in% top20_cuanti_final, "Cuantitativa", "Cualitativa")) %>%
  select(Codigo_Variable, Grupo, Descripcion_Real, Tabla_Origen) %>%
  arrange(match(Codigo_Variable, top40_final))   # mantiene el orden que definimos arriba

# 4.4 Exportar a un solo Excel con 2 hojas: "Datos" y "Diccionario_Variables"
write_xlsx(
  list(
    "Datos" = base_top40,
    "Diccionario_Variables" = diccionario_top40
  ),
  path = "base_TOP40_PISA_alfabetizacion_financiera.xlsx"
)

cat("\n✅ Archivo exportado: base_TOP40_PISA_alfabetizacion_financiera.xlsx\n")
cat("   - Hoja 'Datos':", nrow(base_top40), "filas x", ncol(base_top40), "columnas\n")
cat("   - Hoja 'Diccionario_Variables':", nrow(diccionario_top40), "variables documentadas\n")