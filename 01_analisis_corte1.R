# ==============================================================================
# PROYECTO PISA 2022 - CORTE 1: ANÁLISIS UNIVARIADO Y BIVARIADO
# ==============================================================================

options(digits = 4, scipen = 999)
library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(patchwork)


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

# ==============================================================================
# MUESTRA INICIAL DE LA TABLA
# ==============================================================================

# Número total de individuos
n <- nrow(datos)

# Muestra de los primeros individuos (primeras 8 columnas para que se vea ordenado)
kable(head(datos[, 1:8]), digits = 2)


#Mostrar un resumen  para las variables
summary(datos)




# ==============================================================================
# ------------------------------------------------------------------------------
# 0. RECODIFICACIÓN DE VARIABLES CUALITATIVAS A FACTORES ETIQUETADOS
# ------------------------------------------------------------------------------
datos_pisa <- datos %>%
  mutate(
    # Género
    ST004D01T = factor(ST004D01T, levels = c(1, 2), labels = c("Femenino", "Masculino")),
    
    # Inclusión Financiera
    FL161Q01HA = factor(FL161Q01HA, levels = c(1, 2, 3), labels = c("Sí", "No", "No sabe/No resp.")),
    FL161Q02HA = factor(FL161Q02HA, levels = c(1, 2, 3), labels = c("Sí", "No", "No sabe/No resp.")),
    
    # Autopercepción y Responsabilidad (Escala Likert de 4 niveles)
    FL169Q05JA = factor(FL169Q05JA, levels = c(1, 2, 3, 4), 
                        labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo")),
    FL159Q04HA = factor(FL159Q04HA, levels = c(1, 2, 3, 4), 
                        labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo")),
    
    # Frecuencia de Ahorro (5 niveles)
    FL171Q11JA = factor(FL171Q11JA, levels = c(1, 2, 3, 4, 5), 
                        labels = c("Nunca/Casi nunca", "1 vez al año", "Varias veces/año", "1 vez al mes", "1+ vez/semana")),
    FL171Q12JA = factor(FL171Q12JA, levels = c(1, 2, 3, 4, 5), 
                        labels = c("Nunca/Casi nunca", "1 vez al año", "Varias veces/año", "1 vez al mes", "1+ vez/semana")),
    
    # Libros en el hogar
    ST255Q01JA = factor(ST255Q01JA, levels = c(1, 2, 3, 4, 5, 6), 
                        labels = c("0-10", "11-25", "26-100", "101-200", "201-500", ">500")),
    
    # Nivel ISCEDP
    ISCEDP = factor(ISCEDP, levels = c(244, 254, 344, 354), 
                    labels = c("Sec. Baja Gen. (244)", "Sec. Baja Voc. (254)", 
                               "Sec. Alta Gen. (344)", "Sec. Alta Voc. (354)"))
  )


# ------------------------------------------------------------------------------
# 1. PALETA CORPORATIVA Y DICCIONARIO DE TÍTULOS
# ------------------------------------------------------------------------------
paleta_custom <- c("#023047", "#219EBC", "#8ECAE6", "#FFB703", "#FB8500")

obtener_colores_custom <- function(n) {
  if (n <= 5) {
    return(paleta_custom[1:n])
  } else {
    return(colorRampPalette(paleta_custom)(n))
  }
}

titulos_cualitativos <- c(
  "CNT"        = "País del Estudiante",
  "ST004D01T"  = "Género del Estudiante",
  "ST255Q01JA" = "Cantidad de Libros en el Hogar",
  "FL161Q01HA" = "Posee Cuenta Bancaria",
  "FL161Q02HA" = "Posee Tarjeta de Débito/Pago",
  "FL169Q05JA" = "Autopercepción: Manejo del Dinero",
  "FL159Q04HA" = "Responsabilidad Financiera",
  "FL171Q11JA" = "Frecuencia de Ahorro Formal (Banco)",
  "FL171Q12JA" = "Frecuencia de Ahorro Informal (Casa)",
  "ISCEDP"     = "Nivel Educativo (ISCED 2011)"
)


# ------------------------------------------------------------------------------
# 2. GRÁFICO AISLADO: DISTRIBUCIÓN POR PAÍS (CNT - 20 PAÍSES CON GGPLOT2)
# ------------------------------------------------------------------------------
df_cnt <- datos_pisa %>%
  filter(!is.na(CNT)) %>%
  count(CNT) %>%
  mutate(Porcentaje = (n / sum(n)) * 100)

g_paises <- ggplot(df_cnt, aes(x = reorder(CNT, Porcentaje), y = Porcentaje)) +
  geom_col(fill = "#023047", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje)), 
            hjust = -0.15, size = 3.3, fontface = "bold", color = "#023047") +
  coord_flip(ylim = c(0, max(df_cnt$Porcentaje) + 2)) +
  labs(
    title = "Distribución de Estudiantes por País de Origen (CNT)",
    subtitle = "Muestra PISA 2022 - Alfabetización Financiera (20 Países)",
    x = "Código País",
    y = "Porcentaje Muestral (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#023047"),
    axis.text.y = element_text(face = "bold", color = "black", size = 9.5),
    panel.grid.major.y = element_blank()
  )

print(g_paises)


# ------------------------------------------------------------------------------
# 3. PANELES AGRUPADOS PARA LAS 9 VARIABLES CUALITATIVAS RESTANTES (5 Y 4)
# ------------------------------------------------------------------------------
vars_panel_1 <- c("ST004D01T", "ST255Q01JA", "FL161Q01HA", "FL161Q02HA", "FL169Q05JA")
vars_panel_2 <- c("FL159Q04HA", "FL171Q11JA", "FL171Q12JA", "ISCEDP")

# ------------------------------------------------------------------------------
# PANEL 1: Primeras 5 variables cualitativas (2 filas x 3 columnas)
# ------------------------------------------------------------------------------


vars_panel_1 <- c("ST004D01T", "ST255Q01JA", "FL161Q01HA", "FL161Q02HA", "FL169Q05JA")

# Variables que se mostrarán como diagrama de torta (pocas categorías, sin orden natural relevante)
vars_torta <- c("ST004D01T", "FL161Q01HA", "FL161Q02HA")

par(las = 1, mfrow = c(2, 3), mai = c(0.5, 1.2, 0.5, 0.3))

for (v in vars_panel_1) {
  if (v %in% names(datos_pisa)) {
    tab <- table(datos_pisa[[v]], useNA = "no")
    per <- prop.table(tab) * 100
    
    colores <- obtener_colores_custom(length(tab))
    titulo  <- titulos_cualitativos[v]
    
    if (v %in% vars_torta) {
      # --- Diagrama de torta, sin texto encima (evita superposición) ---
      etiquetas_leyenda <- paste0(names(tab), " (", round(per, 1), "%)")
      
      pie(per,
          labels = NA,
          col = colores,
          border = "white",
          main = titulo,
          cex.main = 0.9,
          radius = 0.75)          # un poco más pequeño para dejar espacio a la leyenda
      
      legend("bottom",
             legend = etiquetas_leyenda,
             fill = colores,
             border = "white",
             bty = "n",
             cex = 0.65,           # letra más chica para que quepan las 3 categorías
             ncol = 1)             # apiladas en vertical, en vez de horiz = TRUE
      
    } else {
      # --- Diagrama de barras (igual que antes) ---
      pl <- barplot(per, horiz = TRUE, 
                    col = colores,
                    border = "white",
                    main = titulo, 
                    cex.main = 0.9,
                    cex.names = 0.8,
                    xlab = "%", xlim = c(0, max(per) + 25))
      
      text(per + 2, pl, round(per, 1), cex = 0.8, pos = 4)
    }
  }
}

par(mfrow = c(1, 1))


# ------------------------------------------------------------------------------
# PANEL 2: Siguientes 4 variables cualitativas (2 filas x 2 columnas)
# ------------------------------------------------------------------------------
par(las = 1, mfrow = c(2, 2), mai = c(0.5, 1.2, 0.5, 0.3))

for (v in vars_panel_2) {
  if (v %in% names(datos_pisa)) {
    tab <- table(datos_pisa[[v]], useNA = "no")
    per <- prop.table(tab) * 100
    
    colores <- obtener_colores_custom(length(tab))
    titulo  <- titulos_cualitativos[v]
    
    pl <- barplot(per, horiz = TRUE, 
                  col = colores,
                  border = "white",
                  main = titulo, 
                  cex.main = 0.9,
                  cex.names = 0.8,
                  xlab = "%", xlim = c(0, max(per) + 25))
    
    text(per + 2, pl, round(per, 1), cex = 0.8, pos = 4)
  }
}

par(mfrow = c(1, 1)) # Restaurar ventana gráfica










##########################################Análisis Bivariado###############################################






