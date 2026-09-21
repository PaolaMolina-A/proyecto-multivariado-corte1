
# ==============================================================================
options(digits = 4, scipen = 999)

paquetes <- c(
  "FactoClass", "plotly", "knitr", "DT"
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

# ==============================================================================
# VARIABLE CONTINUA CONTRA EL RESTO (Ancla: PV1FLIT)
# ==============================================================================

# 1. Correlaciones con las demás cuantitativas
base_top20 <- read_excel("base_TOP20_PISA_alfabetizacion_financiera.xlsx")
cuanti_vars <- base_top20[, c("PV1FLIT", "PV1MATH", "PV1READ", "ESCS", "HOMEPOS")]
kable(cor(cuanti_vars, use = "complete.obs"), digits = 2)

# Varianza
kable(var(cuanti_vars, na.rm = TRUE), digits = 2)

# 2. Continua vs Cualitativa (Ej: PV1FLIT vs Género ST004D01T)
# Diagramas de caja
par(bty="n")
sorted_genero <- with(base_top20, reorder(ST004D01T, PV1FLIT, median, na.rm = TRUE))
boxplot(base_top20$PV1FLIT ~ sorted_genero, las = 1, col = 4, horizontal = TRUE)

# Gráfico de violín interactivo (Razón de correlación)
fig <- base_top20 %>%
  plot_ly(x = ~PV1FLIT, split = ~ST004D01T, type = 'violin',
          box = list(visible = TRUE),
          meanline = list(visible = TRUE, color = "black")) %>%
  layout(xaxis = list(title = "Alfabetización Financiera (PV1FLIT)"), 
         yaxis = list(title = "Género"))
fig

# Valores Test para variables continuas (Caracterización)
cluster.carac(cuanti_vars, base_top20$ST004D01T, tipo.v = "co", v.lim = 2, dn = 2, neg = TRUE)#*

# ==============================================================================
# B. VARIABLE CUALITATIVA CONTRA EL RESTO (Ancla: ST004D01T)
# ==============================================================================

# 1. Cualitativa vs Cualitativa (Ej: Género vs Acceso a cuenta FL161Q01HA)
tc <- table(base_top20$ST004D01T, base_top20$FL161Q01HA)
tabtc <- cbind(tc, totF = rowSums(tc))
tabtc <- rbind(tabtc, totC = colSums(tabtc))
kable(tabtc, digits = 1) # Tabla de contingencia

# Perfiles fila
PF <- prop.table(tc, 1)
kable(addmargins(PF, margin = 2) * 100, digits = 1)

# ------------------------------------------------------------------------------
# CONVERSIÓN OBLIGATORIA PARA FACTOCLASS
# Transformamos el subset y la variable ancla a factores puros y data.frame base
cuali_vars_subset <- base_top20[, c("FL161Q01HA", "FL169Q05JA", "FL171Q11JA")]
cuali_vars_subset_factor <- as.data.frame(lapply(cuali_vars_subset, as.factor))
genero_factor <- as.factor(base_top20$ST004D01T)
# ------------------------------------------------------------------------------

# 2. Valores indicativos de asociación (Chi-cuadrado)
kable(chisq.carac(cuali_vars_subset_factor, genero_factor), digits = 3)

# 3. Valores Test para caracterización por variables cualitativas
cluster.carac(cuali_vars_subset_factor, genero_factor)

