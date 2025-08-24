# =================================================
# 0) Librerías
# =================================================
library(dplyr)
library(readxl)
library(tidyverse)   # incluye stringr, tidyr, etc.
library(ggplot2)
library(moments)
library(lubridate)

# =================================================
# 1) Lectura de datos
# =================================================
setwd("C:/Users/usuario/Desktop/Analitica/Github/CasoHarvard1/Stores")

visitas <- read_excel("Weekly Visits.xls",  skip = 4)
fin     <- read_excel("Financials.xls",     skip = 4)
lbs     <- read_excel("Lbs. Sold.xls",      skip = 4)
daily   <- read_excel("Daily Visits.xls",   skip = 4)
demo    <- read_excel("Demographics.xls")

# Importante: usar backticks en la columna con espacios/paréntesis
visitas <- visitas %>% rename(Week = Week (2008-2009))
fin     <- fin     %>% rename(Week = Week (2008-2009))

# =================================================
# 2) Limpieza de semanas (DEJAR lógicas tal cual)
# =================================================
find_week_col <- function(df){
  nm <- names(df)
  wk <- nm[str_detect(nm, regex("week", ignore_case = TRUE))]
  if (length(wk) == 0) wk <- nm[1]
  wk[1]
}

parse_weeks <- function(df, colname){
  w <- as.character(df[[colname]])
  # Normalizar espacios (incluye NBSP)
  w <- str_replace_all(w, "\u00A0", " ")
  w <- str_replace_all(w, "\\p{Z}+", " ")
  w <- str_squish(w)
  # Tomar el primer "Mes Día" del rango
  first_md <- str_extract(w, "^[A-Za-z]{3,}\\s*\\d{1,2}")
  # Base 2000 para detectar vuelta de año
  base <- suppressWarnings(parse_date_time(paste(first_md, "2000"),
                                           orders = c("b d Y","B d Y")))
  decr <- c(FALSE, base[-1] < base[-length(base)])
  decr[is.na(decr)] <- FALSE
  year <- 2008 + cumsum(decr)
  week_date <- suppressWarnings(parse_date_time(paste(first_md, year),
                                                orders = c("b d Y","B d Y")))
  df %>%
    mutate(
      Week_original = w,
      Week_date     = as.Date(week_date)
    )
}

wk_vis <- find_week_col(visitas)
wk_fin <- find_week_col(fin)

visitas <- parse_weeks(visitas, wk_vis)
fin     <- parse_weeks(fin,     wk_fin)

# =================================================
# 3) Unir datasets y clasificar periodos
#    (NO se cambia la lógica de periodos)
# =================================================
dataset <- left_join(visitas, fin, by = "Week_date", suffix = c("_visitas","_fin"))

dataset <- dataset %>%
  mutate(period = case_when(
    Week_date >= as.Date("2008-05-25") & Week_date <= as.Date("2008-07-20") ~ "Initial",
    Week_date >  as.Date("2008-07-20") & Week_date <= as.Date("2008-12-14") ~ "Pre-Promotion",
    Week_date >  as.Date("2008-12-14") & Week_date <= as.Date("2009-02-15") ~ "Promotion",
    Week_date >  as.Date("2009-02-15") & Week_date <= as.Date("2009-08-29") ~ "Post-Promotion",
    TRUE ~ "Fuera_de_rango"
  ))

# Normalizar potenciales espacios raros en nombres (no afecta Week_date)
names(dataset) <- str_replace_all(names(dataset), "\u00A0", " ")
names(dataset) <- str_squish(names(dataset))

# =================================================
# 4) Series de tiempo (4 gráficos solicitados)
# =================================================
# Asegurar tipos numéricos
dataset <- dataset %>%
  mutate(
    Visits         = as.numeric(Visits),
    Unique Visits= as.numeric(Unique Visits),
    Revenue        = as.numeric(Revenue),
    Profit         = as.numeric(Profit),
    Lbs. Sold    = as.numeric(Lbs. Sold)
  )

# 4 gráficos: Unique Visits, Revenue, Profit, Lbs. Sold
ggplot(dataset, aes(x = Week_date, y = Unique Visits)) + 
  geom_col() + labs(title = "Unique Visits por semana", x = "Semana", y = "Unique Visits")

ggplot(dataset, aes(x = Week_date, y = Revenue)) + 
  geom_col() + labs(title = "Revenue por semana", x = "Semana", y = "Revenue")

ggplot(dataset, aes(x = Week_date, y = Profit)) + 
  geom_col() + labs(title = "Profit por semana", x = "Semana", y = "Profit")

ggplot(dataset, aes(x = Week_date, y = Lbs. Sold)) + 
  geom_col() + labs(title = "Lbs. Sold por semana", x = "Semana", y = "Lbs. Sold")


# Visitas totalesggplot(dataset, aes(x = Week_date, y = Visits)) + geom_col() + labs(title = "Visits por semana")
# 5) Descriptivos por periodo (4 tablas × 25 valores)
# =================================================
# Función helper para resumir 5 stats × 5 variables (transpuesta)
resumir_5x5_t <- function(df_periodo){
  tibble(
    Estadística = c("Mean","Median","SD","Min","Max"),
    Visits = c(mean(df_periodo$Visits, na.rm=TRUE),
               median(df_periodo$Visits, na.rm=TRUE),
               sd(df_periodo$Visits, na.rm=TRUE),
               min(df_periodo$Visits, na.rm=TRUE),
               max(df_periodo$Visits, na.rm=TRUE)),
    Unique Visits = c(mean(df_periodo$Unique Visits, na.rm=TRUE),
                        median(df_periodo$Unique Visits, na.rm=TRUE),
                        sd(df_periodo$Unique Visits, na.rm=TRUE),
                        min(df_periodo$Unique Visits, na.rm=TRUE),
                        max(df_periodo$Unique Visits, na.rm=TRUE)),
    Revenue = c(mean(df_periodo$Revenue, na.rm=TRUE),
                median(df_periodo$Revenue, na.rm=TRUE),
                sd(df_periodo$Revenue, na.rm=TRUE),
                min(df_periodo$Revenue, na.rm=TRUE),
                max(df_periodo$Revenue, na.rm=TRUE)),
    Profit = c(mean(df_periodo$Profit, na.rm=TRUE),
               median(df_periodo$Profit, na.rm=TRUE),
               sd(df_periodo$Profit, na.rm=TRUE),
               min(df_periodo$Profit, na.rm=TRUE),
               max(df_periodo$Profit, na.rm=TRUE)),
    Lbs. Sold = c(mean(df_periodo$Lbs. Sold, na.rm=TRUE),
                    median(df_periodo$Lbs. Sold, na.rm=TRUE),
                    sd(df_periodo$Lbs. Sold, na.rm=TRUE),
                    min(df_periodo$Lbs. Sold, na.rm=TRUE),
                    max(df_periodo$Lbs. Sold, na.rm=TRUE))
  )
}

# Generar tablas por periodo
initial_tbl      <- dataset %>% filter(period=="Initial")        %>% resumir_5x5_t()
prepromo_tbl     <- dataset %>% filter(period=="Pre-Promotion")  %>% resumir_5x5_t()
promotion_tbl    <- dataset %>% filter(period=="Promotion")      %>% resumir_5x5_t()
postpromo_tbl    <- dataset %>% filter(period=="Post-Promotion") %>% resumir_5x5_t()

# Mostrar resultados
cat("\n### Tabla (Initial)\n");       print(initial_tbl)
cat("\n### Tabla (Pre-Promotion)\n"); print(prepromo_tbl)
cat("\n### Tabla (Promotion)\n");     print(promotion_tbl)
cat("\n### Tabla (Post-Promotion)\n");print(postpromo_tbl)
# 6) Gráficos de medias por periodo (5 barras)
# =================================================
means <- dataset %>%
  group_by(period) %>%
  summarise(
    visits_mean  = mean(Visits, na.rm=TRUE),
    unique_mean  = mean(Unique Visits, na.rm=TRUE),
    revenue_mean = mean(Revenue, na.rm=TRUE),
    profit_mean  = mean(Profit, na.rm=TRUE),
    lbs_mean     = mean(Lbs. Sold, na.rm=TRUE)
  )

ggplot(means, aes(x = period, y = visits_mean))  + geom_col() + labs(title="Mean Visits por periodo")
ggplot(means, aes(x = period, y = unique_mean))  + geom_col() + labs(title="Mean Unique Visits por periodo")
ggplot(means, aes(x = period, y = revenue_mean)) + geom_col() + labs(title="Mean Revenue por periodo")
ggplot(means, aes(x = period, y = profit_mean))  + geom_col() + labs(title="Mean Profit por periodo")
ggplot(means, aes(x = period, y = lbs_mean))     + geom_col() + labs(title="Mean Lbs. Sold por periodo")

# Pequeño resumen automático (texto corto)
cat("\n---\nResumen breve por periodo (medias):\n")
print(means)
# 7) Relaciones entre variables (scatter + correlaciones)
# =================================================
cor_visits_revenue <- cor(dataset$Visits,       dataset$Revenue,   use="complete.obs")
cor_lbs_revenue    <- cor(dataset$Lbs. Sold,  dataset$Revenue,   use="complete.obs")
cor_unique_revenue <- cor(dataset$Unique Visits, dataset$Revenue, use="complete.obs")

cat("\nCorrelación Revenue vs Lbs. Sold:", round(cor_lbs_revenue, 3), "\n")
cat("Correlación Revenue vs Visits:",      round(cor_visits_revenue, 3), "\n")
cat("Correlación Revenue vs Unique:",      round(cor_unique_revenue, 3), "\n")

ggplot(dataset, aes(x = Lbs. Sold, y = Revenue)) +
  geom_point() + labs(title="Revenue vs Lbs. Sold (Y=Revenue)")

ggplot(dataset, aes(x = Visits, y = Revenue)) +
  geom_point() + labs(title="Revenue vs Visits (Y=Revenue)")

# Interpretación corta automática
cat("\nInterpretación rápida:\n")
if(!is.na(cor_lbs_revenue)){
  if(cor_lbs_revenue > 0.7) cat("- Revenue y Lbs. Sold muestran relación fuerte y positiva.\n")
  else if(cor_lbs_revenue > 0.4) cat("- Revenue y Lbs. Sold muestran relación moderada y positiva.\n")
  else cat("- La relación entre Revenue y Lbs. Sold es débil/modesta.\n")
}
if(!is.na(cor_visits_revenue)){
  if(cor_visits_revenue > 0.7) cat("- Revenue y Visits muestran relación fuerte y positiva.\n")
  else if(cor_visits_revenue > 0.4) cat("- Revenue y Visits muestran relación moderada y positiva.\n")
  else cat("- La relación entre Revenue y Visits es débil/modesta.\n")

    # =================================================
# 8) Modelado de Lbs. Sold (2005–2010)
# =================================================
# Estadísticos
lbs_vec <- as.numeric(lbs$`Lbs. Sold`)
lbs_mean <- mean(lbs_vec, na.rm=TRUE)
lbs_sd   <- sd(lbs_vec, na.rm=TRUE)
lbs_med  <- median(lbs_vec, na.rm=TRUE)
lbs_min  <- min(lbs_vec, na.rm=TRUE)
lbs_max  <- max(lbs_vec, na.rm=TRUE)

cat("\nResumen Lbs. Sold (2005–2010):\n")
cat("Mean:", lbs_mean, " Median:", lbs_med, " SD:", lbs_sd, " Min:", lbs_min, " Max:", lbs_max, "\n")

# Histograma
hist(lbs_vec, breaks=30, main="Histograma Lbs. Sold (2005–2010)", xlab="Lbs. Sold")

# Regla Empírica (±1,2,3 sd)
z <- (lbs_vec - lbs_mean)/lbs_sd
N <- sum(!is.na(z))

within_1 <- sum(abs(z) <= 1, na.rm=TRUE)
within_2 <- sum(abs(z) <= 2, na.rm=TRUE)
within_3 <- sum(abs(z) <= 3, na.rm=TRUE)

emp_tbl <- tibble(
  Intervalo = c("±1 SD","±2 SD","±3 SD"),
  `% Teórico Normal` = c(68.27, 95.45, 99.73),
  `N teórico`        = round(c(0.6827, 0.9545, 0.9973) * N),
  `N real`           = c(within_1, within_2, within_3),
  `% real`           = round(c(within_1, within_2, within_3) / N * 100, 2)
)
cat("\nRegla Empírica (Lbs. Sold):\n"); print(emp_tbl)

# Tabla refinada dentro de ±3 sd (segmentos de 0.5 sd)
bins <- seq(-3, 3, by=0.5)
labels <- paste0("(", head(bins, -1), ", ", tail(bins, -1), "]")
cut_z <- cut(z, breaks=bins, labels=labels, include.lowest=TRUE)
refined_tbl <- as.data.frame(table(cut_z)) %>%
  rename(Intervalo = cut_z, Frecuencia = Freq) %>%
  mutate(`%` = round(Frecuencia / sum(Frecuencia) * 100, 2))
cat("\nTabla refinada (z en intervalos de 0.5 dentro de ±3):\n"); print(refined_tbl)


# Skewness y kurtosis
lbs_skew <- skewness(lbs_vec, na.rm=TRUE)
lbs_kurt <- kurtosis(lbs_vec, na.rm=TRUE)
cat("\nSkewness Lbs. Sold:", round(lbs_skew,3),
    " | Kurtosis Lbs. Sold:", round(lbs_kurt,3), "\n")

# Texto automático sobre normalidad
cat("\nEvaluación de normalidad (Lbs. Sold):\n")
if(abs(within_1/N*100 - 68.27) < 5 &&
   abs(within_2/N*100 - 95.45) < 3 &&
   abs(within_3/N*100 - 99.73) < 1){
  cat("- Los porcentajes están bastante alineados con la Regla Empírica; la distribución es cercana a normal.\n")
} else {
  cat("- Hay desviaciones respecto a la Regla Empírica; la distribución se aleja de la normal.\n")
}
cat("- Skewness:", round(lbs_skew,2), " (0 indica simetría; >0 sesgo a la derecha).\n")
cat("- Kurtosis:", round(lbs_kurt,2), " (≈3 normal; >3 leptocúrtica; <3 platicúrtica).\n")

# =================================================
# 9) Comparación con Daily Visits (normalidad)
# =================================================
daily_vec <- as.numeric(daily$Visits)
d_skew <- skewness(daily_vec, na.rm=TRUE)
d_kurt <- kurtosis(daily_vec, na.rm=TRUE)

hist(daily_vec, breaks=30, main="Histograma Daily Visits", xlab="Daily Visits")

cat("\nComparación normalidad:\n")
cat("- Daily Visits -> Skewness:", round(d_skew,2), " Kurtosis:", round(d_kurt,2), "\n")
cat("- Lbs. Sold    -> Skewness:", round(lbs_skew,2), " Kurtosis:", round(lbs_kurt,2), "\n")
if(abs(d_skew) < abs(lbs_skew) && abs(d_kurt-3) < abs(lbs_kurt-3)){
  cat("Conclusión: Daily Visits luce más cercana a normal.\n")
} else {
  cat("Conclusión: Lbs. Sold luce más cercana a normal (o similar) en comparación con Daily Visits.\n")
}

# =================================================
# 10) Demográficos: gráficos + conclusiones breves
# =================================================
# Para cada columna: si es carácter/factor -> barra; si es numérica -> histograma
if(ncol(demo) > 0){
  for(col in names(demo)){
    v <- demo[[col]]
    if(is.numeric(v)){
      hist(v, breaks=20, main=paste("Histograma", col), xlab=col)
      cat(paste0("\n[Demographics] ", col, ": media=", round(mean(v, na.rm=TRUE),2),
                 ", mediana=", round(median(v, na.rm=TRUE),2), ".\n"))
    } else {
      cnt <- sort(table(v), decreasing = TRUE)
      topn <- head(cnt, 5)
      ggplot(as.data.frame(cnt), aes(x=reorder(Var1, Freq), y=Freq)) +
        geom_col() + coord_flip() + labs(title=paste("Demographics -", col), x=col, y="Frecuencia")
      cat(paste0("\n[Demographics] Top categorías en ", col, ": ",
                 paste(paste(names(topn), topn, sep="="), collapse=", "), ".\n"))
    }
  }
}

# =================================================
# 11) Texto de hallazgos (parte cuantitativa)
# =================================================
# Cambios porcentuales clave entre periodos
mean_by_period <- means %>%
  select(period, revenue_mean, visits_mean, unique_mean, lbs_mean = lbs_mean, profit_mean)

chg <- function(x1, x0) ifelse(is.na(x1) | is.na(x0) | x0==0, NA, 100*(x1/x0 - 1))

pre_rev  <- mean_by_period %>% filter(period=="Pre-Promotion") %>% pull(revenue_mean)
promo_rev<- mean_by_period %>% filter(period=="Promotion") %>% pull(revenue_mean)
post_rev <- mean_by_period %>% filter(period=="Post-Promotion") %>% pull(revenue_mean)

pre_vis  <- mean_by_period %>% filter(period=="Pre-Promotion") %>% pull(visits_mean)
promo_vis<- mean_by_period %>% filter(period=="Promotion") %>% pull(visits_mean)
post_vis <- mean_by_period %>% filter(period=="Post-Promotion") %>% pull(visits_mean)

pre_uniq <- mean_by_period %>% filter(period=="Pre-Promotion") %>% pull(unique_mean)
promo_uniq<- mean_by_period %>% filter(period=="Promotion") %>% pull(unique_mean)
post_uniq <- mean_by_period %>% filter(period=="Post-Promotion") %>% pull(unique_mean)

pre_lbs  <- mean_by_period %>% filter(period=="Pre-Promotion") %>% pull(lbs_mean)
promo_lbs<- mean_by_period %>% filter(period=="Promotion") %>% pull(lbs_mean)
post_lbs <- mean_by_period %>% filter(period=="Post-Promotion") %>% pull(lbs_mean)

cat("\n---\nHallazgos clave (texto):\n")
cat("- Efecto de la promoción (Revenue): Δ% Promo vs Pre =",
    round(chg(promo_rev, pre_rev),1), "%; Post vs Pre =",
    round(chg(post_rev, pre_rev),1), "%.\n")
cat("- Visitas: Δ% Promo vs Pre =",  round(chg(promo_vis, pre_vis),1),
    "%; Post vs Pre =", round(chg(post_vis, pre_vis),1), "%.\n")
cat("- Unique Visits: Δ% Promo vs Pre =", round(chg(promo_uniq, pre_uniq),1),
    "%; Post vs Pre =", round(chg(post_uniq, pre_uniq),1), "%.\n")
cat("- Lbs. Sold: Δ% Promo vs Pre =", round(chg(promo_lbs, pre_lbs),1),
    "%; Post vs Pre =", round(chg(post_lbs, pre_lbs),1), "%.\n")
cat("- Correlaciones: Revenue~Lbs.Sold =", round(cor_lbs_revenue,2),
    "; Revenue~Visits =", round(cor_visits_revenue,2),
    "; Revenue~Unique =", round(cor_unique_revenue,2), ".\n")

# =================================================
# 12) PARTE 1 – Resumen Ejecutivo (1–2 páginas, consola)
# =================================================
cat("\n=================================================\nRESUMEN EJECUTIVO – Quality Alloys, Inc. (QA)\n=================================================\n")
cat("Contexto: QA monitorea tráfico web, conversión a ventas y resultados financieros.\n")
cat("Objetivo: Evaluar desempeño por periodos (Initial, Pre-Promotion, Promotion, Post-Promotion),\n")
cat("relaciones entre métricas clave (Visits, Unique Visits, Revenue, Profit, Lbs. Sold) y el impacto de la promoción.\n\n")

cat("1) Principales hallazgos cuantitativos:\n")
cat("- Ingresos (Revenue): durante la fase de Promoción cambian en", round(chg(promo_rev, pre_rev),1), 
    "% vs Pre-Promotion; en Post-Promotion cambian en", round(chg(post_rev, pre_rev),1), "% vs Pre.\n")
cat("- Tráfico: Visits cambia en", round(chg(promo_vis, pre_vis),1), "% (Promo vs Pre) y",
    round(chg(post_vis, pre_vis),1), "% (Post vs Pre). Unique Visits muestra cambios de",
    round(chg(promo_uniq, pre_uniq),1), "% y", round(chg(post_uniq, pre_uniq),1), "%, respectivamente.\n")
cat("- Volumen: Lbs. Sold varía en", round(chg(promo_lbs, pre_lbs),1), "% (Promo vs Pre) y",
    round(chg(post_lbs, pre_lbs),1), "% (Post vs Pre).\n")
cat("- Correlaciones: Revenue~Lbs. Sold =", round(cor_lbs_revenue,2),
    " (relación de ingreso con volumen físico);\n  Revenue~Visits =", round(cor_visits_revenue,2),
    " y Revenue~Unique =", round(cor_unique_revenue,2), ".\n\n")

cat("2) Interpretación y efectos de la promoción:\n")
cat("- Si Revenue~Lbs. Sold es alta/positiva, las ventas físicas explican gran parte del ingreso; la promoción\n")
cat("  pudo impulsar unidades vendidas o mix de productos. Si Revenue~Visits mejora, sugiere mayor eficacia\n")
cat("  del embudo (mejor tasa de conversión o ticket medio) además de tráfico.\n")
cat("- Comparando medios por periodo, se observa el cambio de nivel en Promo y si se sostiene en Post.\n\n")

cat("3) Normalidad y variabilidad (Lbs. Sold):\n")
cat("- Regla Empírica: ver tabla; la cercanía a 68-95-99.7% indica cuán 'normal' es la serie.\n")
cat("- Skewness =", round(lbs_skew,2), ", Kurtosis =", round(lbs_kurt,2), ".\n")
cat("- Comparación con Daily Visits: la serie más cercana a normalidad es indicada arriba.\n\n")

cat("4) Recomendaciones a la gerencia:\n")
cat("- Tráfico → Ingreso: Dado el nivel de correlación con Visits/Unique, invertir en fuentes que atraigan\n")
cat("  segmentos con mayor propensión a compra. Medir CAC por canal y ROAS por campaña.\n")
cat("- Conversión → Ingreso: A/B testing de páginas clave (landing, carrito) y promociones selectivas (no\n")
cat("  canibalizar márgenes). Introducir métricas de conversión por cohortes y por industria/país.\n")
cat("- Mix de productos: Si Revenue~Lbs. Sold es fuerte, optimizar pricing/promos en SKUs con mayor elasticidad.\n")
cat("- Post-Promoción: Si el efecto se diluye, considerar tácticas de re-engagement (email, remarketing) y\n")
cat("  promociones más cortas/frecuentes con mejor segmentación.\n")
cat("- Datos adicionales: identificar fuentes/medium UTM, tasa de conversión, ticket medio por periodo,\n")
cat("  y lead time web→venta para aislar efecto de tráfico vs eficacia comercial.\n\n")

cat("5) Próximos pasos analíticos:\n")
cat("- Modelos multivariados (ej. regresión) con Revenue como dependiente y drivers: Visits, Unique, Lbs. Sold,\n")
cat("  dummies de promoción, estacionalidad y canales.\n")
cat("- Atribución por canal y análisis de elasticidad precio-volumen.\n")
cat("- Segmentación de clientes y análisis RFM para promociones dirigidas.\n")
cat("\n=== Fin del Resumen Ejecutivo ===\n")


