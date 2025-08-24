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
