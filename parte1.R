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