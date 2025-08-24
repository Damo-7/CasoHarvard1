# Quality Alloys, Inc. – Web Analytics Case Study

Este proyecto reproduce y extiende el análisis del caso **Web Analytics at Quality Alloys, Inc.**, desarrollado por Rob Weitz y David Rosenthal.  
El objetivo es evaluar el impacto del sitio web corporativo y de una campaña promocional en los resultados comerciales de QA, integrando métricas digitales y financieras.

---

## Estructura del proyecto

CasoHarvard1/
│
├── Stores/ # Carpeta con datos en Excel
│ ├── Weekly Visits.xls
│ ├── Financials.xls
│ ├── Lbs. Sold.xls
│ ├── Daily Visits.xls
│ └── Demographics.xls
│
├── analysis.R # Script principal en R con todo el análisis
├── README.md # Este archivo
└── resultados/ # (opcional) salida de tablas, gráficos y reportes

yaml
Copiar
Editar

---

## Librerías requeridas

El análisis se realiza en **R (≥4.2)** con los siguientes paquetes:

```r
library(dplyr)
library(readxl)
library(tidyverse)   # incluye stringr, tidyr, etc.
library(ggplot2)
library(moments)     # skewness y kurtosis
library(lubridate)
Instalación (solo la primera vez):

r
Copiar
Editar
install.packages(c("dplyr","readxl","tidyverse","ggplot2","moments","lubridate"))
▶ Pasos para reproducir el análisis
Clonar o descargar este repositorio y ubicar los archivos de datos (.xls) en la carpeta Stores/.

Abrir R y establecer el directorio de trabajo:

r
Copiar
Editar
setwd("C:/Users/usuario/Desktop/Analitica/Github/CasoHarvard1/Stores")
Ejecutar el script principal:

r
Copiar
Editar
source("analysis.R")
Esto realiza automáticamente:

Lectura y limpieza de datos (manejo de semanas, joins).

Clasificación en los 4 periodos:
Initial, Pre-Promotion, Promotion, Post-Promotion.

Generación de gráficos de series de tiempo y barras.

Tablas descriptivas (media, mediana, SD, min, max).

Análisis de correlaciones (Revenue vs Lbs. Sold, Visits, Unique Visits).

Modelado básico de normalidad (Lbs. Sold, Daily Visits).

Exploración de variables demográficas.

Texto resumen ejecutivo en consola.

 Principales outputs generados
Gráficos de columnas (Weekly):

Unique Visits

Revenue

Profit

Lbs. Sold

Tablas descriptivas (25 valores por periodo × 4 periodos).

Gráficos comparativos por periodo: medias de Visits, Unique Visits, Revenue, Profit y Lbs. Sold.

Correlaciones:

Revenue ~ Lbs. Sold

Revenue ~ Visits

Revenue ~ Unique Visits

Histograma + análisis de normalidad:

Lbs. Sold (2005–2010)

Daily Visits

Demographics: histogramas (si numéricas) o gráficos de barras (si categóricas).

Resumen ejecutivo en consola, con hallazgos y recomendaciones estratégicas.

 Ejemplo de resultados (basado en caso QA)
Revenue y Lbs. Sold tienen correlación fuerte (≈0.87).

Revenue y Visits tienen correlación casi nula (≈-0.06).

La campaña de promoción generó un salto temporal en tráfico y ventas, pero los efectos financieros fueron de corto plazo.

Lbs. Sold presenta sesgo a la derecha (Skewness ≈0.63) y Kurtosis ≈3.53, lo que indica que la distribución es cercana a normal pero con colas algo pesadas.

Reproducibilidad y próximos pasos
Todo el análisis es reproducible ejecutando analysis.R.

Se sugiere extender con:

Modelos de regresión multivariada (Revenue ~ Visits + Unique + Lbs. Sold + dummy promoción).

Segmentación RFM de clientes.

Análisis de atribución digital por canal.

✍️ Autores:
Trabajo académico basado en el caso Quality Alloys, Inc. – Universidad Javeriana.
Nombres:
karol juliana riaño ortiz (juli5677)
Daniel Parra Mora(Damo-7)
ALejandra Herrrera Jimenez(Mariaaherreraj)

