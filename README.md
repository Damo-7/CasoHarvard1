# Web Analytics at Quality Alloys, Inc. (QA)

Este repositorio corresponde al análisis del caso **Web Analytics at Quality Alloys, Inc.**, desarrollado para el curso de Business Analytics – Pontificia Universidad Javeriana (2025-1).

Datos y archivos de réplica para el caso **Quality Alloys, Inc. (QA)** por *Karol Juliana Riaño Ortiz*.

---

## Resumen

Quality Alloys, Inc. (QA) es una empresa fabricante de aleaciones metálicas que buscó fortalecer su presencia digital a través de un nuevo sitio web y una campaña promocional.  
El caso propone evaluar el impacto de esta estrategia digital sobre las métricas clave de negocio (visitas al sitio web, ingresos, ganancias y volumen vendido), y extraer recomendaciones gerenciales basadas en un análisis cuantitativo de datos históricos (2005–2010).

El presente trabajo integra:

- Estadísticas descriptivas por periodos (`Initial`, `Pre-Promotion`, `Promotion`, `Post-Promotion`).
- Análisis de correlaciones entre métricas clave (Revenue, Visits, Unique Visits, Lbs. Sold).
- Evaluación de la normalidad de los datos (`Lbs. Sold` vs. `Daily Visits`).
- Visualización de variables demográficas asociadas a clientes.
- Elaboración de un **Resumen Ejecutivo** con recomendaciones estratégicas.

Con base en esto, se ofrece un reporte analítico que permite a la gerencia de QA comprender el efecto real de la campaña promocional y delinear futuras estrategias de marketing y ventas.

---

## Este repositorio contiene las siguientes carpetas:

### 📂 Carpeta `Stores/`
Alberga las bases de datos originales en Excel entregadas en el caso:
- `Weekly Visits.xls`
- `Financials.xls`
- `Lbs. Sold.xls`
- `Daily Visits.xls`
- `Demographics.xls`

---

### 📂 Carpeta `Scripts/`
Contiene el código en R utilizado para procesar, limpiar y analizar la información.  
El análisis se realizó en **R version 4.5.1 (2025-06-13)** bajo Windows.

- `analysis.R` → Script principal que incluye:
  - Lectura y preparación de datos.
  - Clasificación en periodos (`Initial`, `Pre-Promotion`, `Promotion`, `Post-Promotion`).
  - Estadísticos descriptivos y gráficos de series de tiempo.
  - Cálculo de correlaciones entre métricas clave.
  - Modelado de distribución de `Lbs. Sold` y comparación con `Daily Visits`.
  - Exploración de datos demográficos.
  - Generación automática de un **Resumen Ejecutivo** en consola.

---

### 📂 Carpeta `Views/`
Incluye tablas y gráficos generados automáticamente por el script:
- Series de tiempo (Revenue, Profit, Visits, Unique Visits, Lbs. Sold).
- Tablas descriptivas por periodo (25 valores cada una).
- Diagramas de dispersión con correlaciones.
- Histogramas y tablas de la Regla Empírica.
- Gráficos demográficos.

---

### 📂 Carpeta `Document/`
- `Caso_QA.pdf`: Documento original del caso (Harvard Business School).
- `Reporte_QA.pdf`: Informe final con resultados, hallazgos y recomendaciones.

---

## 🔧 Notas de uso

1. Si se ejecutan los scripts desde programas como **RStudio**, se debe asegurar que el directorio base se configure en:

   ```r
   setwd(".../CasoHarvard1/Stores")

