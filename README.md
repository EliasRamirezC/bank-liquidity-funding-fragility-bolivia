# ICVLF Bolivia

**Índice Compuesto de Vulnerabilidad de Liquidez y Fondeo para el sistema bancario boliviano, 2010–2025**

Repositorio reproducible para la construcción, evaluación y comunicación del ICVLF utilizando indicadores públicos de la Autoridad de Supervisión del Sistema Financiero (ASFI), Bolivia.

## Objetivo de investigación

Construir y evaluar una medida compuesta, transparente y reproducible de vulnerabilidad histórica relativa de liquidez y fondeo para el sistema bancario boliviano durante 2010–2025.

El proyecto no pretende reconstruir el LCR o el NSFR regulatorio, estimar probabilidades de crisis ni establecer causalidad. El ICVLF es una medida histórica relativa construida para integrar, descomponer y auditar información pública.

## Arquitectura conceptual

El índice utiliza 25 indicadores distribuidos en cinco dimensiones:

1. **Liquidez inmediata**
2. **Estructura de fondeo**
3. **Composición de activos**
4. **Presión de cartera crediticia**
5. **Solvencia y capacidad de absorción**

La interpretación económica precede a la agregación estadística. Cada indicador se orienta ex ante para que valores mayores representen mayor vulnerabilidad.

## Especificación principal

Dentro de cada dimensión se utilizan pesos iguales. Los agregados dimensionales se reestandarizan y las cinco dimensiones reciben 20% cada una:

\[
ICVLF_t = \frac{1}{5}\sum_{d=1}^{5}S_{d,t}.
\]

La escala 0–100 es retrospectiva respecto de la muestra 2010–2025 y se utiliza únicamente para comunicación.

## Sensibilidades y validación

El repositorio documenta:

- PCA como diagnóstico y sensibilidad, no como regla principal;
- normalización robusta mediana/MAD;
- ponderaciones alternativas;
- leave-one-indicator-out;
- leave-one-dimension-out;
- exclusión de indicadores con signos más dependientes del contexto;
- validación convergente con P1/P2 y otras proxies;
- validación sin solapamiento mecánico;
- ADF, PP y KPSS;
- HAC Newey-West;
- correlaciones rolling;
- Bai–Perron;
- escenarios de estrés Q95;
- sensibilidad de estrés +1.5 desviaciones estándar.

## Datos

La base original utilizada por el pipeline se conserva en:

```text
data/raw/Datos RL SB.xlsm
```

El Bloque 1 genera automáticamente:

```text
data/processed/asfi_indicators_2010_2025.csv
data/processed/icvlf_monthly_2010_2025.csv
data/processed/indicator_dictionary.csv
```

## Estructura del repositorio

```text
.
├── .github/
│   └── workflows/
│       └── publish.yml
├── R/
│   ├── 01_build_icvlf.R
│   ├── 02_generate_outputs.R
│   └── report_helpers.R
├── assets/
│   └── styles.css
├── data/
│   ├── raw/
│   └── processed/
├── output/
│   ├── bloque1/
│   └── bloque2/
├── _quarto.yml
├── index.qmd
├── run_all.R
├── renv.lock
├── CITATION.cff
└── LICENSE
```

## Pipeline reproducible

```text
data/raw/Datos RL SB.xlsm
        ↓
R/01_build_icvlf.R
        ↓
output/bloque1/diagnosticos/OBJETOS_ICVLF_v6_3_BLOQUE1.rds
        ↓
R/02_generate_outputs.R
        ↓
5 tablas + 6 figuras del cuerpo
22 tablas + 2 figuras de anexos
CSV + Excel + logs
        ↓
index.qmd
        ↓
_site/index.html
        ↓
GitHub Pages
```

## Reproducción local

### 1. Restaurar el entorno de R

```r
install.packages("renv")
renv::restore()
```

### 2. Ejecutar el análisis completo

```bash
Rscript run_all.R
```

### 3. Renderizar el informe

```bash
quarto render
```

### 4. Previsualizar

```bash
quarto preview
```

## Salidas principales

El Bloque 2 produce cinco tablas para el cuerpo del estudio:

```text
Tabla_4_1_Hechos_Estilizados.html
Tabla_4_2_ICVLF_Subperiodos.html
Tabla_4_3_Validacion_P1_P2.html
Tabla_4_4_Robustez_Ejecutiva.html
Tabla_4_5_Rupturas_Bai_Perron.html
```

y seis figuras principales:

```text
Figura_4_1_Perfil_Series_Entrada.png
Figura_4_2_Trayectoria_ICVLF_Rupturas.png
Figura_4_3_Subindices_Dimensiones.png
Figura_4_4_Contribuciones_Subperiodo.png
Figura_4_5_Rolling_P1_P2_60m.png
Figura_4_6_Estres_Q95.png
```

Los diagnósticos completos se conservan en `output/bloque2/02_ANEXOS/`.

## Informe en línea

El informe está diseñado para publicarse en:

https://eliasramirezc.github.io/bank-liquidity-funding-fragility-bolivia/

El front-end es deliberadamente una sola página Quarto para mantener una narrativa académica continua y evitar duplicación entre páginas de metodología, resultados, robustez y dashboard.

## Reproducibilidad

El workflow de GitHub Actions:

1. restaura el entorno R con `renv`;
2. ejecuta `Rscript run_all.R`;
3. renderiza Quarto;
4. publica `_site/` en GitHub Pages.

Por tanto, el sitio publicado se deriva del código y los datos del repositorio.

## Interpretación

Un valor mayor del ICVLF representa una posición de mayor vulnerabilidad respecto de la propia experiencia histórica 2010–2025.

Los niveles Baja, Moderada, Alta y Muy alta son cuartiles históricos relativos. No constituyen:

- límites regulatorios;
- probabilidades de crisis;
- ratings;
- señales automáticas de intervención.

## Citación

Consulte `CITATION.cff`.

## Licencia

Código y materiales del repositorio: MIT License, salvo que una fuente de datos indique condiciones distintas.

## Descargo

Este proyecto es un trabajo de investigación reproducible basado en información pública. Las interpretaciones son responsabilidad del autor y no representan una posición institucional de ASFI ni de otra entidad.
