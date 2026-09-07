# ══════════════════════════════════════════════════════════════
# 0.  PAQUETES Y REPRODUCIBILIDAD
# ══════════════════════════════════════════════════════════════

options(stringsAsFactors = FALSE, scipen = 999)

paquetes <- c(
  "readxl", "dplyr", "tidyr", "stringr", "readr",
  "psych", "purrr", "tibble", "strucchange", "zoo",
  "tseries", "moments", "sandwich", "lmtest"
)

# Dependencias: lubridate y ggtext fueron eliminadas porque no son necesarias.
# Para evitar que el script se detenga por paquetes faltantes no esenciales,
# se conservan únicamente las librerías efectivamente utilizadas.
# Cambiar a TRUE si desea que R instale automáticamente cualquier dependencia faltante.
instalar_faltantes <- FALSE
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]

if (length(faltantes) > 0) {
  if (instalar_faltantes) {
    install.packages(faltantes, repos = "https://cloud.r-project.org")
  } else {
    stop(
      "Faltan paquetes requeridos: ", paste(faltantes, collapse = ", "),
      ". Instálelos antes de ejecutar o establezca instalar_faltantes <- TRUE."
    )
  }
}

invisible(lapply(paquetes, library, character.only = TRUE))
cat("✓ Paquetes cargados.\n")


# ══════════════════════════════════════════════════════════════
# 1.  PARÁMETROS GLOBALES
# ══════════════════════════════════════════════════════════════

# ============================================================
# RUTA REPRODUCIBLE DE LA BASE
# ============================================================

# Ruta de datos reproducible:
# 1) variable de entorno opcional;
# 2) base pública dentro del repositorio.
ruta_candidatas <- c(
  Sys.getenv("ICVLF_DATA_PATH", unset = ""),
  file.path(
    getwd(),
    "data",
    "raw",
    "Datos RL SB.xlsm"
  )
)

ruta_candidatas <- unique(
  ruta_candidatas[nzchar(ruta_candidatas)]
)

ruta_existente <- ruta_candidatas[
  file.exists(ruta_candidatas)
]

if (length(ruta_existente) == 0) {
  stop(
    "No se encontró data/raw/Datos RL SB.xlsm. ",
    "Coloque la base pública en esa ruta o defina ICVLF_DATA_PATH."
  )
}

ruta <- ruta_existente[1]

hoja       <- "Hoja1"
skip_filas <- 4

fecha_inicio_muestra <- as.Date("2010-01-01")
fecha_fin_muestra    <- as.Date("2025-12-01")

umbral_cobertura       <- 0.70
umbral_kmo              <- 0.50
umbral_cor_media_pca    <- 0.30
umbral_bartlett         <- 0.05
umbral_var_pc1           <- 0.50
umbral_coherencia_pc1    <- 0.75
n_min_frac_subindice    <- 0.80
min_pares_validacion    <- 5
min_frac_rolling        <- 0.80
min_obs_proxy_rolling_q <- 60
lag_hac                 <- 12
ventana_rolling_q       <- 60
ventana_rolling_s       <- 24
percentil_estres        <- 0.95
shock_sensibilidad_de   <- 1.50
winsor_prob             <- c(0.01, 0.99)

muestra_str   <- "Enero 2010 – Diciembre 2025"
muestra_short <- "2010–2025"

unidad_analisis_texto <- paste0(
  "Indicadores del sistema bancario reportados para la categoría agregada ",
  "'Todos' en la fuente utilizada"
)

fuente_asfi <- paste0(
  "Fuente: Elaboración propia con indicadores publicados por ASFI ",
  "(Autoridad de Supervisión del Sistema Financiero de Bolivia)."
)

fuente_propia <- "Fuente: Elaboración propia."

# ============================================================
# RUTA REPRODUCIBLE DE SALIDA
# ============================================================

carpeta_salida <- file.path(
  getwd(),
  "output",
  "bloque1"
)

for (sub in c("logs", "diagnosticos")) {
  dir.create(
    file.path(carpeta_salida, sub),
    showWarnings = FALSE,
    recursive = TRUE
  )
}

cat(
  "✓ Parámetros definidos | Base:",
  normalizePath(ruta, winslash = "/", mustWork = TRUE),
  "\n"
)

cat("✓ Muestra objetivo:", muestra_str, "\n")
cat("✓ Bloque 1: motor metodológico + auditoría + salidas impresas; sin gráficos/tablas publicables.\n")
cat("✓ Lectura del índice: vulnerabilidad histórica relativa; no LCR/NSFR regulatorio, no probabilidad de crisis, no causalidad.\n")


# ══════════════════════════════════════════════════════════════
# 2.  FUNCIONES AUXILIARES
# ══════════════════════════════════════════════════════════════

normalizar_0100 <- function(x) {
  ok <- is.finite(x) & !is.na(x)

  if (!any(ok)) {
    return(rep(NA_real_, length(x)))
  }

  mn <- min(x[ok])
  mx <- max(x[ok])

  if (isTRUE(all.equal(mx, mn))) {
    return(ifelse(ok, 50, NA_real_))
  }

  out <- rep(NA_real_, length(x))
  out[ok] <- 100 * (x[ok] - mn) / (mx - mn)

  out
}


# Aplica a nuevos valores la misma transformación 0–100 obtenida de una
# referencia histórica. Permite valores <0 o >100 si el escenario excede
# el rango histórico; no se trunca deliberadamente.
escalar_0100_con_referencia <- function(x, ref) {

  ref_ok <- ref[
    is.finite(ref) &
      !is.na(ref)
  ]

  if (length(ref_ok) == 0) {
    return(rep(NA_real_, length(x)))
  }

  mn <- min(ref_ok)
  mx <- max(ref_ok)

  if (isTRUE(all.equal(mx, mn))) {
    return(rep(50, length(x)))
  }

  100 * (x - mn) / (mx - mn)
}


parse_num <- function(x) {

  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x_chr <- stringr::str_squish(
    as.character(x)
  )

  x_chr[
    x_chr %in% c(
      "",
      "NA",
      "N/A",
      "-",
      "--"
    )
  ] <- NA_character_

  parse_one <- function(s) {

    if (is.na(s)) {
      return(NA_real_)
    }

    s <- gsub(
      "\\s+",
      "",
      s
    )

    tiene_coma <- grepl(
      ",",
      s,
      fixed = TRUE
    )

    tiene_punto <- grepl(
      ".",
      s,
      fixed = TRUE
    )

    if (tiene_coma && tiene_punto) {

      pos_coma <- max(
        gregexpr(
          ",",
          s,
          fixed = TRUE
        )[[1]]
      )

      pos_punto <- max(
        gregexpr(
          ".",
          s,
          fixed = TRUE
        )[[1]]
      )

      if (pos_coma > pos_punto) {

        return(
          suppressWarnings(
            readr::parse_number(
              s,
              locale = readr::locale(
                decimal_mark = ",",
                grouping_mark = "."
              )
            )
          )
        )

      } else {

        return(
          suppressWarnings(
            readr::parse_number(
              s,
              locale = readr::locale(
                decimal_mark = ".",
                grouping_mark = ","
              )
            )
          )
        )
      }
    }

    if (tiene_coma) {

      return(
        suppressWarnings(
          readr::parse_number(
            s,
            locale = readr::locale(
              decimal_mark = ",",
              grouping_mark = "."
            )
          )
        )
      )
    }

    suppressWarnings(
      readr::parse_number(
        s,
        locale = readr::locale(
          decimal_mark = ".",
          grouping_mark = ","
        )
      )
    )
  }

  vapply(
    x_chr,
    parse_one,
    numeric(1)
  )
}


mes_a_numero <- function(m) {

  meses <- c(
    Enero = 1,
    Febrero = 2,
    Marzo = 3,
    Abril = 4,
    Mayo = 5,
    Junio = 6,
    Julio = 7,
    Agosto = 8,
    Septiembre = 9,
    Octubre = 10,
    Noviembre = 11,
    Diciembre = 12
  )

  unname(
    meses[
      stringr::str_squish(
        as.character(m)
      )
    ]
  )
}


safe_mean <- function(x) {

  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(
    x,
    na.rm = TRUE
  )
}


safe_cv <- function(x) {

  m <- mean(
    x,
    na.rm = TRUE
  )

  s <- sd(
    x,
    na.rm = TRUE
  )

  if (
    !is.finite(m) ||
      abs(m) < .Machine$double.eps^0.5
  ) {
    return(NA_real_)
  }

  s / abs(m) * 100
}


# Escala robusta de referencia. Se usa solo como sensibilidad a outliers.
# MAD usa el factor 1.4826 para consistencia bajo normalidad. Si MAD=0,
# se utiliza IQR/1.349 y, como último respaldo, la desviación estándar.
robust_scale_ref <- function(x_ref) {

  med <- median(
    x_ref,
    na.rm = TRUE
  )

  sc <- stats::mad(
    x_ref,
    center = med,
    constant = 1.4826,
    na.rm = TRUE
  )

  if (
    !is.finite(sc) ||
      sc <= 0
  ) {

    sc <- IQR(
      x_ref,
      na.rm = TRUE
    ) / 1.349
  }

  if (
    !is.finite(sc) ||
      sc <= 0
  ) {

    sc <- sd(
      x_ref,
      na.rm = TRUE
    )
  }

  if (
    !is.finite(sc) ||
      sc <= 0
  ) {
    sc <- 1
  }

  c(
    center = med,
    scale = sc
  )
}


intensidad_cor <- function(r) {

  dplyr::case_when(
    is.na(r) ~ "S/D",
    abs(r) >= 0.70 ~ "Alta (|r| ≥ 0.70)",
    abs(r) >= 0.40 ~ "Moderada (0.40 ≤ |r| < 0.70)",
    TRUE ~ "Baja (|r| < 0.40)"
  )
}


moda_factor <- function(x) {

  tx <- table(
    x,
    useNA = "no"
  )

  if (length(tx) == 0) {
    return(NA_character_)
  }

  names(tx)[
    which.max(tx)
  ]
}


validar_pesos <- function(
    w,
    cols,
    tol = 1e-10
) {

  if (is.null(names(w))) {
    stop(
      "El vector de pesos debe estar nombrado."
    )
  }

  if (!setequal(
    names(w),
    cols
  )) {
    stop(
      "Los nombres de los pesos no coinciden con las columnas del bloque."
    )
  }

  w <- w[cols]

  if (
    any(!is.finite(w)) ||
      any(w < 0)
  ) {
    stop(
      "Pesos internos inválidos."
    )
  }

  if (
    abs(sum(w) - 1) >
      tol
  ) {
    stop(
      "Los pesos internos no suman 1."
    )
  }

  w
}


suma_ponderada_robusto <- function(
    Xmat,
    w,
    n_min_frac = 0.5
) {

  Xmat <- as.matrix(Xmat)

  if (is.null(colnames(Xmat))) {
    stop(
      "Xmat debe tener nombres de columnas."
    )
  }

  w <- validar_pesos(
    w,
    colnames(Xmat)
  )

  na_mask <-
    !is.na(Xmat) &
    is.finite(Xmat)

  Xsafe <- Xmat
  Xsafe[!na_mask] <- NA_real_

  num <- rowSums(
    sweep(
      Xsafe,
      2,
      w,
      "*"
    ),
    na.rm = TRUE
  )

  den <- rowSums(
    sweep(
      na_mask,
      2,
      w,
      "*"
    ),
    na.rm = TRUE
  )

  res <- num / den

  res[
    rowSums(na_mask) /
      ncol(Xmat) <
      n_min_frac
  ] <- NA_real_

  res[
    den <= 0
  ] <- NA_real_

  res
}


safe_cor <- function(
    x,
    y,
    method = "pearson",
    min_pares = min_pares_validacion
) {

  idx <-
    complete.cases(x, y) &
    is.finite(x) &
    is.finite(y)

  if (
    sum(idx) <
      min_pares
  ) {
    return(NA_real_)
  }

  suppressWarnings(
    tryCatch(
      cor(
        x[idx],
        y[idx],
        method = method
      ),
      error = function(e) {
        NA_real_
      }
    )
  )
}


safe_n <- function(x, y) {

  sum(
    complete.cases(x, y) &
      is.finite(x) &
      is.finite(y)
  )
}


# Regímenes relativos respecto a la distribución histórica completa.
clasificar_cuartiles <- function(x) {

  q <- quantile(
    x,
    c(
      .25,
      .50,
      .75
    ),
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )

  if (
    length(unique(q)) <
      3
  ) {

    warning(
      "Cuartiles con empates; se usa ntile(4) como respaldo descriptivo."
    )

    nt <- dplyr::ntile(
      x,
      4
    )

    return(
      factor(
        c(
          "Baja",
          "Moderada",
          "Alta",
          "Muy alta"
        )[nt],
        levels = c(
          "Baja",
          "Moderada",
          "Alta",
          "Muy alta"
        )
      )
    )
  }

  cut(
    x,
    breaks = c(
      -Inf,
      q,
      Inf
    ),
    include.lowest = TRUE,
    labels = c(
      "Baja",
      "Moderada",
      "Alta",
      "Muy alta"
    ),
    ordered_result = TRUE
  )
}


# Nota de arquitectura: funciones de visualización, estilos y exportación publicable
# se reservan para el BLOQUE 2. El BLOQUE 1 no genera figuras ni HTML.


# ══════════════════════════════════════════════════════════════
# 3.  CARGAR BASE Y AUDITAR ESTRUCTURA TEMPORAL
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 3. Cargando y auditando base de datos ──\n"
)

Base_raw <- readxl::read_excel(
  path = ruta,
  sheet = hoja,
  skip = skip_filas
)

names(Base_raw) <- stringr::str_squish(
  names(Base_raw)
)

if (
  ncol(Base_raw) <
    3
) {
  stop(
    "La hoja no contiene la estructura esperada."
  )
}

Base_raw <- Base_raw %>%
  rename(
    Anio = 1,
    Mes_nombre = 2
  ) %>%
  mutate(
    anio_num = as.integer(
      parse_num(
        as.character(Anio)
      )
    ),
    mes_num = as.integer(
      mes_a_numero(
        Mes_nombre
      )
    ),
    Fecha = as.Date(
      sprintf(
        "%04d-%02d-01",
        anio_num,
        mes_num
      )
    )
  ) %>%
  filter(
    !is.na(Fecha)
  ) %>%
  arrange(
    Fecha
  )

if (
  anyDuplicated(
    Base_raw$Fecha
  ) >
    0
) {

  dup <- unique(
    Base_raw$Fecha[
      duplicated(
        Base_raw$Fecha
      )
    ]
  )

  stop(
    "Existen fechas mensuales duplicadas: ",
    paste(
      dup,
      collapse = ", "
    )
  )
}

fechas_esperadas <- seq(
  fecha_inicio_muestra,
  fecha_fin_muestra,
  by = "month"
)

fechas_observadas <- Base_raw %>%
  filter(
    Fecha >= fecha_inicio_muestra,
    Fecha <= fecha_fin_muestra
  ) %>%
  pull(
    Fecha
  )

fechas_faltantes <- setdiff(
  fechas_esperadas,
  fechas_observadas
)

fechas_extra <- setdiff(
  fechas_observadas,
  fechas_esperadas
)

if (
  length(fechas_faltantes) >
    0
) {

  stop(
    "Faltan meses dentro de la muestra 2010–2025: ",
    paste(
      format(
        fechas_faltantes,
        "%Y-%m"
      ),
      collapse = ", "
    )
  )
}

if (
  length(fechas_extra) >
    0
) {
  warning(
    "Se detectaron fechas fuera de la secuencia mensual esperada."
  )
}

cat(
  "  Base completa:",
  format(
    min(Base_raw$Fecha),
    "%b %Y"
  ),
  "→",
  format(
    max(Base_raw$Fecha),
    "%b %Y"
  ),
  "(",
  nrow(Base_raw),
  "obs.)\n"
)

cat(
  "  Muestra objetivo: ",
  length(fechas_esperadas),
  " meses consecutivos.\n",
  sep = ""
)


# ══════════════════════════════════════════════════════════════
# 4.  DICCIONARIO METODOLÓGICO
# ══════════════════════════════════════════════════════════════

dic <- tibble::tribble(
  ~original, ~var, ~bloque_id, ~bloque, ~direccion_riesgo, ~entra_indice, ~entra_ipfe,

  "Disponibilidades/Oblig.a Corto Plazo",
  "liq_disp_oblig_cp", "liquidez", "Liquidez inmediata", -1, TRUE, FALSE,

  "Disponib.+Inv.Temp./Oblig.a Corto Plazo",
  "liq_disp_inv_oblig_cp", "liquidez", "Liquidez inmediata", -1, TRUE, FALSE,

  "Disponib.+Inv.Temp./Pasivo",
  "liq_disp_inv_pasivo", "liquidez", "Liquidez inmediata", -1, TRUE, FALSE,

  "Disponibilidades+Inv.Temporarias/Activo",
  "liq_disp_inv_activo", "liquidez", "Liquidez inmediata", -1, TRUE, FALSE,

  "Activos Liquidos/pasivos de corto plazo",
  "liq_activos_pasivos_cp", "liquidez", "Liquidez inmediata", -1, FALSE, FALSE,

  "Oblig.con el Público y con Empresas Públicas/Pasivo+Patrimonio",
  "fon_oblig_publico_emp_paspat", "fondeo", "Estructura de fondeo", 1, TRUE, FALSE,

  "Oblig.con el Público/Pasivo+Patrimonio",
  "fon_oblig_publico_paspat", "fondeo", "Estructura de fondeo", 1, TRUE, FALSE,

  "Oblig.con Bancos y Ent. Fin./Pasivo+Patrimonio",
  "fon_oblig_bancos_paspat", "fondeo", "Estructura de fondeo", 1, TRUE, TRUE,

  "Obligaciones Subordinadas/Pasivo+Patrimonio",
  "fon_oblig_subordinadas_paspat", "fondeo", "Estructura de fondeo", -1, TRUE, TRUE,

  "Oblig. Pers. Jurídicas e Institucionales /Total Oblig. Publico",
  "fon_juridicas_institucionales", "fondeo", "Estructura de fondeo", 1, TRUE, TRUE,

  "Oblig. Personas. Naturales /Total Oblig. Publico",
  "fon_personas_naturales", "fondeo", "Estructura de fondeo", -1, TRUE, TRUE,

  "Días de permanencia de los depósitos a plazo fijo",
  "fon_dias_permanencia_dpf", "fondeo", "Estructura de fondeo", -1, TRUE, TRUE,

  "Disponibilidades / Activos",
  "act_disp_activo", "activos", "Estructura de activos", -1, TRUE, FALSE,

  "Cartera Neta / Activo",
  "act_cartera_neta_activo", "activos", "Estructura de activos", 1, TRUE, FALSE,

  "Activo Productivo/Activo+Contingente",
  "act_productivo_actcont", "activos", "Estructura de activos", -1, TRUE, FALSE,

  "Activo Improductivo/Patrimonio",
  "act_improductivo_patrimonio", "activos", "Estructura de activos", 1, TRUE, FALSE,

  "Cartera Reprogramada o Reestructurada/ Cartera",
  "car_reprogramada_cartera", "cartera", "Presión de cartera", 1, TRUE, FALSE,

  "Cartera Vencida Total+Ejecución Total /Cartera",
  "car_mora_cartera", "cartera", "Presión de cartera", 1, TRUE, FALSE,

  "Cartera Reprog. o Reestruct. Vencida y Ejec./ Cartera Reprog. o Reestruct. Total",
  "car_reprog_venc_ejec", "cartera", "Presión de cartera", 1, TRUE, FALSE,

  "Prev.Cartera Incobrable/Cartera",
  "car_prev_incobrable", "cartera", "Presión de cartera", 1, TRUE, FALSE,

  "F Cartera con Requerimiento de Previsión del 100%",
  "car_categoria_f", "cartera", "Presión de cartera", 1, TRUE, FALSE,

  "Patrimonio/Activo",
  "solv_patrimonio_activo", "solvencia", "Solvencia y absorción", -1, TRUE, TRUE,

  "Patrimonio/Activo+Contingente",
  "solv_patrimonio_actcont", "solvencia", "Solvencia y absorción", -1, TRUE, FALSE,

  "Coeficiente de Adecuación Patrimonial",
  "solv_cap", "solvencia", "Solvencia y absorción", -1, TRUE, FALSE,

  "Cartera Vencida Total + Ejecucion Total / Patrimonio",
  "solv_mora_patrimonio", "solvencia", "Solvencia y absorción", 1, TRUE, FALSE,

  "Cartera Vencida Total + Ejecución Total - Prev/Patrimonio",
  "solv_mora_neta_patrimonio", "solvencia", "Solvencia y absorción", 1, TRUE, FALSE,

  "Result.Neto de la Gestión/(Activo+Contingente) (ROA)",
  "rent_roa", "rentabilidad", "Rentabilidad", -1, FALSE, FALSE,

  "Result.Neto de la Gestión/Patrimonio (ROE)",
  "rent_roe", "rentabilidad", "Rentabilidad", -1, FALSE, FALSE,

  "Resultado de operación después de Incobrables /(Activo + Contingente)",
  "rent_resultado_operativo", "rentabilidad", "Rentabilidad", -1, FALSE, FALSE,

  "Gastos Financieros/Pasivos con costo promedio",
  "rent_gastos_fin_pasivos_costo", "rentabilidad", "Rentabilidad", 1, FALSE, FALSE,

  "Gastos de Administración/Activo+Contingente.",
  "rent_gastos_admin_actcont", "rentabilidad", "Rentabilidad", 1, FALSE, FALSE
)


etiquetas_var <- c(
  liq_disp_oblig_cp = "Disponibilidades / Oblig. Corto Plazo",
  liq_disp_inv_oblig_cp = "(Disp. + Inv. Temp.) / Oblig. Corto Plazo",
  liq_disp_inv_pasivo = "(Disp. + Inv. Temp.) / Pasivo Total",
  liq_disp_inv_activo = "(Disp. + Inv. Temp.) / Activo Total",
  liq_activos_pasivos_cp = "Activos Líquidos / Pasivos Corto Plazo",
  fon_oblig_publico_emp_paspat = "Oblig. Público + Emp. Públicas / (Pas. + Pat.)",
  fon_oblig_publico_paspat = "Oblig. con el Público / (Pas. + Pat.)",
  fon_oblig_bancos_paspat = "Oblig. con Bancos y Ent. Fin. / (Pas. + Pat.)",
  fon_oblig_subordinadas_paspat = "Obligaciones Subordinadas / (Pas. + Pat.)",
  fon_juridicas_institucionales = "Oblig. Personas Jurídicas e Institucionales / Total Oblig.",
  fon_personas_naturales = "Oblig. Personas Naturales / Total Oblig. Público",
  fon_dias_permanencia_dpf = "Días de Permanencia Promedio de DPF",
  act_disp_activo = "Disponibilidades / Activo Total",
  act_cartera_neta_activo = "Cartera Neta / Activo Total",
  act_productivo_actcont = "Activo Productivo / (Activo + Contingente)",
  act_improductivo_patrimonio = "Activo Improductivo / Patrimonio",
  car_reprogramada_cartera = "Cartera Reprog. o Reestruct. / Cartera Total",
  car_mora_cartera = "Cartera Vencida y Ejecución / Cartera Total",
  car_reprog_venc_ejec = "Cartera Reprog. Vencida y Ejec. / Cartera Reprog.",
  car_prev_incobrable = "Previsión Cartera Incobrable / Cartera Total",
  car_categoria_f = "Cartera Categoría F (previsión 100%) / Cartera",
  solv_patrimonio_activo = "Patrimonio / Activo Total",
  solv_patrimonio_actcont = "Patrimonio / (Activo + Contingente)",
  solv_cap = "Coeficiente de Adecuación Patrimonial (CAP)",
  solv_mora_patrimonio = "(Cartera Vencida + Ejecución) / Patrimonio",
  solv_mora_neta_patrimonio = "(Cartera Vencida + Ejecución – Prev.) / Patrimonio",
  rent_roa = "Resultado Neto / (Activo + Contingente) — ROA",
  rent_roe = "Resultado Neto / Patrimonio — ROE",
  rent_resultado_operativo = "Result. Operativo (neto de incobrables) / (Activo + Cont.)",
  rent_gastos_fin_pasivos_costo = "Gastos Financieros / Pasivos con Costo Promedio",
  rent_gastos_admin_actcont = "Gastos de Administración / (Activo + Contingente)"
)


etiquetas_bloque <- c(
  liquidez = "Dimensión 1: Liquidez Inmediata",
  fondeo = "Dimensión 2: Estructura de Fondeo",
  activos = "Dimensión 3: Composición de Activos",
  cartera = "Dimensión 4: Presión de Cartera Crediticia",
  solvencia = "Dimensión 5: Solvencia y Capacidad de Absorción"
)


etiquetas_bloque_corto <- c(
  liquidez = "Liquidez",
  fondeo = "Fondeo",
  activos = "Activos",
  cartera = "Cartera",
  solvencia = "Solvencia"
)


# Marco financiero del constructo: separa dimensiones directas de canales de transmisión/absorción.
tabla_marco_dimensiones <- tibble::tribble(
  ~bloque, ~naturaleza, ~mecanismo_financiero, ~limite_interpretativo,

  "liquidez",
  "Dimensión directa",
  "Capacidad inmediata de atender obligaciones mediante disponibilidades y activos líquidos/temporarios.",
  "No equivale al LCR regulatorio ni incorpora por sí sola todos los flujos de 30 días.",

  "fondeo",
  "Dimensión directa/estructural",
  "Estabilidad, concentración, composición y permanencia relativa de las fuentes de financiación.",
  "No equivale al NSFR regulatorio ni reconstruye ASF/RSF por vencimientos contractuales.",

  "activos",
  "Canal de transmisión",
  "Composición y monetización del activo: mayor inmovilización puede limitar la generación rápida de liquidez.",
  "La composición de activos condiciona la liquidez, pero no constituye por sí sola riesgo de liquidez regulatorio.",

  "cartera",
  "Canal de transmisión de flujos",
  "Deterioro, mora y reprogramación pueden reducir o retrasar entradas contractuales de caja y elevar necesidades de financiación.",
  "Es un canal crediticio hacia liquidez; no se interpreta como sustituto del riesgo de crédito ni como causalidad estimada.",

  "solvencia",
  "Canal de absorción/confianza",
  "La capacidad patrimonial de absorción puede condicionar confianza, acceso a fondeo y resiliencia ante pérdidas que tensionen liquidez.",
  "Solvencia y liquidez son riesgos distintos; aquí solvencia actúa como condicionante, no como sustituto de métricas de liquidez."
) %>%
  mutate(
    bloque_nombre = etiquetas_bloque[bloque],
    .before = 1
  )


# Auditoría financiera ex ante de la dirección de riesgo.
# Esta tabla documenta POR QUÉ se asigna d_i = +1 o -1 y distingue
# direcciones financieramente directas de supuestos que requieren sensibilidad.
auditoria_financiera_signos <- tibble::tribble(
  ~variable, ~fortaleza_signo, ~fundamento_financiero, ~sensibilidad_especifica,

  "liq_disp_oblig_cp",
  "Alta",
  "Mayor disponibilidad relativa a obligaciones de corto plazo aumenta la capacidad inmediata de pago; por ello reduce vulnerabilidad.",
  FALSE,

  "liq_disp_inv_oblig_cp",
  "Alta",
  "Mayor cobertura de obligaciones de corto plazo mediante disponibilidades e inversiones temporarias amplía el colchón líquido.",
  FALSE,

  "liq_disp_inv_pasivo",
  "Alta",
  "Una mayor proporción de activos líquidos/temporarios frente al pasivo total mejora la capacidad de respuesta ante salidas de fondos.",
  FALSE,

  "liq_disp_inv_activo",
  "Alta",
  "Una mayor fracción del activo en disponibilidades e inversiones temporarias incrementa la liquidez del balance.",
  FALSE,

  "fon_oblig_publico_emp_paspat",
  "Media",
  "Una mayor dependencia agregada de obligaciones con público y empresas públicas puede aumentar concentración/dependencia de fondeo; su estabilidad depende de composición y vencimiento.",
  TRUE,

  "fon_oblig_publico_paspat",
  "Media",
  "La participación total de depósitos/obligaciones con el público no es adversa por sí misma: su riesgo depende de estabilidad, concentración y plazo. Se conserva como señal estructural y se exige sensibilidad.",
  TRUE,

  "fon_oblig_bancos_paspat",
  "Alta",
  "Una mayor dependencia de fondeo bancario/interfinanciero aproxima el balance a fuentes mayoristas potencialmente más sensibles a condiciones de mercado.",
  FALSE,

  "fon_oblig_subordinadas_paspat",
  "Media-Alta",
  "El fondeo subordinado suele tener horizonte contractual más largo y mayor estabilidad relativa de financiación, aunque no sustituye liquidez inmediata.",
  FALSE,

  "fon_juridicas_institucionales",
  "Alta",
  "Una mayor participación de personas jurídicas/institucionales aproxima la base de depósitos a fondeo corporativo potencialmente menos granular y más sensible a retiros concentrados.",
  FALSE,

  "fon_personas_naturales",
  "Alta",
  "Una mayor participación de personas naturales aproxima la estructura a depósitos minoristas más granulares; se interpreta como mayor estabilidad relativa del fondeo.",
  FALSE,

  "fon_dias_permanencia_dpf",
  "Alta",
  "Una mayor permanencia de depósitos a plazo reduce presión de refinanciación de corto plazo y mejora estabilidad temporal del fondeo.",
  FALSE,

  "act_disp_activo",
  "Alta",
  "Mayor peso de disponibilidades dentro del activo aumenta capacidad de monetización inmediata y reduce vulnerabilidad de liquidez.",
  FALSE,

  "act_cartera_neta_activo",
  "Alta",
  "Una mayor concentración del activo en cartera crediticia incrementa la proporción de activos menos líquidos y dependientes de cobros contractuales.",
  FALSE,

  "act_productivo_actcont",
  "Media",
  "Un mayor activo productivo puede mejorar generación de ingresos, pero productivo no equivale necesariamente a líquido; el signo protector es una hipótesis de estructura del balance y requiere sensibilidad.",
  TRUE,

  "act_improductivo_patrimonio",
  "Media-Alta",
  "Un mayor activo improductivo respecto al patrimonio reduce flexibilidad del balance y puede elevar necesidades de financiación/absorción.",
  FALSE,

  "car_reprogramada_cartera",
  "Alta",
  "Una mayor cartera reprogramada/restructurada indica mayor incertidumbre sobre flujos contractuales y potencial presión sobre entradas de caja.",
  FALSE,

  "car_mora_cartera",
  "Alta",
  "Mayor mora reduce la realización esperada de flujos de caja de la cartera y eleva la vulnerabilidad financiera vinculada a liquidez.",
  FALSE,

  "car_reprog_venc_ejec",
  "Alta",
  "Mayor deterioro dentro de la cartera reprogramada implica menor recuperación esperada de flujos; su dinámica empírica distinta se controla con leave-one-out.",
  FALSE,

  "car_prev_incobrable",
  "Media",
  "Más previsiones sobre cartera pueden reflejar mayor deterioro crediticio reconocido, aunque simultáneamente constituyen un colchón contable; se usa como señal de deterioro y exige sensibilidad.",
  TRUE,

  "car_categoria_f",
  "Alta",
  "Mayor participación de cartera con requerimiento de previsión del 100% refleja deterioro crediticio severo y menor expectativa de recuperación de flujos.",
  FALSE,

  "solv_patrimonio_activo",
  "Alta",
  "Mayor patrimonio relativo al activo amplía capacidad de absorción de pérdidas y sostiene confianza/acceso a fondeo; reduce vulnerabilidad.",
  FALSE,

  "solv_patrimonio_actcont",
  "Alta",
  "Mayor patrimonio frente a activos y contingentes fortalece capacidad de absorción ante shocks y reduce vulnerabilidad.",
  FALSE,

  "solv_cap",
  "Alta",
  "Mayor suficiencia patrimonial incrementa capacidad de absorción y resiliencia; se orienta como factor protector.",
  FALSE,

  "solv_mora_patrimonio",
  "Alta",
  "Mayor mora relativa al patrimonio consume capacidad de absorción y puede deteriorar confianza y acceso a fondeo.",
  FALSE,

  "solv_mora_neta_patrimonio",
  "Alta",
  "Mayor mora neta de previsiones respecto al patrimonio implica mayor exposición residual de pérdidas frente al colchón patrimonial.",
  FALSE
)

if (
  anyDuplicated(
    auditoria_financiera_signos$variable
  ) >
    0
) {
  stop(
    "La auditoría financiera de signos contiene variables duplicadas."
  )
}


# ══════════════════════════════════════════════════════════════
# 5.  BASE_MODEL, DICCIONARIO Y MUESTRA ANALÍTICA
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 5. Construyendo y validando base modelo ──\n"
)

# Auditoría de correspondencia entre el diccionario y la base.
auditoria_diccionario <- dic %>%
  mutate(
    existe_en_base = original %in% names(Base_raw),
    obligatorio_nucleo = entra_indice
  )

faltantes_nucleo <- auditoria_diccionario %>%
  filter(
    obligatorio_nucleo,
    !existe_en_base
  )

if (
  nrow(faltantes_nucleo) >
    0
) {

  stop(
    "Faltan indicadores núcleo en la base: ",
    paste(
      faltantes_nucleo$original,
      collapse = " | "
    )
  )
}

# Variables complementarias pueden no existir; se documentan, pero no detienen.
dic_ok <- dic %>%
  filter(
    original %in% names(Base_raw)
  )

if (
  anyDuplicated(dic_ok$var) >
    0
) {
  stop(
    "El diccionario contiene nombres internos duplicados."
  )
}

if (
  anyDuplicated(dic_ok$original) >
    0
) {
  stop(
    "El diccionario contiene columnas originales duplicadas."
  )
}


# Auditoría de parseo: distingue columnas ya numéricas de conversiones desde texto
# y detiene la ejecución si existen valores no vacíos que no pudieron convertirse.
auditoria_parseo <- purrr::map_dfr(
  dic_ok$original,
  function(nm) {

    raw <- Base_raw[[nm]]

    raw_chr <- stringr::str_squish(
      as.character(raw)
    )

    vacio <-
      is.na(raw) |
      raw_chr %in% c(
        "",
        "NA",
        "N/A",
        "-",
        "--"
      )

    parsed <- parse_num(
      raw
    )

    tibble(
      variable_original = nm,
      clase_original = paste(
        class(raw),
        collapse = "/"
      ),
      n_total = length(raw),
      n_no_vacios = sum(!vacio),
      n_parseados = sum(
        is.finite(parsed) &
          !is.na(parsed)
      ),
      n_fallo_parseo = sum(
        !vacio &
          (
            is.na(parsed) |
              !is.finite(parsed)
          )
      )
    )
  }
)

if (
  any(
    auditoria_parseo$n_fallo_parseo >
      0
  )
) {

  stop(
    "Existen valores no vacíos que no pudieron convertirse a numérico. Revise 'auditoria_parseo'."
  )
}

Base_model <- Base_raw %>%
  select(
    Fecha,
    all_of(dic_ok$original)
  )

names(Base_model)[
  match(
    dic_ok$original,
    names(Base_model)
  )
] <- dic_ok$var

Base_model <- Base_model %>%
  mutate(
    across(
      -Fecha,
      parse_num
    )
  ) %>%
  arrange(
    Fecha
  )

Base_muestra <- Base_model %>%
  filter(
    Fecha >= fecha_inicio_muestra,
    Fecha <= fecha_fin_muestra
  )

if (
  nrow(Base_muestra) !=
    length(fechas_esperadas)
) {

  stop(
    "La muestra analítica no contiene exactamente ",
    length(fechas_esperadas),
    " meses."
  )
}

if (
  !identical(
    Base_muestra$Fecha,
    fechas_esperadas
  )
) {

  stop(
    "La secuencia temporal de la muestra no coincide con los 192 meses esperados."
  )
}


# Auditoría de valores no finitos.
auditoria_no_finitos <- tibble(
  variable = setdiff(
    names(Base_muestra),
    "Fecha"
  ),
  n_na = vapply(
    Base_muestra[
      setdiff(
        names(Base_muestra),
        "Fecha"
      )
    ],
    function(x) {
      sum(
        is.na(x)
      )
    },
    numeric(1)
  ),
  n_no_finitos = vapply(
    Base_muestra[
      setdiff(
        names(Base_muestra),
        "Fecha"
      )
    ],
    function(x) {
      sum(
        !is.na(x) &
          !is.finite(x)
      )
    },
    numeric(1)
  )
) %>%
  left_join(
    dic_ok %>%
      select(
        var,
        original,
        bloque_id
      ),
    by = c(
      "variable" = "var"
    )
  )

if (
  any(
    auditoria_no_finitos$n_no_finitos >
      0
  )
) {
  stop(
    "Existen valores Inf/-Inf en indicadores. Revise 'auditoria_no_finitos'."
  )
}

cat(
  "  Base completa:",
  nrow(Base_model),
  "obs.\n"
)

cat(
  "  Muestra analítica:",
  nrow(Base_muestra),
  "obs. (",
  format(
    min(Base_muestra$Fecha),
    "%b %Y"
  ),
  "→",
  format(
    max(Base_muestra$Fecha),
    "%b %Y"
  ),
  ")\n"
)


# ══════════════════════════════════════════════════════════════
# 6.  COBERTURA EN MUESTRA
# ══════════════════════════════════════════════════════════════

vars_cand <- dic_ok %>%
  filter(
    entra_indice
  ) %>%
  pull(
    var
  )

cobertura_vars <- Base_muestra %>%
  summarise(
    across(
      all_of(vars_cand),
      ~ mean(
        is.finite(.) &
          !is.na(.)
      )
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "cobertura"
  ) %>%
  left_join(
    dic_ok %>%
      select(
        var,
        original,
        bloque_id
      ),
    by = c(
      "variable" = "var"
    )
  ) %>%
  mutate(
    incluir = cobertura >= umbral_cobertura,
    cobertura_pct = round(
      cobertura * 100,
      1
    ),
    etiqueta_variable = etiquetas_var[variable]
  )

vars_excluidas_cob <- cobertura_vars %>%
  filter(
    !incluir
  ) %>%
  pull(
    variable
  )


# CRÍTICO: se preserva el orden del diccionario; nunca se deriva el orden
# metodológico de un arrange(cobertura).
vars_indice <- dic_ok %>%
  filter(
    entra_indice,
    var %in%
      cobertura_vars$variable[
        cobertura_vars$incluir
      ]
  ) %>%
  pull(
    var
  )

if (
  length(vars_indice) ==
    0
) {
  stop(
    "Ninguna variable supera el umbral de cobertura."
  )
}

if (
  anyDuplicated(vars_indice) >
    0
) {
  stop(
    "vars_indice contiene duplicados."
  )
}

cat(
  "\n  Variables excluidas (cobertura <",
  umbral_cobertura * 100,
  "%):\n"
)

if (
  length(vars_excluidas_cob) ==
    0
) {

  cat(
    "    ninguna\n"
  )

} else {

  for (v in vars_excluidas_cob) {

    cob <- cobertura_vars$cobertura[
      cobertura_vars$variable ==
        v
    ]

    cat(
      "    –",
      v,
      sprintf(
        "(%.1f%%)\n",
        cob * 100
      )
    )
  }
}

cat(
  "  Variables núcleo ICVLF:",
  length(vars_indice),
  "\n"
)


# ══════════════════════════════════════════════════════════════
# 7.  EDA — ANÁLISIS EXPLORATORIO
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 7. EDA ──\n"
)

estadisticas_ext <- Base_muestra %>%
  select(
    all_of(vars_indice)
  ) %>%
  summarise(
    across(
      everything(),
      list(
        n = ~ sum(!is.na(.)),
        n_na = ~ sum(is.na(.)),
        media = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        cv_pct = ~ safe_cv(.),
        min = ~ min(., na.rm = TRUE),
        p5 = ~ quantile(., 0.05, na.rm = TRUE),
        p25 = ~ quantile(., 0.25, na.rm = TRUE),
        mediana = ~ median(., na.rm = TRUE),
        p75 = ~ quantile(., 0.75, na.rm = TRUE),
        p95 = ~ quantile(., 0.95, na.rm = TRUE),
        max = ~ max(., na.rm = TRUE),
        sesgo = ~ moments::skewness(
          .[!is.na(.)]
        ),
        curtosis = ~ moments::kurtosis(
          .[!is.na(.)]
        ) - 3,
        jb_stat = ~ {
          x <- .[!is.na(.)]
          n <- length(x)
          s <- moments::skewness(x)
          k <- moments::kurtosis(x) - 3
          n / 6 * (
            s^2 +
              k^2 / 4
          )
        },
        jb_pval = ~ {
          x <- .[!is.na(.)]
          n <- length(x)
          s <- moments::skewness(x)
          k <- moments::kurtosis(x) - 3
          jb <- n / 6 * (
            s^2 +
              k^2 / 4
          )
          pchisq(
            jb,
            df = 2,
            lower.tail = FALSE
          )
        }
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c(
      "variable",
      "estadistico"
    ),
    names_sep = "__"
  ) %>%
  pivot_wider(
    names_from = estadistico,
    values_from = value
  ) %>%
  left_join(
    dic_ok %>%
      select(
        var,
        original,
        bloque_id
      ),
    by = c(
      "variable" = "var"
    )
  ) %>%
  mutate(
    etiqueta = etiquetas_var[variable],
    normalidad_jb = ifelse(
      jb_pval >= 0.05,
      "Normal (JB ≥ 5%)",
      "No normal (JB < 5%)"
    ),
    orientacion_riesgo = ifelse(
      dic_ok$direccion_riesgo[
        match(
          variable,
          dic_ok$var
        )
      ] ==
        1,
      "↑ mayor valor = mayor riesgo",
      "↓ mayor valor = menor riesgo"
    )
  ) %>%
  arrange(
    bloque_id,
    variable
  )

cat(
  "  Estadísticas ext:",
  nrow(estadisticas_ext),
  "variables\n"
)


test_adf <- lapply(
  vars_indice,
  function(v) {

    x <- Base_muestra[[v]]

    x <- x[
      is.finite(x) &
        !is.na(x)
    ]

    if (
      length(x) <
        20
    ) {

      return(
        tibble(
          variable = v,
          adf_stat = NA,
          adf_pval = NA,
          estacionaria = NA
        )
      )
    }

    res <- tryCatch(
      tseries::adf.test(
        x,
        alternative = "stationary"
      ),
      error = function(e) {
        NULL
      }
    )

    if (is.null(res)) {

      return(
        tibble(
          variable = v,
          adf_stat = NA,
          adf_pval = NA,
          estacionaria = NA
        )
      )
    }

    tibble(
      variable = v,
      adf_stat = as.numeric(
        res$statistic
      ),
      adf_pval = as.numeric(
        res$p.value
      ),
      estacionaria = as.numeric(
        res$p.value
      ) <
        0.05
    )
  }
) %>%
  bind_rows() %>%
  left_join(
    dic_ok %>%
      select(
        var,
        bloque_id
      ),
    by = c(
      "variable" = "var"
    )
  ) %>%
  mutate(
    etiqueta = etiquetas_var[variable],
    interpretacion = ifelse(
      estacionaria,
      "Estacionaria (p < 5%)",
      "No estacionaria (p ≥ 5%)"
    ),
    nota = "Diagnóstico complementario."
  )

cat(
  "  ADF estacionarias:",
  sum(
    test_adf$estacionaria,
    na.rm = TRUE
  ),
  "/ no estacionarias:",
  sum(
    !test_adf$estacionaria,
    na.rm = TRUE
  ),
  "\n"
)


matrices_cor_bloque <- list()

for (
  bl in unique(
    dic_ok$bloque_id[
      dic_ok$var %in%
        vars_indice
    ]
  )
) {

  vars_bl <- dic_ok %>%
    filter(
      bloque_id == bl,
      var %in% vars_indice
    ) %>%
    pull(
      var
    )

  if (
    length(vars_bl) <
      2
  ) {
    next
  }

  Xbl <- Base_muestra %>%
    select(
      all_of(vars_bl)
    )

  C_pearson <- cor(
    Xbl,
    use = "pairwise.complete.obs",
    method = "pearson"
  )

  C_spearman <- cor(
    Xbl,
    use = "pairwise.complete.obs",
    method = "spearman"
  )

  N_pares <- outer(
    vars_bl,
    vars_bl,
    Vectorize(
      function(v1, v2) {
        safe_n(
          Base_muestra[[v1]],
          Base_muestra[[v2]]
        )
      }
    )
  )

  dimnames(N_pares) <- list(
    vars_bl,
    vars_bl
  )

  matrices_cor_bloque[[bl]] <- list(
    pearson = C_pearson,
    spearman = C_spearman,
    n_pares = N_pares,
    n_min = min(
      N_pares[
        upper.tri(N_pares)
      ],
      na.rm = TRUE
    ),
    n_max = max(
      N_pares[
        upper.tri(N_pares)
      ],
      na.rm = TRUE
    )
  )
}


tabla_cor_bloques <- lapply(
  names(matrices_cor_bloque),
  function(bl) {

    mp <- matrices_cor_bloque[[bl]]$pearson
    ms <- matrices_cor_bloque[[bl]]$spearman
    nn <- matrices_cor_bloque[[bl]]$n_pares

    df_p <- as.data.frame(mp) %>%
      tibble::rownames_to_column(
        "var1"
      ) %>%
      pivot_longer(
        -var1,
        names_to = "var2",
        values_to = "r_pearson"
      ) %>%
      filter(
        var1 <
          var2
      )

    df_s <- as.data.frame(ms) %>%
      tibble::rownames_to_column(
        "var1"
      ) %>%
      pivot_longer(
        -var1,
        names_to = "var2",
        values_to = "r_spearman"
      ) %>%
      filter(
        var1 <
          var2
      )

    df_n <- as.data.frame(nn) %>%
      tibble::rownames_to_column(
        "var1"
      ) %>%
      pivot_longer(
        -var1,
        names_to = "var2",
        values_to = "n_pares"
      ) %>%
      filter(
        var1 <
          var2
      )

    df_p %>%
      left_join(
        df_s,
        by = c(
          "var1",
          "var2"
        )
      ) %>%
      left_join(
        df_n,
        by = c(
          "var1",
          "var2"
        )
      ) %>%
      mutate(
        bloque = bl,
        etiq_var1 = etiquetas_var[var1],
        etiq_var2 = etiquetas_var[var2],
        intensidad_p = case_when(
          abs(r_pearson) >= 0.70 ~
            "Alta (|r| ≥ 0.70)",
          abs(r_pearson) >= 0.40 ~
            "Moderada (0.40 ≤ |r| < 0.70)",
          TRUE ~
            "Baja (|r| < 0.40)"
        )
      )
  }
) %>%
  bind_rows()

cat(
  "  Correlaciones EDA: pairwise.complete.obs + N por par (no listwise deletion).\n"
)


# ══════════════════════════════════════════════════════════════
# 8.  NORMALIZACIÓN Z-SCORE Y ORIENTACIÓN ECONÓMICA DEL RIESGO
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 8. Normalización y orientación de riesgo ──\n"
)

X_ref <- Base_muestra %>%
  select(
    all_of(vars_indice)
  )

medias_ref <- vapply(
  X_ref,
  mean,
  numeric(1),
  na.rm = TRUE
)

sds_ref <- vapply(
  X_ref,
  sd,
  numeric(1),
  na.rm = TRUE
)

if (
  any(!is.finite(medias_ref))
) {
  stop(
    "Hay medias no finitas en la muestra de referencia."
  )
}

if (
  any(
    !is.finite(sds_ref) |
      sds_ref <= 0
  )
) {

  malas <- names(sds_ref)[
    !is.finite(sds_ref) |
      sds_ref <= 0
  ]

  stop(
    "Indicadores sin variación utilizable: ",
    paste(
      malas,
      collapse = ", "
    )
  )
}

X_z <- sweep(
  sweep(
    Base_model %>%
      select(
        all_of(vars_indice)
      ) %>%
      as.data.frame(),
    2,
    medias_ref,
    "-"
  ),
  2,
  sds_ref,
  "/"
)

names(X_z) <- vars_indice


# CORRECCIÓN CRÍTICA: emparejamiento nominal explícito.
pos_dir <- match(
  vars_indice,
  dic_ok$var
)

if (
  anyNA(pos_dir)
) {
  stop(
    "No se pudo emparejar una variable con su dirección de riesgo."
  )
}

direccion <- setNames(
  dic_ok$direccion_riesgo[
    pos_dir
  ],
  vars_indice
)

if (
  !all(
    direccion %in%
      c(
        -1,
        1
      )
  )
) {
  stop(
    "Las direcciones de riesgo deben ser -1 o +1."
  )
}

if (
  !identical(
    names(direccion),
    colnames(X_z)
  )
) {
  stop(
    "Error de alineación entre variables y dirección de riesgo."
  )
}


# Prueba explícita contra el diccionario: evita reaparición del bug de v5.1.
stopifnot(
  all(
    direccion ==
      dic_ok$direccion_riesgo[
        match(
          names(direccion),
          dic_ok$var
        )
      ]
  )
)

X_risk <- sweep(
  as.matrix(X_z),
  2,
  direccion[
    colnames(X_z)
  ],
  "*"
)

X_risk <- as.data.frame(
  X_risk
)


# Tabla auditable para anexos/metodología.
tabla_orientacion_riesgo <- tibble(
  variable = vars_indice,
  indicador = etiquetas_var[
    vars_indice
  ],
  bloque = dic_ok$bloque_id[
    match(
      vars_indice,
      dic_ok$var
    )
  ],
  direccion = as.numeric(
    direccion[
      vars_indice
    ]
  ),
  lectura = ifelse(
    direccion[
      vars_indice
    ] ==
      1,
    "Mayor valor original = mayor vulnerabilidad",
    "Mayor valor original = menor vulnerabilidad; se invierte el signo"
  )
) %>%
  left_join(
    auditoria_financiera_signos,
    by = "variable"
  )

datos_risk_full <- bind_cols(
  Fecha = Base_model$Fecha,
  X_risk
)

datos_risk_muestra <- datos_risk_full %>%
  filter(
    Fecha >= fecha_inicio_muestra,
    Fecha <= fecha_fin_muestra
  )

cat(
  "  ✓ Direcciones de riesgo verificadas por nombre para",
  length(direccion),
  "variables.\n"
)


# ══════════════════════════════════════════════════════════════
# 9.  SUBÍNDICES: PRINCIPAL IGUAL + PCA COMO SENSIBILIDAD
# ══════════════════════════════════════════════════════════════

# Diagnóstico PCA: se ejecuta DESPUÉS de estandarizar y orientar al riesgo.
# El PCA no define la dirección económica. La coherencia de cargas se usa
# para evaluar interpretabilidad, no para modificar el modelo principal.
diagnosticar_pca_dimension <- function(
    data_risk,
    vars,
    nombre_bloque
) {

  vars <- vars[
    vars %in%
      names(data_risk)
  ]

  Xb <- data_risk %>%
    select(
      all_of(vars)
    ) %>%
    as.data.frame()

  Xc <- Xb[
    complete.cases(Xb),
    ,
    drop = FALSE
  ]

  n_vars <- ncol(Xb)

  out <- list(
    cor_media_abs = NA_real_,
    cor_media_signed = NA_real_,
    prop_cor_positiva = NA_real_,
    KMO = NA_real_,
    Bartlett_p = NA_real_,
    var_exp_pc1 = NA_real_,
    coherencia_cargas = NA_real_,
    cargas_pc1 = setNames(
      rep(
        NA_real_,
        n_vars
      ),
      vars
    ),
    pesos_pca = setNames(
      rep(
        1 / n_vars,
        n_vars
      ),
      vars
    ),
    pca_interpretable = FALSE
  )

  if (
    n_vars < 2 ||
      nrow(Xc) <
        max(
          20,
          5 * n_vars
        )
  ) {
    return(out)
  }

  C <- cor(
    Xc,
    method = "pearson"
  )

  vals <- C[
    upper.tri(C)
  ]

  out$cor_media_abs <- mean(
    abs(vals),
    na.rm = TRUE
  )

  out$cor_media_signed <- mean(
    vals,
    na.rm = TRUE
  )

  out$prop_cor_positiva <- mean(
    vals > 0,
    na.rm = TRUE
  )

  if (
    n_vars >=
      3
  ) {

    out$KMO <- tryCatch(
      psych::KMO(C)$MSA,
      error = function(e) {
        NA_real_
      }
    )

    out$Bartlett_p <- tryCatch(
      psych::cortest.bartlett(
        C,
        n = nrow(Xc)
      )$p.value,
      error = function(e) {
        NA_real_
      }
    )
  }

  pca <- tryCatch(
    prcomp(
      Xc,
      center = FALSE,
      scale. = FALSE
    ),
    error = function(e) {
      NULL
    }
  )

  if (
    is.null(pca)
  ) {
    return(out)
  }

  out$var_exp_pc1 <-
    pca$sdev[1]^2 /
    sum(
      pca$sdev^2
    )

  score_pc1 <-
    pca$x[
      ,
      1
    ]

  prom_riesgo <- rowMeans(
    Xc,
    na.rm = TRUE
  )

  r_pc1_media <- safe_cor(
    score_pc1,
    prom_riesgo,
    min_pares = 5
  )

  signo <- ifelse(
    is.finite(r_pc1_media) &&
      r_pc1_media < 0,
    -1,
    1
  )

  cargas <- signo *
    pca$rotation[
      ,
      1
    ]

  out$cargas_pc1 <- cargas

  out$coherencia_cargas <- mean(
    cargas >= 0,
    na.rm = TRUE
  )

  out$pesos_pca <- abs(cargas) /
    sum(
      abs(cargas)
    )

  out$pca_interpretable <-
    is.finite(out$cor_media_abs) &&
    out$cor_media_abs >=
      umbral_cor_media_pca &&
    is.finite(out$KMO) &&
    out$KMO >=
      umbral_kmo &&
    is.finite(out$Bartlett_p) &&
    out$Bartlett_p <
      umbral_bartlett &&
    is.finite(out$var_exp_pc1) &&
    out$var_exp_pc1 >=
      umbral_var_pc1 &&
    is.finite(out$coherencia_cargas) &&
    out$coherencia_cargas >=
      umbral_coherencia_pc1

  out
}


estandarizar_score <- function(
    x,
    nombre
) {

  m <- mean(
    x,
    na.rm = TRUE
  )

  s <- sd(
    x,
    na.rm = TRUE
  )

  if (
    !is.finite(s) ||
      s <= 0
  ) {
    stop(
      "El subíndice ",
      nombre,
      " no tiene variación suficiente."
    )
  }

  (x - m) / s
}


crear_subindices_dimension <- function(
    data_risk,
    vars,
    nombre_bloque,
    n_min_frac = n_min_frac_subindice
) {

  vars <- vars[
    vars %in%
      names(data_risk)
  ]

  if (
    length(vars) ==
      0
  ) {
    return(NULL)
  }

  Xb <- data_risk %>%
    select(
      all_of(vars)
    ) %>%
    as.data.frame()

  n_vars <- ncol(Xb)

  diag <- diagnosticar_pca_dimension(
    data_risk,
    vars,
    nombre_bloque
  )


  # PRINCIPAL: igualdad dentro de la dimensión.
  w_eq <- setNames(
    rep(
      1 / n_vars,
      n_vars
    ),
    vars
  )

  score_eq_raw <- suma_ponderada_robusto(
    as.matrix(Xb),
    w_eq,
    n_min_frac
  )

  score_eq <- estandarizar_score(
    score_eq_raw,
    paste0(
      nombre_bloque,
      " [equal]"
    )
  )


  # SENSIBILIDAD: pesos informados por la magnitud del PC1.
  w_pca <- validar_pesos(
    diag$pesos_pca,
    vars
  )

  score_pca_raw <- suma_ponderada_robusto(
    as.matrix(Xb),
    w_pca,
    n_min_frac
  )

  score_pca <- estandarizar_score(
    score_pca_raw,
    paste0(
      nombre_bloque,
      " [PCA]"
    )
  )

  list(
    score_equal = score_eq,
    score_equal_raw = score_eq_raw,
    score_pca = score_pca,
    score_pca_raw = score_pca_raw,

    metodo = tibble(
      bloque = nombre_bloque,
      n_indicadores = n_vars,
      n_casos_completos = sum(
        complete.cases(Xb)
      ),
      cor_media_abs = diag$cor_media_abs,
      cor_media_orientada = diag$cor_media_signed,
      prop_cor_positiva = diag$prop_cor_positiva,
      KMO = diag$KMO,
      Bartlett_pvalue = diag$Bartlett_p,
      var_exp_pc1 = diag$var_exp_pc1,
      coherencia_cargas_pc1 = diag$coherencia_cargas,
      pca_interpretable = diag$pca_interpretable,
      media_subindice_bruto = mean(
        score_eq_raw,
        na.rm = TRUE
      ),
      sd_subindice_bruto = sd(
        score_eq_raw,
        na.rm = TRUE
      ),
      n_na_subindice_bruto = sum(
        is.na(score_eq_raw)
      ),
      metodo_principal = "Promedio simple de indicadores orientados (pesos internos iguales)",
      rol_pca = "Sensibilidad empírica; no determina la especificación principal"
    ),

    pesos_equal = tibble(
      bloque = nombre_bloque,
      indicador = vars,
      etiqueta = etiquetas_var[vars],
      peso_interno = as.numeric(w_eq),
      metodo = "Pesos internos iguales — principal"
    ),

    pesos_pca = tibble(
      bloque = nombre_bloque,
      indicador = vars,
      etiqueta = etiquetas_var[vars],
      carga_pc1 = as.numeric(
        diag$cargas_pc1[vars]
      ),
      peso_pca = as.numeric(
        w_pca[vars]
      ),
      pca_interpretable = diag$pca_interpretable,
      metodo = "|coeficiente PC1| normalizado — sensibilidad"
    )
  )
}


# Helper para reconstrucciones leave-one-out y validación sin solapamiento.
crear_subindice_equal_simple <- function(
    data_risk,
    vars,
    nombre_bloque,
    n_min_frac = n_min_frac_subindice
) {

  vars <- vars[
    vars %in%
      names(data_risk)
  ]

  if (
    length(vars) ==
      0
  ) {
    return(NULL)
  }

  Xb <- data_risk %>%
    select(
      all_of(vars)
    ) %>%
    as.data.frame()

  w <- setNames(
    rep(
      1 / length(vars),
      length(vars)
    ),
    vars
  )

  raw <- suma_ponderada_robusto(
    as.matrix(Xb),
    w,
    n_min_frac
  )

  estandarizar_score(
    raw,
    nombre_bloque
  )
}


# Normalización robusta mediana/MAD para sensibilidad.
param_rob <- lapply(
  Base_muestra %>%
    select(
      all_of(vars_indice)
    ),
  robust_scale_ref
)

centros_rob <- vapply(
  param_rob,
  function(z) {
    z[["center"]]
  },
  numeric(1)
)

escalas_rob <- vapply(
  param_rob,
  function(z) {
    z[["scale"]]
  },
  numeric(1)
)

X_z_rob <- sweep(
  sweep(
    Base_model %>%
      select(
        all_of(vars_indice)
      ) %>%
      as.data.frame(),
    2,
    centros_rob,
    "-"
  ),
  2,
  escalas_rob,
  "/"
)

X_risk_rob <- sweep(
  as.matrix(X_z_rob),
  2,
  direccion[
    colnames(X_z_rob)
  ],
  "*"
)

datos_risk_rob_muestra <- bind_cols(
  Fecha = Base_model$Fecha,
  as.data.frame(X_risk_rob)
) %>%
  filter(
    Fecha >= fecha_inicio_muestra,
    Fecha <= fecha_fin_muestra
  )


# ══════════════════════════════════════════════════════════════
# 10.  CONSTRUCCIÓN DE SUBÍNDICES
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 10. Construyendo subíndices: Equal principal + PCA sensibilidad ──\n"
)

vars_por_bloque <- dic_ok %>%
  filter(
    var %in% vars_indice
  ) %>%
  group_by(
    bloque_id
  ) %>%
  summarise(
    vars = list(var),
    .groups = "drop"
  )

subindices <- tibble(
  Fecha = datos_risk_muestra$Fecha
)

subindices_brutos <- tibble(
  Fecha = datos_risk_muestra$Fecha
)

subindices_pca_interno <- tibble(
  Fecha = datos_risk_muestra$Fecha
)

subindices_robustos <- tibble(
  Fecha = datos_risk_muestra$Fecha
)

metodos_subindices <- list()
pesos_equal_list <- list()
pesos_pca_list <- list()

for (
  i in seq_len(
    nrow(vars_por_bloque)
  )
) {

  bloque_i <- vars_por_bloque$bloque_id[i]
  vars_i <- vars_por_bloque$vars[[i]]

  obj <- crear_subindices_dimension(
    datos_risk_muestra,
    vars_i,
    bloque_i
  )

  if (
    !is.null(obj)
  ) {

    subindices[[bloque_i]] <-
      obj$score_equal

    subindices_brutos[[bloque_i]] <-
      obj$score_equal_raw

    subindices_pca_interno[[bloque_i]] <-
      obj$score_pca

    subindices_robustos[[bloque_i]] <-
      crear_subindice_equal_simple(
        datos_risk_rob_muestra,
        vars_i,
        paste0(
          bloque_i,
          " [robust-z]"
        )
      )

    metodos_subindices[[bloque_i]] <-
      obj$metodo

    pesos_equal_list[[bloque_i]] <-
      obj$pesos_equal

    pesos_pca_list[[bloque_i]] <-
      obj$pesos_pca
  }
}

tabla_metodos_subindices <- bind_rows(
  metodos_subindices
) %>%
  mutate(
    bloque_nombre = etiquetas_bloque[bloque]
  )

tabla_pesos_internos <- bind_rows(
  pesos_equal_list
)

tabla_pesos_pca_internos <- bind_rows(
  pesos_pca_list
)

bloques_core <- intersect(
  c(
    "liquidez",
    "fondeo",
    "activos",
    "cartera",
    "solvencia"
  ),
  names(subindices)
)

if (
  length(bloques_core) !=
    5
) {
  stop(
    "No se construyeron las cinco dimensiones núcleo del ICVLF."
  )
}

if (
  anyNA(
    subindices[
      ,
      bloques_core
    ]
  )
) {

  stop(
    "Al menos una dimensión presenta NA tras aplicar el umbral interno. ",
    "No se permite reponderación automática ENTRE dimensiones en el ICVLF principal."
  )
}

tabla_pesos_jerarquicos <- tabla_pesos_internos %>%
  left_join(
    tabla_metodos_subindices %>%
      select(
        bloque,
        sd_subindice_bruto
      ),
    by = "bloque"
  ) %>%
  mutate(
    peso_dimension = 1 /
      length(bloques_core),
    peso_jerarquico_nominal =
      peso_dimension *
      peso_interno,
    coeficiente_analitico_z =
      peso_dimension *
      peso_interno /
      sd_subindice_bruto,
    nota = paste0(
      "Peso jerárquico nominal = 20% × peso interno. ",
      "El coeficiente analítico incorpora además la reestandarización del subíndice bruto."
    )
  )


auditoria_reponderacion_interna <- map_dfr(
  bloques_core,
  function(bl) {

    vars_bl <- vars_por_bloque$vars[[
      match(
        bl,
        vars_por_bloque$bloque_id
      )
    ]]

    X <- datos_risk_muestra %>%
      select(
        all_of(vars_bl)
      )

    n_disp <- rowSums(
      is.finite(
        as.matrix(X)
      ) &
        !is.na(
          as.matrix(X)
        )
    )

    tibble(
      Fecha = datos_risk_muestra$Fecha,
      bloque = bl,
      n_indicadores_total = length(vars_bl),
      n_disponibles = n_disp,
      fraccion_disponible =
        n_disp /
        length(vars_bl),
      hubo_reponderacion_interna =
        n_disp <
        length(vars_bl)
    )
  }
) %>%
  filter(
    hubo_reponderacion_interna
  )


# Auditorías de pesos y estandarización.
tabla_suma_pesos <- tabla_pesos_internos %>%
  group_by(
    bloque
  ) %>%
  summarise(
    suma_pesos = sum(peso_interno),
    .groups = "drop"
  )

if (
  any(
    abs(
      tabla_suma_pesos$suma_pesos -
        1
    ) >
      1e-10
  )
) {
  stop(
    "Pesos iguales internos no suman 1."
  )
}


tabla_suma_pesos_pca <- tabla_pesos_pca_internos %>%
  group_by(
    bloque
  ) %>%
  summarise(
    suma_pesos = sum(peso_pca),
    .groups = "drop"
  )

if (
  any(
    abs(
      tabla_suma_pesos_pca$suma_pesos -
        1
    ) >
      1e-10
  )
) {
  stop(
    "Pesos PCA internos no suman 1."
  )
}


auditoria_subindices <- tibble(
  bloque = bloques_core,
  media = vapply(
    subindices[bloques_core],
    mean,
    numeric(1),
    na.rm = TRUE
  ),
  sd = vapply(
    subindices[bloques_core],
    sd,
    numeric(1),
    na.rm = TRUE
  ),
  n_na = vapply(
    subindices[bloques_core],
    function(x) {
      sum(
        is.na(x)
      )
    },
    numeric(1)
  )
)

cor_subindices <- cor(
  subindices %>%
    select(
      all_of(bloques_core)
    ),
  use = "pairwise.complete.obs"
)


# Redundancia: pares con |r|>=0.90 en datos ORIENTADOS al riesgo.
tabla_redundancia <- map_dfr(
  bloques_core,
  function(bl) {

    vars_bl <- vars_por_bloque$vars[[
      match(
        bl,
        vars_por_bloque$bloque_id
      )
    ]]

    X <- datos_risk_muestra %>%
      select(
        all_of(vars_bl)
      )

    C <- cor(
      X,
      use = "pairwise.complete.obs"
    )

    as.data.frame(C) %>%
      rownames_to_column(
        "var1"
      ) %>%
      pivot_longer(
        -var1,
        names_to = "var2",
        values_to = "r"
      ) %>%
      filter(
        var1 < var2,
        abs(r) >= 0.90
      ) %>%
      transmute(
        bloque = bl,
        indicador_1 = etiquetas_var[var1],
        indicador_2 = etiquetas_var[var2],
        r_orientado = r,
        advertencia = "Alta redundancia potencial; revisar doble conteo"
      )
  }
)

cat(
  "  Método principal dentro de cada dimensión: pesos iguales.\n"
)

cat(
  "  PCA conservado como sensibilidad; diagnósticos exportados por dimensión.\n"
)


# ══════════════════════════════════════════════════════════════
# 11.  ICVLF — PRINCIPAL Y SENSIBILIDADES
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 11. Construyendo ICVLF y especificaciones de sensibilidad ──\n"
)


# PRINCIPAL: pesos iguales dentro de dimensión + 20% entre las cinco dimensiones.
# No se permite que un NA de una dimensión convierta silenciosamente 20% en 25%.
pesos_dimensionales <- setNames(
  rep(
    1 / length(bloques_core),
    length(bloques_core)
  ),
  bloques_core
)

subindices$ICVLF_equal <- suma_ponderada_robusto(
  as.matrix(
    subindices[
      ,
      bloques_core
    ]
  ),
  pesos_dimensionales,
  n_min_frac = 1
)

stopifnot(
  max(
    abs(
      subindices$ICVLF_equal -
        rowMeans(
          subindices[
            ,
            bloques_core
          ]
        )
    ),
    na.rm = TRUE
  ) <
    1e-10
)


# Sensibilidad A: pesos alternativos ENTRE dimensiones. No es ponderación regulatoria.
pesos_alt <- c(
  liquidez = 0.35,
  fondeo = 0.25,
  activos = 0.15,
  cartera = 0.15,
  solvencia = 0.10
)

pesos_alt <- pesos_alt[
  bloques_core
] /
  sum(
    pesos_alt[
      bloques_core
    ]
  )

subindices$ICVLF_alt_pesos <- suma_ponderada_robusto(
  as.matrix(
    subindices[
      ,
      bloques_core
    ]
  ),
  pesos_alt
)


# Sensibilidad B: núcleo más estrecho liquidez + fondeo.
subindices$ICVLF_nucleo <-
  0.5 *
  subindices$liquidez +
  0.5 *
  subindices$fondeo


# Sensibilidad C: PCA global de los cinco subíndices PRINCIPALES.
X_sub_imp <- subindices %>%
  select(
    all_of(bloques_core)
  ) %>%
  mutate(
    across(
      everything(),
      ~ ifelse(
        is.na(.),
        mean(
          .,
          na.rm = TRUE
        ),
        .
      )
    )
  )

pca_sub <- prcomp(
  X_sub_imp,
  center = TRUE,
  scale. = TRUE
)

score_pca_sub <- pca_sub$x[
  ,
  1
]

if (
  safe_cor(
    score_pca_sub,
    subindices$ICVLF_equal,
    min_pares = 5
  ) <
    0
) {

  score_pca_sub <- -score_pca_sub

  pesos_sub_pca <- -pca_sub$rotation[
    ,
    1
  ]

} else {

  pesos_sub_pca <- pca_sub$rotation[
    ,
    1
  ]
}

subindices$ICVLF_pca_global <- as.numeric(
  scale(
    score_pca_sub
  )
)

var_exp_pca_global <-
  pca_sub$sdev[1]^2 /
  sum(
    pca_sub$sdev^2
  )


# Sensibilidad D: PCA para pesos DENTRO de cada dimensión.
if (
  anyNA(
    subindices_pca_interno[
      ,
      bloques_core
    ]
  )
) {
  stop(
    "NA en dimensiones PCA internas."
  )
}

subindices$ICVLF_pca_interno <- suma_ponderada_robusto(
  as.matrix(
    subindices_pca_interno[
      ,
      bloques_core
    ]
  ),
  pesos_dimensionales,
  n_min_frac = 1
)


# Sensibilidad E: z robusto mediana/MAD, manteniendo pesos iguales.
if (
  anyNA(
    subindices_robustos[
      ,
      bloques_core
    ]
  )
) {
  stop(
    "NA en dimensiones robust-z."
  )
}

subindices$ICVLF_robust_z <- suma_ponderada_robusto(
  as.matrix(
    subindices_robustos[
      ,
      bloques_core
    ]
  ),
  pesos_dimensionales,
  n_min_frac = 1
)


tabla_pesos_pca_bloques <- tibble(
  bloque = names(pesos_sub_pca),
  bloque_nombre = etiquetas_bloque_corto[
    names(pesos_sub_pca)
  ],
  carga_pca_global = as.numeric(
    pesos_sub_pca
  ),
  peso_abs_normalizado =
    abs(pesos_sub_pca) /
    sum(
      abs(pesos_sub_pca)
    )
)


indices <- subindices %>%
  mutate(
    ICVLF_equal_100 = normalizar_0100(
      ICVLF_equal
    ),
    ICVLF_alt_pesos_100 = normalizar_0100(
      ICVLF_alt_pesos
    ),
    ICVLF_pca_global_100 = normalizar_0100(
      ICVLF_pca_global
    ),
    ICVLF_nucleo_100 = normalizar_0100(
      ICVLF_nucleo
    ),
    ICVLF_pca_interno_100 = normalizar_0100(
      ICVLF_pca_interno
    ),
    ICVLF_robust_z_100 = normalizar_0100(
      ICVLF_robust_z
    ),
    nivel_vulnerabilidad = clasificar_cuartiles(
      ICVLF_equal_100
    )
  )


# Helper para cualquier reconstrucción del ICVLF principal con subconjuntos de variables.
construir_icvlf_equal_desde_vars <- function(
    vars_keep
) {

  vpb <- dic_ok %>%
    filter(
      var %in% vars_keep,
      bloque_id %in% bloques_core
    ) %>%
    group_by(
      bloque_id
    ) %>%
    summarise(
      vars = list(var),
      .groups = "drop"
    )

  tmp <- tibble(
    Fecha = datos_risk_muestra$Fecha
  )

  for (
    i in seq_len(
      nrow(vpb)
    )
  ) {

    tmp[[vpb$bloque_id[i]]] <- crear_subindice_equal_simple(
      datos_risk_muestra,
      vpb$vars[[i]],
      vpb$bloque_id[i]
    )
  }

  if (
    !all(
      bloques_core %in%
        names(tmp)
    )
  ) {
    return(
      rep(
        NA_real_,
        nrow(tmp)
      )
    )
  }

  w_dim <- setNames(
    rep(
      1 / length(bloques_core),
      length(bloques_core)
    ),
    bloques_core
  )

  # En reconstrucciones no se permite que un NA en una dimensión repondere las restantes.
  suma_ponderada_robusto(
    as.matrix(
      tmp[
        ,
        bloques_core
      ]
    ),
    w_dim,
    n_min_frac = 1
  )
}


cat(
  "  Varianza explicada PCA global (PC1): ",
  sprintf(
    "%.1f%%\n",
    var_exp_pca_global * 100
  ),
  sep = ""
)

cat(
  "  Distribución de niveles históricos relativos:\n"
)

print(
  table(
    indices$nivel_vulnerabilidad
  )
)


# ══════════════════════════════════════════════════════════════
# 12.  INVENTARIO, SUBPERIODOS Y TABLAS DESCRIPTIVAS
# ══════════════════════════════════════════════════════════════

metadata_ejecucion <- tibble(
  item = c(
    "Versión del script",
    "Archivo de datos",
    "MD5 de la base",
    "Fecha de inicio",
    "Fecha de fin",
    "Total de observaciones",
    "Frecuencia",
    "Unidad de análisis",
    "Umbral de cobertura",
    "Interpretación temporal",
    "Escala principal"
  ),
  valor = c(
    "ICVLF v6.3-B1",
    basename(ruta),
    unname(
      tools::md5sum(ruta)
    ),
    format(
      fecha_inicio_muestra,
      "%d/%m/%Y"
    ),
    format(
      fecha_fin_muestra,
      "%d/%m/%Y"
    ),
    as.character(
      nrow(indices)
    ),
    "Mensual",
    unidad_analisis_texto,
    paste0(
      umbral_cobertura * 100,
      "%"
    ),
    "Retrospectiva: parámetros estimados sobre la muestra completa 2010–2025",
    "z-score orientado al riesgo; pesos internos iguales; 0–100 solo para comunicación"
  )
)


periodo_efectivo <- metadata_ejecucion %>%
  filter(
    item %in% c(
      "Fecha de inicio",
      "Fecha de fin",
      "Total de observaciones",
      "Frecuencia",
      "Unidad de análisis",
      "Umbral de cobertura"
    )
  )


inventario_bloques <- dic_ok %>%
  filter(
    bloque_id %in% bloques_core
  ) %>%
  mutate(
    incluido_nucleo = var %in% vars_indice,
    rol = case_when(
      entra_indice &
        incluido_nucleo ~
        "Núcleo ICVLF",
      !entra_indice ~
        "Complementario / proxy",
      TRUE ~
        "Excluido por cobertura"
    )
  ) %>%
  group_by(
    bloque_id,
    bloque
  ) %>%
  summarise(
    n_disponibles = n(),
    n_candidatos_nucleo = sum(
      entra_indice
    ),
    n_nucleo_icvlf = sum(
      entra_indice &
        incluido_nucleo
    ),
    n_complementarios = sum(
      !entra_indice
    ),
    n_excluidos_cob = sum(
      entra_indice &
        !incluido_nucleo
    ),
    .groups = "drop"
  ) %>%
  mutate(
    bloque_nombre = etiquetas_bloque[bloque_id]
  )


# Catálogo completo de cobertura para las cinco dimensiones, incluyendo proxies complementarios.
vars_catalogo <- dic_ok %>%
  filter(
    bloque_id %in% bloques_core
  ) %>%
  pull(
    var
  )

cobertura_catalogo <- Base_muestra %>%
  summarise(
    across(
      all_of(vars_catalogo),
      ~ mean(
        is.finite(.) &
          !is.na(.)
      )
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "cobertura"
  ) %>%
  left_join(
    dic_ok %>%
      select(
        var,
        original,
        bloque_id,
        entra_indice,
        direccion_riesgo
      ),
    by = c(
      "variable" = "var"
    )
  ) %>%
  mutate(
    cobertura_pct =
      100 *
      cobertura,
    indicador = etiquetas_var[variable],
    rol = case_when(
      entra_indice &
        variable %in% vars_indice ~
        "Núcleo ICVLF",
      !entra_indice ~
        "Complementario / proxy",
      TRUE ~
        "Excluido por cobertura"
    ),
    direccion = ifelse(
      direccion_riesgo ==
        1,
      "+1",
      "−1"
    ),
    lectura_direccion = ifelse(
      direccion_riesgo ==
        1,
      "Mayor valor = mayor vulnerabilidad",
      "Mayor valor = menor vulnerabilidad"
    )
  )


indices_subperiodos <- indices %>%
  mutate(
    subperiodo = case_when(
      Fecha <= as.Date("2014-12-01") ~
        "2010–2014",
      Fecha <= as.Date("2019-12-01") ~
        "2015–2019",
      Fecha <= as.Date("2021-12-01") ~
        "2020–2021",
      TRUE ~
        "2022–2025"
    )
  )


tabla_subperiodos <- indices_subperiodos %>%
  group_by(
    subperiodo
  ) %>%
  summarise(
    n = n(),
    icvlf_media = mean(
      ICVLF_equal_100,
      na.rm = TRUE
    ),
    icvlf_sd = sd(
      ICVLF_equal_100,
      na.rm = TRUE
    ),
    icvlf_min = min(
      ICVLF_equal_100,
      na.rm = TRUE
    ),
    icvlf_max = max(
      ICVLF_equal_100,
      na.rm = TRUE
    ),
    icvlf_sesgo = moments::skewness(
      na.omit(
        ICVLF_equal_100
      )
    ),
    regimen_modal = moda_factor(
      nivel_vulnerabilidad
    ),
    pct_alta_muy_alta = mean(
      nivel_vulnerabilidad %in%
        c(
          "Alta",
          "Muy alta"
        ),
      na.rm = TRUE
    ) *
      100,
    .groups = "drop"
  )

cat(
  "\n  Estadísticas por subperiodo:\n"
)

print(
  tabla_subperiodos %>%
    select(
      subperiodo,
      n,
      icvlf_media,
      regimen_modal
    )
)


# ══════════════════════════════════════════════════════════════
# 13.  PROXIES DE CONSISTENCIA PRUDENCIAL
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 13. Construyendo proxies de consistencia prudencial ──\n"
)

proxies <- Base_muestra %>%
  select(
    Fecha
  )


# P1 y P2 son proxies de cobertura construidos con la base disponible; NO son el LCR regulatorio de Basilea III.
if (
  "liq_disp_oblig_cp" %in%
    names(Base_muestra)
) {
  proxies$P1 <-
    Base_muestra$liq_disp_oblig_cp
}

if (
  "liq_disp_inv_oblig_cp" %in%
    names(Base_muestra)
) {
  proxies$P2 <-
    Base_muestra$liq_disp_inv_oblig_cp
}

if (
  "liq_activos_pasivos_cp" %in%
    names(Base_muestra)
) {
  proxies$CLA <-
    Base_muestra$liq_activos_pasivos_cp
}


vars_ipfe <- intersect(
  c(
    "fon_personas_naturales",
    "fon_dias_permanencia_dpf",
    "solv_patrimonio_activo",
    "fon_oblig_subordinadas_paspat",
    "fon_juridicas_institucionales",
    "fon_oblig_bancos_paspat"
  ),
  names(Base_muestra)
)

if (
  length(vars_ipfe) >=
    2
) {

  X_ipfe <- Base_muestra %>%
    select(
      all_of(vars_ipfe)
    )

  ok_ipfe <- complete.cases(
    X_ipfe
  )

  X_ok <- X_ipfe[
    ok_ipfe,
    ,
    drop = FALSE
  ]

  X_ipfe_z <- as.data.frame(
    scale(X_ok)
  )


  # +1 = mayor estabilidad; -1 = menor estabilidad.
  dir_est <- c(
    fon_personas_naturales = 1,
    fon_dias_permanencia_dpf = 1,
    solv_patrimonio_activo = 1,
    fon_oblig_subordinadas_paspat = 1,
    fon_juridicas_institucionales = -1,
    fon_oblig_bancos_paspat = -1
  )

  if (
    anyNA(
      match(
        names(X_ipfe_z),
        names(dir_est)
      )
    )
  ) {
    stop(
      "No se pudo emparejar la orientación del proxy de fondeo estable."
    )
  }

  dir_est <- dir_est[
    names(X_ipfe_z)
  ]

  X_ipfe_st <- sweep(
    as.matrix(X_ipfe_z),
    2,
    dir_est,
    "*"
  )

  ipfe_raw <- rowMeans(
    X_ipfe_st
  )

  IPFE_df <- tibble(
    Fecha = Base_muestra$Fecha[
      ok_ipfe
    ],
    IPFE = as.numeric(
      scale(ipfe_raw)
    ),
    IPFE_100 = normalizar_0100(
      ipfe_raw
    )
  )

  proxies <- proxies %>%
    left_join(
      IPFE_df,
      by = "Fecha"
    )
}


proxies_disponibles <- intersect(
  c(
    "P1",
    "P2",
    "CLA",
    "IPFE_100"
  ),
  names(proxies)
)


etiquetas_proxy <- c(
  P1 = "Proxy cobertura líquida 1: Disp. / Oblig. corto plazo (no LCR regulatorio)",
  P2 = "Proxy cobertura líquida 2: (Disp.+Inv.Temp.) / Oblig. corto plazo (no LCR regulatorio)",
  CLA = "CLA: Activos líquidos / pasivos de corto plazo (cobertura parcial)",
  IPFE_100 = "IPFE: proxy compuesto de estabilidad de fondeo (0–100)"
)


etiquetas_proxy_corto <- c(
  P1 = "P1 cobertura líquida",
  P2 = "P2 cobertura líquida + inversiones",
  CLA = "CLA (cobertura parcial)",
  IPFE_100 = "IPFE estabilidad de fondeo"
)

cat(
  "  Proxies disponibles:",
  paste(
    proxies_disponibles,
    collapse = ", "
  ),
  "\n"
)


# ══════════════════════════════════════════════════════════════
# 14.  VALIDACIÓN CONVERGENTE Y CONSISTENCIA PRUDENCIAL
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 14. Validación convergente / consistencia prudencial ──\n"
)

base_val <- indices %>%
  select(
    Fecha,
    ICVLF_equal,
    ICVLF_equal_100,
    ICVLF_alt_pesos_100,
    ICVLF_pca_global_100,
    ICVLF_nucleo_100,
    ICVLF_pca_interno_100,
    ICVLF_robust_z_100,
    nivel_vulnerabilidad
  ) %>%
  left_join(
    proxies,
    by = "Fecha"
  )


indices_icvlf <- c(
  "ICVLF_equal_100",
  "ICVLF_alt_pesos_100",
  "ICVLF_pca_global_100",
  "ICVLF_nucleo_100",
  "ICVLF_pca_interno_100",
  "ICVLF_robust_z_100"
)


etiq_icvlf <- c(
  ICVLF_equal_100 = "ICVLF principal: igualdad interna + 20% × 5",
  ICVLF_alt_pesos_100 = "Pesos dimensionales alternativos (35–25–15–15–10)",
  ICVLF_pca_global_100 = "PCA global de subíndices — sensibilidad",
  ICVLF_nucleo_100 = "Benchmark de alcance estrecho: Liquidez + Fondeo (50–50)",
  ICVLF_pca_interno_100 = "Pesos internos PCA — sensibilidad",
  ICVLF_robust_z_100 = "Normalización robusta mediana/MAD — sensibilidad"
)


tabla_correlaciones <- expand.grid(
  indice = indices_icvlf,
  proxy = proxies_disponibles,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    n_obs = map2_int(
      indice,
      proxy,
      ~ safe_n(
        base_val[[.x]],
        base_val[[.y]]
      )
    ),
    pearson = map2_dbl(
      indice,
      proxy,
      ~ safe_cor(
        base_val[[.x]],
        base_val[[.y]],
        "pearson"
      )
    ),
    spearman = map2_dbl(
      indice,
      proxy,
      ~ safe_cor(
        base_val[[.x]],
        base_val[[.y]],
        "spearman"
      )
    ),
    intensidad = intensidad_cor(
      pearson
    ),
    etiq_indice = etiq_icvlf[indice],
    etiq_proxy = etiquetas_proxy[proxy]
  )


# Validación convergente reduciendo solapamiento mecánico con P1/P2.
proxy_var_solapada <- c(
  P1 = "liq_disp_oblig_cp",
  P2 = "liq_disp_inv_oblig_cp"
)

tabla_validacion_sin_solapamiento <- map_dfr(
  names(proxy_var_solapada),
  function(px) {

    v <- unname(
      proxy_var_solapada[px]
    )

    if (
      !(px %in% names(base_val)) ||
        !(v %in% vars_indice)
    ) {
      return(
        tibble()
      )
    }

    alt_z <- construir_icvlf_equal_desde_vars(
      setdiff(
        vars_indice,
        v
      )
    )

    tibble(
      proxy = px,
      indicador_retirado = etiquetas_var[v],
      n_obs = safe_n(
        alt_z,
        base_val[[px]]
      ),
      pearson = safe_cor(
        alt_z,
        base_val[[px]],
        "pearson"
      ),
      spearman = safe_cor(
        alt_z,
        base_val[[px]],
        "spearman"
      ),
      lectura = "ICVLF reconstruido sin el indicador que coincide con el proxy"
    )
  }
) %>%
  mutate(
    etiq_proxy = etiquetas_proxy[proxy]
  )


base_val_sp <- base_val %>%
  left_join(
    indices_subperiodos %>%
      select(
        Fecha,
        subperiodo
      ),
    by = "Fecha"
  )


tabla_correlaciones_subperiodo <- expand.grid(
  proxy = proxies_disponibles,
  subperiodo = unique(
    indices_subperiodos$subperiodo
  ),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    n_obs = map2_int(
      proxy,
      subperiodo,
      function(px, sp) {

        df <- base_val_sp %>%
          filter(
            subperiodo == sp
          )

        safe_n(
          df$ICVLF_equal_100,
          df[[px]]
        )
      }
    ),
    pearson = map2_dbl(
      proxy,
      subperiodo,
      function(px, sp) {

        df <- base_val_sp %>%
          filter(
            subperiodo == sp
          )

        safe_cor(
          df$ICVLF_equal_100,
          df[[px]],
          "pearson"
        )
      }
    ),
    spearman = map2_dbl(
      proxy,
      subperiodo,
      function(px, sp) {

        df <- base_val_sp %>%
          filter(
            subperiodo == sp
          )

        safe_cor(
          df$ICVLF_equal_100,
          df[[px]],
          "spearman"
        )
      }
    ),
    intensidad = intensidad_cor(
      pearson
    ),
    etiq_proxy = etiquetas_proxy[proxy]
  )


# Asociación adelantada descriptiva. NO se interpreta como prueba predictiva.
tabla_rezagos <- expand.grid(
  proxy = proxies_disponibles,
  h = c(
    1,
    3,
    6
  ),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    n_obs = map2_int(
      proxy,
      h,
      function(px, h_) {
        safe_n(
          base_val$ICVLF_equal_100,
          dplyr::lead(
            base_val[[px]],
            h_
          )
        )
      }
    ),
    pearson_h = map2_dbl(
      proxy,
      h,
      function(px, h_) {
        safe_cor(
          base_val$ICVLF_equal_100,
          dplyr::lead(
            base_val[[px]],
            h_
          ),
          "pearson"
        )
      }
    ),
    spearman_h = map2_dbl(
      proxy,
      h,
      function(px, h_) {
        safe_cor(
          base_val$ICVLF_equal_100,
          dplyr::lead(
            base_val[[px]],
            h_
          ),
          "spearman"
        )
      }
    ),
    intensidad = intensidad_cor(
      pearson_h
    ),
    etiq_proxy = etiquetas_proxy[proxy],
    horizonte = paste0(
      h,
      " mes",
      ifelse(
        h == 1,
        "",
        "es"
      )
    ),
    nota = "Asociación adelantada descriptiva; no constituye validación predictiva"
  )


# Regresión de consistencia con errores Newey-West (HAC) a 12 rezagos.
# Se usa como inferencia complementaria; no transforma la relación en causal.
tabla_hac_consistencia <- map_dfr(
  proxies_disponibles,
  function(px) {

    df <- base_val %>%
      select(
        ICVLF_equal,
        all_of(px)
      ) %>%
      rename(
        proxy = all_of(px)
      ) %>%
      filter(
        complete.cases(.),
        is.finite(ICVLF_equal),
        is.finite(proxy)
      )

    if (
      nrow(df) <
        max(
          24,
          lag_hac + 5
        )
    ) {

      return(
        tibble(
          proxy = px,
          n_obs = nrow(df),
          beta_icvlf = NA_real_,
          se_hac = NA_real_,
          t_hac = NA_real_,
          p_hac = NA_real_,
          r2 = NA_real_
        )
      )
    }

    fit <- lm(
      proxy ~ ICVLF_equal,
      data = df
    )

    V <- sandwich::NeweyWest(
      fit,
      lag = lag_hac,
      prewhite = FALSE,
      adjust = TRUE
    )

    ct <- lmtest::coeftest(
      fit,
      vcov. = V
    )

    tibble(
      proxy = px,
      n_obs = nrow(df),
      beta_icvlf = unname(
        coef(fit)["ICVLF_equal"]
      ),
      se_hac = unname(
        ct[
          "ICVLF_equal",
          "Std. Error"
        ]
      ),
      t_hac = unname(
        ct[
          "ICVLF_equal",
          "t value"
        ]
      ),
      p_hac = unname(
        ct[
          "ICVLF_equal",
          "Pr(>|t|)"
        ]
      ),
      r2 = summary(fit)$r.squared
    )
  }
) %>%
  mutate(
    etiq_proxy = etiquetas_proxy[proxy],
    nota = "Inferencia HAC Newey-West; asociación, no causalidad"
  )


# ── Estacionariedad ICVLF/proxies y HAC en primeras diferencias ─────────────
# ADF y PP: H0 = raíz unitaria. KPSS: H0 = estacionariedad de nivel.
test_estacionariedad_serie <- function(x) {

  x <- x[
    is.finite(x) &
      !is.na(x)
  ]

  if (
    length(x) < 25 ||
      sd(x) == 0
  ) {

    return(
      tibble(
        n = length(x),
        adf_stat = NA_real_,
        adf_p = NA_real_,
        pp_stat = NA_real_,
        pp_p = NA_real_,
        kpss_level_stat = NA_real_,
        kpss_level_p = NA_real_,
        kpss_trend_stat = NA_real_,
        kpss_trend_p = NA_real_
      )
    )
  }

  adf <- tryCatch(
    suppressWarnings(
      tseries::adf.test(
        x,
        alternative = "stationary"
      )
    ),
    error = function(e) {
      NULL
    }
  )

  pp <- tryCatch(
    suppressWarnings(
      tseries::pp.test(
        x,
        alternative = "stationary"
      )
    ),
    error = function(e) {
      NULL
    }
  )

  kp_l <- tryCatch(
    suppressWarnings(
      tseries::kpss.test(
        x,
        null = "Level"
      )
    ),
    error = function(e) {
      NULL
    }
  )

  kp_t <- tryCatch(
    suppressWarnings(
      tseries::kpss.test(
        x,
        null = "Trend"
      )
    ),
    error = function(e) {
      NULL
    }
  )

  tibble(
    n = length(x),
    adf_stat = if (
      is.null(adf)
    ) {
      NA_real_
    } else {
      as.numeric(
        adf$statistic
      )
    },
    adf_p = if (
      is.null(adf)
    ) {
      NA_real_
    } else {
      as.numeric(
        adf$p.value
      )
    },
    pp_stat = if (
      is.null(pp)
    ) {
      NA_real_
    } else {
      as.numeric(
        pp$statistic
      )
    },
    pp_p = if (
      is.null(pp)
    ) {
      NA_real_
    } else {
      as.numeric(
        pp$p.value
      )
    },
    kpss_level_stat = if (
      is.null(kp_l)
    ) {
      NA_real_
    } else {
      as.numeric(
        kp_l$statistic
      )
    },
    kpss_level_p = if (
      is.null(kp_l)
    ) {
      NA_real_
    } else {
      as.numeric(
        kp_l$p.value
      )
    },
    kpss_trend_stat = if (
      is.null(kp_t)
    ) {
      NA_real_
    } else {
      as.numeric(
        kp_t$statistic
      )
    },
    kpss_trend_p = if (
      is.null(kp_t)
    ) {
      NA_real_
    } else {
      as.numeric(
        kp_t$p.value
      )
    }
  )
}


series_estacionariedad <- c(
  "ICVLF_equal",
  proxies_disponibles
)

tabla_estacionariedad_icvlf_proxies <- map_dfr(
  series_estacionariedad,
  function(nm) {

    x <- base_val[[nm]]

    lvl <- test_estacionariedad_serie(
      x
    ) %>%
      mutate(
        transformacion = "Nivel"
      )

    dx <- diff(x)

    dif <- test_estacionariedad_serie(
      dx
    ) %>%
      mutate(
        transformacion = "Primera diferencia"
      )

    bind_rows(
      lvl,
      dif
    ) %>%
      mutate(
        serie = nm,
        .before = 1
      )
  }
) %>%
  mutate(
    evidencia_estacionaria = case_when(
      is.na(adf_p) |
        is.na(pp_p) |
        is.na(kpss_level_p) ~
        "S/D",
      adf_p < 0.05 &
        pp_p < 0.05 &
        kpss_level_p >= 0.05 ~
        "Convergente: estacionaria",
      adf_p >= 0.05 &
        pp_p >= 0.05 &
        kpss_level_p < 0.05 ~
        "Convergente: no estacionaria",
      TRUE ~
        "Mixta / inconclusa"
    ),
    evidencia_kpss_tendencia = case_when(
      is.na(kpss_trend_p) ~
        "S/D",
      kpss_trend_p >= 0.05 ~
        "No rechaza estacionariedad alrededor de tendencia",
      TRUE ~
        "Rechaza estacionariedad alrededor de tendencia"
    )
  )


tabla_integracion_resumen <- tabla_estacionariedad_icvlf_proxies %>%
  select(
    serie,
    transformacion,
    evidencia_estacionaria
  ) %>%
  pivot_wider(
    names_from = transformacion,
    values_from = evidencia_estacionaria
  ) %>%
  mutate(
    lectura = case_when(
      `Nivel` ==
        "Convergente: estacionaria" ~
        "Compatible con I(0)",
      `Nivel` ==
        "Convergente: no estacionaria" &
        `Primera diferencia` ==
        "Convergente: estacionaria" ~
        "Compatible con I(1)",
      TRUE ~
        "Resultado mixto: interpretar con cautela"
    )
  )


tabla_hac_diferencias <- map_dfr(
  proxies_disponibles,
  function(px) {

    df <- base_val %>%
      select(
        Fecha,
        ICVLF_equal,
        all_of(px)
      ) %>%
      rename(
        proxy = all_of(px)
      ) %>%
      arrange(
        Fecha
      ) %>%
      mutate(
        d_icvlf =
          ICVLF_equal -
          lag(ICVLF_equal),
        d_proxy =
          proxy -
          lag(proxy)
      ) %>%
      filter(
        complete.cases(
          d_icvlf,
          d_proxy
        ),
        is.finite(d_icvlf),
        is.finite(d_proxy)
      )

    if (
      nrow(df) <
        max(
          24,
          lag_hac + 5
        )
    ) {

      return(
        tibble(
          proxy = px,
          n_obs = nrow(df),
          beta_delta = NA_real_,
          se_hac = NA_real_,
          t_hac = NA_real_,
          p_hac = NA_real_,
          r2 = NA_real_
        )
      )
    }

    fit <- lm(
      d_proxy ~ d_icvlf,
      data = df
    )

    V <- sandwich::NeweyWest(
      fit,
      lag = lag_hac,
      prewhite = FALSE,
      adjust = TRUE
    )

    ct <- lmtest::coeftest(
      fit,
      vcov. = V
    )

    tibble(
      proxy = px,
      n_obs = nrow(df),
      beta_delta = unname(
        coef(fit)["d_icvlf"]
      ),
      se_hac = unname(
        ct[
          "d_icvlf",
          "Std. Error"
        ]
      ),
      t_hac = unname(
        ct[
          "d_icvlf",
          "t value"
        ]
      ),
      p_hac = unname(
        ct[
          "d_icvlf",
          "Pr(>|t|)"
        ]
      ),
      r2 = summary(fit)$r.squared
    )
  }
) %>%
  mutate(
    etiq_proxy = etiquetas_proxy[proxy],
    nota = "HAC Newey-West sobre primeras diferencias del ICVLF analítico z; asociación de corto plazo, no causalidad"
  )


winsorizar <- function(
    x,
    probs = winsor_prob
) {

  q <- quantile(
    x,
    probs = probs,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )

  pmin(
    pmax(
      x,
      q[1]
    ),
    q[2]
  )
}


tabla_hac_diferencias_winsor <- map_dfr(
  proxies_disponibles,
  function(px) {

    df <- base_val %>%
      select(
        Fecha,
        ICVLF_equal,
        all_of(px)
      ) %>%
      rename(
        proxy = all_of(px)
      ) %>%
      arrange(
        Fecha
      ) %>%
      mutate(
        d_icvlf =
          ICVLF_equal -
          lag(ICVLF_equal),
        d_proxy =
          proxy -
          lag(proxy)
      ) %>%
      filter(
        complete.cases(
          d_icvlf,
          d_proxy
        ),
        is.finite(d_icvlf),
        is.finite(d_proxy)
      )

    if (
      nrow(df) <
        max(
          24,
          lag_hac + 5
        )
    ) {

      return(
        tibble(
          proxy = px,
          n_obs = nrow(df),
          beta_delta_w = NA_real_,
          se_hac_w = NA_real_,
          t_hac_w = NA_real_,
          p_hac_w = NA_real_,
          r2_w = NA_real_,
          n_cook_gt_4n = NA_integer_,
          max_cook = NA_real_
        )
      )
    }

    fit_base <- lm(
      d_proxy ~ d_icvlf,
      data = df
    )

    cook <- cooks.distance(
      fit_base
    )

    dfw <- df %>%
      mutate(
        d_icvlf_w = winsorizar(
          d_icvlf
        ),
        d_proxy_w = winsorizar(
          d_proxy
        )
      )

    fit <- lm(
      d_proxy_w ~ d_icvlf_w,
      data = dfw
    )

    V <- sandwich::NeweyWest(
      fit,
      lag = lag_hac,
      prewhite = FALSE,
      adjust = TRUE
    )

    ct <- lmtest::coeftest(
      fit,
      vcov. = V
    )

    tibble(
      proxy = px,
      n_obs = nrow(df),
      beta_delta_w = unname(
        coef(fit)["d_icvlf_w"]
      ),
      se_hac_w = unname(
        ct[
          "d_icvlf_w",
          "Std. Error"
        ]
      ),
      t_hac_w = unname(
        ct[
          "d_icvlf_w",
          "t value"
        ]
      ),
      p_hac_w = unname(
        ct[
          "d_icvlf_w",
          "Pr(>|t|)"
        ]
      ),
      r2_w = summary(fit)$r.squared,
      n_cook_gt_4n = sum(
        cook >
          4 / nrow(df),
        na.rm = TRUE
      ),
      max_cook = max(
        cook,
        na.rm = TRUE
      )
    )
  }
) %>%
  mutate(
    etiq_proxy = etiquetas_proxy[proxy],
    nota = "Sensibilidad 1%-99%; no sustituye la estimación principal y no altera los datos originales."
  )


tabla_regimenes <- base_val %>%
  filter(
    !is.na(
      nivel_vulnerabilidad
    )
  ) %>%
  group_by(
    nivel_vulnerabilidad
  ) %>%
  summarise(
    across(
      all_of(proxies_disponibles),
      ~ safe_mean(.),
      .names = "media_{.col}"
    ),
    n = n(),
    .groups = "drop"
  )


tabla_robustez <- cor(
  base_val %>%
    select(
      all_of(indices_icvlf)
    ),
  use = "pairwise.complete.obs"
)


indices_robustez_metodologica <- setdiff(
  indices_icvlf,
  "ICVLF_nucleo_100"
)

tabla_robustez_metodologica <- cor(
  base_val %>%
    select(
      all_of(
        indices_robustez_metodologica
      )
    ),
  use = "pairwise.complete.obs"
)


tabla_benchmark_alcance <- tibble(
  benchmark = "Liquidez + Fondeo (50/50)",
  pearson_vs_icvlf_integral = safe_cor(
    base_val$ICVLF_equal_100,
    base_val$ICVLF_nucleo_100,
    "pearson"
  ),
  spearman_vs_icvlf_integral = safe_cor(
    base_val$ICVLF_equal_100,
    base_val$ICVLF_nucleo_100,
    "spearman"
  ),
  lectura = paste0(
    "Benchmark de constructo estrecho; una correlación menor no es un fallo de robustez, ",
    "porque elimina Activos, Cartera y Solvencia."
  )
)


cat(
  "  Correlaciones globales ICVLF Equal vs proxies:\n"
)

print(
  tabla_correlaciones %>%
    filter(
      indice ==
        "ICVLF_equal_100"
    ) %>%
    select(
      proxy,
      n_obs,
      pearson,
      spearman,
      intensidad
    )
)


# ══════════════════════════════════════════════════════════════
# 15.  ANÁLISIS ROLLING
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 15. Rolling windows ──\n"
)


tabla_cobertura_proxies <- map_dfr(
  proxies_disponibles,
  function(px) {

    x <- proxies[[px]]

    ok <-
      is.finite(x) &
      !is.na(x)

    tibble(
      proxy = px,
      descripcion = etiquetas_proxy[px],
      n = sum(ok),
      cobertura_pct =
        100 *
        mean(ok),
      fecha_inicio = if (
        any(ok)
      ) {
        min(
          proxies$Fecha[ok]
        )
      } else {
        as.Date(NA)
      },
      fecha_fin = if (
        any(ok)
      ) {
        max(
          proxies$Fecha[ok]
        )
      } else {
        as.Date(NA)
      }
    )
  }
)


proxies_rolling_q <- tabla_cobertura_proxies %>%
  filter(
    n >=
      min_obs_proxy_rolling_q
  ) %>%
  pull(
    proxy
  )


proxies_rolling_s <- tabla_cobertura_proxies %>%
  filter(
    n >=
      ventana_rolling_s
  ) %>%
  pull(
    proxy
  )


calcular_rolling <- function(
    df_i,
    df_v,
    prx,
    ventana,
    min_frac = min_frac_rolling
) {

  rc <- tibble(
    Fecha = df_i$Fecha
  )

  for (
    px in prx
  ) {

    pv <- df_v[[px]]
    iv <- df_v$ICVLF_equal

    pares <- zoo::rollapply(
      data.frame(
        x = iv,
        y = pv
      ),
      width = ventana,
      FUN = function(m) {
        sum(
          complete.cases(m)
        )
      },
      by.column = FALSE,
      fill = NA,
      align = "right"
    )

    r <- zoo::rollapply(
      data.frame(
        x = iv,
        y = pv
      ),
      width = ventana,
      FUN = function(m) {

        ok <- complete.cases(m)

        if (
          sum(ok) <
            ceiling(
              ventana *
                min_frac
            )
        ) {
          return(
            NA_real_
          )
        }

        cor(
          m[
            ok,
            "x"
          ],
          m[
            ok,
            "y"
          ]
        )
      },
      by.column = FALSE,
      fill = NA,
      align = "right"
    )

    rc[[
      paste0(
        "n_",
        px
      )
    ]] <- as.integer(
      pares
    )

    rc[[
      paste0(
        "r_",
        px
      )
    ]] <- as.numeric(
      r
    )
  }


  rb <- tibble(
    Fecha = df_i$Fecha
  )

  for (
    bl in bloques_core
  ) {

    rb[[
      paste0(
        "rmean_",
        bl
      )
    ]] <- as.numeric(
      zoo::rollapply(
        df_i[[bl]],
        width = ventana,
        FUN = function(x) {

          if (
            sum(
              is.finite(x)
            ) <
              ceiling(
                ventana *
                  min_frac
              )
          ) {
            return(
              NA_real_
            )
          }

          mean(
            x,
            na.rm = TRUE
          )
        },
        fill = NA,
        align = "right"
      )
    )
  }


  rb$rolling_sd_icvlf <- as.numeric(
    zoo::rollapply(
      df_i$ICVLF_equal,
      width = ventana,
      FUN = function(x) {

        if (
          sum(
            is.finite(x)
          ) <
            ceiling(
              ventana *
                min_frac
            )
        ) {
          return(
            NA_real_
          )
        }

        sd(
          x,
          na.rm = TRUE
        )
      },
      fill = NA,
      align = "right"
    )
  )


  ic <- df_i$ICVLF_equal

  beta_ac <- zoo::rollapply(
    data.frame(
      y = ic[-1],
      x = ic[-length(ic)]
    ),
    width = ventana - 1,
    FUN = function(m) {

      ok <- complete.cases(m)

      if (
        sum(ok) <
          ceiling(
            (ventana - 1) *
              min_frac
          )
      ) {
        return(
          NA_real_
        )
      }

      coef(
        lm(
          y ~ x,
          data = as.data.frame(
            m[
              ok,
              ,
              drop = FALSE
            ]
          )
        )
      )[["x"]]
    },
    by.column = FALSE,
    fill = NA,
    align = "right"
  )

  rb$beta_autocorrelacion <- c(
    NA_real_,
    as.numeric(beta_ac)
  )

  list(
    cor = rc,
    bloques = rb,
    proxies = prx,
    ventana = ventana
  )
}


rolling_q <- calcular_rolling(
  indices,
  base_val,
  proxies_rolling_q,
  ventana_rolling_q
)

rolling_s <- calcular_rolling(
  indices,
  base_val,
  proxies_rolling_s,
  ventana_rolling_s
)


cat(
  "  Proxies rolling 60m:",
  ifelse(
    length(proxies_rolling_q) ==
      0,
    "ninguno",
    paste(
      proxies_rolling_q,
      collapse = ", "
    )
  ),
  "\n"
)

cat(
  "  Proxies rolling 24m:",
  ifelse(
    length(proxies_rolling_s) ==
      0,
    "ninguno",
    paste(
      proxies_rolling_s,
      collapse = ", "
    )
  ),
  "\n"
)

cat(
  "  Regla mínima dentro de ventana:",
  min_frac_rolling * 100,
  "% de pares completos.\n"
)


# ══════════════════════════════════════════════════════════════
# 15B. ROBUSTEZ ESTRUCTURAL: LEAVE-ONE-OUT
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 15B. Robustez leave-one-out ──\n"
)


tabla_robustez_leave1_indicador <- map_dfr(
  vars_indice,
  function(v_omitida) {

    alt <- construir_icvlf_equal_desde_vars(
      setdiff(
        vars_indice,
        v_omitida
      )
    )

    tibble(
      variable_omitida = v_omitida,
      indicador_omitido = etiquetas_var[
        v_omitida
      ],
      bloque = dic_ok$bloque_id[
        match(
          v_omitida,
          dic_ok$var
        )
      ],
      n_comparables = safe_n(
        indices$ICVLF_equal,
        alt
      ),
      pearson_vs_base = safe_cor(
        indices$ICVLF_equal,
        alt,
        "pearson"
      ),
      spearman_vs_base = safe_cor(
        indices$ICVLF_equal,
        alt,
        "spearman"
      ),
      dam_z = mean(
        abs(
          indices$ICVLF_equal -
            alt
        ),
        na.rm = TRUE
      )
    )
  }
) %>%
  arrange(
    pearson_vs_base
  )


tabla_robustez_leave1_dimension <- map_dfr(
  bloques_core,
  function(bl_omitido) {

    bl_keep <- setdiff(
      bloques_core,
      bl_omitido
    )

    alt <- rowMeans(
      subindices[
        ,
        bl_keep
      ],
      na.rm = TRUE
    )

    tibble(
      dimension_omitida = bl_omitido,
      dimension = etiquetas_bloque_corto[
        bl_omitido
      ],
      n_comparables = safe_n(
        indices$ICVLF_equal,
        alt
      ),
      pearson_vs_base = safe_cor(
        indices$ICVLF_equal,
        alt,
        "pearson"
      ),
      spearman_vs_base = safe_cor(
        indices$ICVLF_equal,
        alt,
        "spearman"
      ),
      dam_z = mean(
        abs(
          indices$ICVLF_equal -
            alt
        ),
        na.rm = TRUE
      )
    )
  }
) %>%
  arrange(
    pearson_vs_base
  )


tabla_sensibilidad_pesos_internos <- tibble(
  especificacion = "Pesos internos iguales (principal) vs pesos internos PCA (sensibilidad)",
  pearson = safe_cor(
    indices$ICVLF_equal,
    indices$ICVLF_pca_interno,
    "pearson"
  ),
  spearman = safe_cor(
    indices$ICVLF_equal,
    indices$ICVLF_pca_interno,
    "spearman"
  ),
  dam_z = mean(
    abs(
      indices$ICVLF_equal -
        indices$ICVLF_pca_interno
    ),
    na.rm = TRUE
  )
)


tabla_sensibilidad_normalizacion <- tibble(
  especificacion = "z clásico (principal) vs z robusto mediana/MAD",
  pearson = safe_cor(
    indices$ICVLF_equal,
    indices$ICVLF_robust_z,
    "pearson"
  ),
  spearman = safe_cor(
    indices$ICVLF_equal,
    indices$ICVLF_robust_z,
    "spearman"
  ),
  dam_z = mean(
    abs(
      indices$ICVLF_equal -
        indices$ICVLF_robust_z
    ),
    na.rm = TRUE
  )
)


# Sensibilidad financiera conjunta: se retiran simultáneamente los indicadores
# cuya dirección de riesgo depende más del contexto financiero. Esto prueba que
# la conclusión no descansa en supuestos conceptuales discutibles.
vars_signo_sensible <- auditoria_financiera_signos %>%
  filter(
    sensibilidad_especifica,
    variable %in% vars_indice
  ) %>%
  pull(
    variable
  )


icvlf_sin_signos_sensibles <- construir_icvlf_equal_desde_vars(
  setdiff(
    vars_indice,
    vars_signo_sensible
  )
)


tabla_sensibilidad_signos_financieros <- tibble(
  especificacion = "Excluir simultáneamente indicadores con signo/contexto más discutible",
  variables_excluidas = paste(
    vars_signo_sensible,
    collapse = "; "
  ),
  n_variables_excluidas = length(
    vars_signo_sensible
  ),
  pearson = safe_cor(
    indices$ICVLF_equal,
    icvlf_sin_signos_sensibles,
    "pearson"
  ),
  spearman = safe_cor(
    indices$ICVLF_equal,
    icvlf_sin_signos_sensibles,
    "spearman"
  ),
  dam_z = mean(
    abs(
      indices$ICVLF_equal -
        icvlf_sin_signos_sensibles
    ),
    na.rm = TRUE
  )
)


cat(
  "  Menor correlación leave-one-indicator-out: ",
  round(
    min(
      tabla_robustez_leave1_indicador$pearson_vs_base,
      na.rm = TRUE
    ),
    3
  ),
  "\n",
  sep = ""
)

cat(
  "  Menor correlación leave-one-dimension-out: ",
  round(
    min(
      tabla_robustez_leave1_dimension$pearson_vs_base,
      na.rm = TRUE
    ),
    3
  ),
  "\n",
  sep = ""
)


# ══════════════════════════════════════════════════════════════
# 16.  RUPTURAS ESTRUCTURALES — BAI-PERRON, BIC E IC 95%
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 16. Rupturas estructurales Bai-Perron ──\n"
)

rupturas_df <- tibble(
  numero = integer(),
  fecha = as.Date(
    character()
  ),
  ic95_inf = as.Date(
    character()
  ),
  ic95_sup = as.Date(
    character()
  ),
  ancho_ic_meses_aprox = numeric(),
  ic_degenerado = logical()
)

tabla_bic_rupturas <- tibble(
  n_rupturas = integer(),
  BIC = numeric()
)

auditoria_confint_bai_perron <- list()


tryCatch({

  # Se estima sobre la escala analítica z. Una transformación lineal 0-100 no cambia las fechas,
  # pero z mantiene coherencia con el motor estadístico.
  bp_full <- strucchange::breakpoints(
    ICVLF_equal ~ 1,
    data = indices,
    h = 0.15
  )

  sm_bp <- summary(
    bp_full
  )

  if (
    !is.null(sm_bp$RSS) &&
      "BIC" %in%
        rownames(sm_bp$RSS)
  ) {

    tabla_bic_rupturas <- tibble(
      n_rupturas = suppressWarnings(
        as.integer(
          colnames(sm_bp$RSS)
        )
      ),
      BIC = as.numeric(
        sm_bp$RSS[
          "BIC",
        ]
      )
    ) %>%
      filter(
        is.finite(BIC),
        !is.na(n_rupturas)
      )
  }

  if (
    nrow(tabla_bic_rupturas) ==
      0
  ) {
    stop(
      "No fue posible extraer la trayectoria BIC de breakpoints()."
    )
  }

  m_opt <- tabla_bic_rupturas$n_rupturas[
    which.min(
      tabla_bic_rupturas$BIC
    )
  ]

  bp_opt <- strucchange::breakpoints(
    bp_full,
    breaks = m_opt
  )

  bp_pos <- bp_opt$breakpoints[
    !is.na(
      bp_opt$breakpoints
    )
  ]

  if (
    m_opt >
      0 &&
      length(bp_pos) >
        0
  ) {

    ci_obj <- confint(
      bp_full,
      breaks = m_opt,
      level = 0.95
    )

    auditoria_confint_bai_perron$clase <-
      class(ci_obj)

    auditoria_confint_bai_perron$estructura <-
      capture.output(
        str(ci_obj)
      )

    ci_mat <- if (
      is.matrix(ci_obj)
    ) {

      ci_obj

    } else if (
      !is.null(ci_obj$confint)
    ) {

      as.matrix(
        ci_obj$confint
      )

    } else {

      tryCatch(
        as.matrix(ci_obj),
        error = function(e) {
          NULL
        }
      )
    }

    if (
      is.null(ci_mat) ||
        ncol(ci_mat) <
          3 ||
        nrow(ci_mat) !=
          length(bp_pos)
    ) {

      warning(
        "No se pudo verificar de forma segura la estructura de confint.breakpointsfull. ",
        "Se reportan fechas puntuales y los IC quedan como NA hasta revisar el objeto crudo."
      )

      pos_bp <- bp_pos

      pos_inf <- rep(
        NA_integer_,
        length(bp_pos)
      )

      pos_sup <- rep(
        NA_integer_,
        length(bp_pos)
      )

    } else {

      ci_num <- apply(
        ci_mat[
          ,
          1:3,
          drop = FALSE
        ],
        2,
        as.numeric
      )

      pos_inf <- pmax(
        1L,
        pmin(
          nrow(indices),
          as.integer(
            round(
              ci_num[
                ,
                1
              ]
            )
          )
        )
      )

      pos_bp <- pmax(
        1L,
        pmin(
          nrow(indices),
          as.integer(
            round(
              ci_num[
                ,
                2
              ]
            )
          )
        )
      )

      pos_sup <- pmax(
        1L,
        pmin(
          nrow(indices),
          as.integer(
            round(
              ci_num[
                ,
                3
              ]
            )
          )
        )
      )
    }

    fecha_inf <- ifelse(
      is.na(pos_inf),
      NA,
      as.character(
        indices$Fecha[pos_inf]
      )
    )

    fecha_bp <- as.character(
      indices$Fecha[pos_bp]
    )

    fecha_sup <- ifelse(
      is.na(pos_sup),
      NA,
      as.character(
        indices$Fecha[pos_sup]
      )
    )

    rupturas_df <- tibble(
      numero = seq_along(
        pos_bp
      ),
      fecha = as.Date(
        fecha_bp
      ),
      ic95_inf = as.Date(
        fecha_inf
      ),
      ic95_sup = as.Date(
        fecha_sup
      )
    ) %>%
      mutate(
        ancho_ic_meses_aprox = ifelse(
          is.na(ic95_inf) |
            is.na(ic95_sup),
          NA_real_,
          as.numeric(
            ic95_sup -
              ic95_inf
          ) /
            30.4375
        ),
        ic_degenerado =
          !is.na(
            ancho_ic_meses_aprox
          ) &
          ancho_ic_meses_aprox ==
            0
      )

    if (
      nrow(rupturas_df) >
        0 &&
        all(
          rupturas_df$ic_degenerado,
          na.rm = TRUE
        )
    ) {

      warning(
        "Todos los IC95% Bai-Perron tienen ancho cero. ",
        "No deben presentarse como evidencia de precisión extrema sin inspeccionar auditoria_confint_bai_perron."
      )
    }
  }

  cat(
    "  Número de rupturas BIC mínimo:",
    m_opt,
    "\n"
  )

  cat(
    "  Fechas estimadas:",
    ifelse(
      nrow(rupturas_df) ==
        0,
      "ninguna",
      paste(
        format(
          rupturas_df$fecha,
          "%Y-%m"
        ),
        collapse = ", "
      )
    ),
    "\n"
  )

}, error = function(e) {

  cat(
    "  ⚠ Bai-Perron:",
    conditionMessage(e),
    "\n"
  )
})


# ══════════════════════════════════════════════════════════════
# 17.  ESCENARIOS CONTRAFACTUALES DE ESTRÉS
# ══════════════════════════════════════════════════════════════

cat(
  "\n── 17. Escenarios contrafactuales de estrés ──\n"
)

ultimo <- indices %>%
  slice_tail(
    n = 1
  )

base_ult <- as.numeric(
  unlist(
    ultimo[
      1,
      bloques_core
    ],
    use.names = FALSE
  )
)

names(base_ult) <- bloques_core

base_icvlf_z <- mean(
  base_ult,
  na.rm = TRUE
)


q_adverso <- vapply(
  subindices[bloques_core],
  quantile,
  numeric(1),
  probs = percentil_estres,
  na.rm = TRUE,
  names = FALSE
)


calc_stress_q <- function(
    bloques_afectados
) {

  x <- base_ult

  for (
    bl in bloques_afectados
  ) {

    x[bl] <- max(
      x[bl],
      q_adverso[bl],
      na.rm = TRUE
    )
  }

  mean(
    x,
    na.rm = TRUE
  )
}


escenarios_q <- list(
  E0 = character(0),
  E1 = "liquidez",
  E2 = "fondeo",
  E3 = "activos",
  E4 = "cartera",
  E5 = "solvencia",
  E6 = bloques_core
)


nombres_esc <- c(
  paste0(
    "E0 — Estado observado (",
    format(
      ultimo$Fecha,
      "%B %Y"
    ),
    ")"
  ),
  "E1 — Choque adverso en liquidez",
  "E2 — Choque adverso en fondeo",
  "E3 — Choque adverso en composición de activos",
  "E4 — Choque adverso en presión de cartera",
  "E5 — Choque adverso en solvencia/capacidad de absorción",
  "E6 — Choque adverso conjunto en las cinco dimensiones"
)


tabla_estres <- tibble(
  codigo = names(
    escenarios_q
  ),
  escenario = nombres_esc,
  dimensiones_afectadas = vapply(
    escenarios_q,
    function(x) {

      if (
        length(x) ==
          0
      ) {
        "Ninguna"
      } else {
        paste(
          x,
          collapse = ", "
        )
      }
    },
    character(1)
  ),
  calibracion = c(
    "Estado observado",
    rep(
      paste0(
        "Percentil histórico ",
        percentil_estres * 100,
        " del subíndice"
      ),
      6
    )
  ),
  ICVLF_simulado_z = vapply(
    escenarios_q,
    function(bl) {

      if (
        length(bl) ==
          0
      ) {
        base_icvlf_z
      } else {
        calc_stress_q(
          bl
        )
      }
    },
    numeric(1)
  )
) %>%
  mutate(
    delta_z =
      ICVLF_simulado_z -
      first(
        ICVLF_simulado_z
      ),
    ICVLF_simulado_100_ref =
      escalar_0100_con_referencia(
        ICVLF_simulado_z,
        indices$ICVLF_equal
      ),
    tipo = "Contrafactual Q95"
  )


# Stress histórico observado: conserva la dependencia real entre dimensiones.
pos_peor <- which.max(
  indices$ICVLF_equal
)

vector_peor_hist <- as.numeric(
  unlist(
    indices[
      pos_peor,
      bloques_core
    ],
    use.names = FALSE
  )
)

icvlf_peor_hist <- mean(
  vector_peor_hist,
  na.rm = TRUE
)

tabla_estres_historico <- tibble(
  codigo = "EH",
  escenario = paste0(
    "EH — Peor configuración conjunta observada (",
    format(
      indices$Fecha[pos_peor],
      "%B %Y"
    ),
    ")"
  ),
  dimensiones_afectadas = "Configuración conjunta observada",
  calibracion = "Mes histórico de máximo ICVLF",
  ICVLF_simulado_z = icvlf_peor_hist,
  delta_z =
    icvlf_peor_hist -
    base_icvlf_z,
  ICVLF_simulado_100_ref =
    escalar_0100_con_referencia(
      icvlf_peor_hist,
      indices$ICVLF_equal
    ),
  tipo = "Histórico observado"
)

tabla_estres <- bind_rows(
  tabla_estres,
  tabla_estres_historico
)


# Sensibilidad +1.5 d.e. simétrica para las cinco dimensiones y choque conjunto.
calc_stress_de <- function(
    bloques_afectados,
    shock = shock_sensibilidad_de
) {

  x <- base_ult

  if (
    length(bloques_afectados) >
      0
  ) {

    x[
      bloques_afectados
    ] <- x[
      bloques_afectados
    ] +
      shock
  }

  mean(
    x,
    na.rm = TRUE
  )
}


tabla_estres_sensibilidad_de <- tibble(
  codigo = names(
    escenarios_q
  ),
  escenario = nombres_esc,
  dimensiones_afectadas = vapply(
    escenarios_q,
    function(x) {

      if (
        length(x) ==
          0
      ) {
        "Ninguna"
      } else {
        paste(
          x,
          collapse = ", "
        )
      }
    },
    character(1)
  ),
  calibracion = c(
    "Estado observado",
    rep(
      paste0(
        "+",
        shock_sensibilidad_de,
        " d.e. sobre subíndice orientado"
      ),
      6
    )
  ),
  ICVLF_simulado_z = vapply(
    escenarios_q,
    function(bl) {

      if (
        length(bl) ==
          0
      ) {
        base_icvlf_z
      } else {
        calc_stress_de(
          bl
        )
      }
    },
    numeric(1)
  )
) %>%
  mutate(
    delta_z =
      ICVLF_simulado_z -
      first(
        ICVLF_simulado_z
      ),
    ICVLF_simulado_100_ref =
      escalar_0100_con_referencia(
        ICVLF_simulado_z,
        indices$ICVLF_equal
      )
  )


tabla_calibracion_estres_dim <- tibble(
  dimension = bloques_core,
  dimension_label =
    etiquetas_bloque_corto[
      bloques_core
    ],
  estado_actual_z = as.numeric(
    base_ult[
      bloques_core
    ]
  ),
  q95_historico_z = as.numeric(
    q_adverso[
      bloques_core
    ]
  )
) %>%
  mutate(
    brecha_hasta_q95 = pmax(
      q95_historico_z -
        estado_actual_z,
      0
    ),
    aporte_delta_icvlf_z =
      brecha_hasta_q95 /
      length(bloques_core),
    nota = "La brecha mide distancia al Q95 histórico, no importancia estructural ni probabilidad."
  )


print(
  tabla_estres %>%
    select(
      codigo,
      escenario,
      ICVLF_simulado_z,
      delta_z,
      ICVLF_simulado_100_ref
    )
)


# ══════════════════════════════════════════════════════════════
# 18.  CONTRIBUCIONES POR DIMENSIÓN
# ══════════════════════════════════════════════════════════════

contribuciones <- indices %>%
  select(
    Fecha,
    all_of(bloques_core)
  ) %>%
  mutate(
    across(
      all_of(bloques_core),
      ~ .x /
        length(bloques_core)
    )
  ) %>%
  pivot_longer(
    -Fecha,
    names_to = "bloque",
    values_to = "contribucion"
  ) %>%
  left_join(
    indices_subperiodos %>%
      select(
        Fecha,
        subperiodo
      ),
    by = "Fecha"
  ) %>%
  mutate(
    bloque_label =
      etiquetas_bloque_corto[
        bloque
      ],
    bloque_label = factor(
      bloque_label,
      levels =
        etiquetas_bloque_corto[
          bloques_core
        ]
    )
  )


contrib_subperiodos <- contribuciones %>%
  group_by(
    subperiodo,
    bloque
  ) %>%
  summarise(
    contribucion_media = mean(
      contribucion,
      na.rm = TRUE
    ),
    contribucion_max = max(
      contribucion,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    bloque_label =
      etiquetas_bloque_corto[
        bloque
      ],
    bloque_label = factor(
      bloque_label,
      levels =
        etiquetas_bloque_corto[
          bloques_core
        ]
    )
  )


tabla_contribuciones_control <- contrib_subperiodos %>%
  select(
    subperiodo,
    bloque,
    contribucion_media
  ) %>%
  pivot_wider(
    names_from = bloque,
    values_from = contribucion_media
  ) %>%
  mutate(
    suma_contribuciones = rowSums(
      across(
        all_of(bloques_core)
      )
    ),
    icvlf_medio_z = vapply(
      subperiodo,
      function(sp) {
        mean(
          indices_subperiodos$ICVLF_equal[
            indices_subperiodos$subperiodo ==
              sp
          ],
          na.rm = TRUE
        )
      },
      numeric(1)
    ),
    error_identidad =
      suma_contribuciones -
      icvlf_medio_z
  )


if (
  max(
    abs(
      tabla_contribuciones_control$error_identidad
    ),
    na.rm = TRUE
  ) >
    1e-10
) {

  stop(
    "La descomposición por contribuciones no reproduce exactamente el ICVLF medio."
  )
}


# ══════════════════════════════════════════════════════════════
# 19.  AUDITORÍAS NUMÉRICAS COMPLEMENTARIAS DEL BLOQUE 1
# ══════════════════════════════════════════════════════════════

tabla_extremos_icvlf <- bind_rows(

  indices %>%
    slice_max(
      ICVLF_equal_100,
      n = 10,
      with_ties = FALSE
    ) %>%
    transmute(
      tipo = "Máximos históricos",
      Fecha,
      ICVLF_equal_z = ICVLF_equal,
      ICVLF_equal_100,
      nivel_vulnerabilidad
    ),

  indices %>%
    slice_min(
      ICVLF_equal_100,
      n = 10,
      with_ties = FALSE
    ) %>%
    transmute(
      tipo = "Mínimos históricos",
      Fecha,
      ICVLF_equal_z = ICVLF_equal,
      ICVLF_equal_100,
      nivel_vulnerabilidad
    )
)


resumen_serie_rolling <- function(
    fecha,
    x,
    nombre
) {

  ok <-
    is.finite(x) &
    !is.na(x)

  if (
    !any(ok)
  ) {

    return(
      tibble(
        serie = nombre,
        n = 0,
        media = NA_real_,
        minimo = NA_real_,
        fecha_min = as.Date(NA),
        maximo = NA_real_,
        fecha_max = as.Date(NA),
        ultimo = NA_real_,
        fecha_ultimo = as.Date(NA)
      )
    )
  }

  ix <- which(ok)

  tibble(
    serie = nombre,
    n = length(ix),
    media = mean(
      x[ix]
    ),
    minimo = min(
      x[ix]
    ),
    fecha_min = fecha[
      ix[
        which.min(
          x[ix]
        )
      ]
    ],
    maximo = max(
      x[ix]
    ),
    fecha_max = fecha[
      ix[
        which.max(
          x[ix]
        )
      ]
    ],
    ultimo = x[
      tail(
        ix,
        1
      )
    ],
    fecha_ultimo = fecha[
      tail(
        ix,
        1
      )
    ]
  )
}


tabla_resumen_rolling_cor_60m <- if (
  length(proxies_rolling_q) >
    0
) {

  map_dfr(
    proxies_rolling_q,
    function(px) {

      resumen_serie_rolling(
        rolling_q$cor$Fecha,
        rolling_q$cor[[
          paste0(
            "r_",
            px
          )
        ]],
        paste0(
          "ICVLF vs ",
          px,
          " — 60m"
        )
      )
    }
  )

} else {

  tibble()
}


tabla_resumen_rolling_cor_24m <- if (
  length(proxies_rolling_s) >
    0
) {

  map_dfr(
    proxies_rolling_s,
    function(px) {

      resumen_serie_rolling(
        rolling_s$cor$Fecha,
        rolling_s$cor[[
          paste0(
            "r_",
            px
          )
        ]],
        paste0(
          "ICVLF vs ",
          px,
          " — 24m"
        )
      )
    }
  )

} else {

  tibble()
}


tabla_resumen_autocor_60m <- resumen_serie_rolling(
  rolling_q$bloques$Fecha,
  rolling_q$bloques$beta_autocorrelacion,
  "Beta AR(1) rolling 60 meses"
)


# ══════════════════════════════════════════════════════════════
# 19B. DATOS PÚBLICOS Y OBJETOS ANALÍTICOS PARA REPRODUCCIÓN
# ══════════════════════════════════════════════════════════════

dir_processed <- file.path(
  getwd(),
  "data",
  "processed"
)

dir.create(
  dir_processed,
  recursive = TRUE,
  showWarnings = FALSE
)

# Base analítica con nombres internos reproducibles.
utils::write.csv(
  Base_muestra,
  file = file.path(
    dir_processed,
    "asfi_indicators_2010_2025.csv"
  ),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# Serie del índice, dimensiones y sensibilidades.
utils::write.csv(
  indices,
  file = file.path(
    dir_processed,
    "icvlf_monthly_2010_2025.csv"
  ),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# Diccionario económico y orientación de riesgo.
utils::write.csv(
  tabla_orientacion_riesgo,
  file = file.path(
    dir_processed,
    "indicator_dictionary.csv"
  ),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

cat(
  "✓ Datos públicos exportados en data/processed/.\n"
)


# ══════════════════════════════════════════════════════════════
# 20.  SALIDA MAESTRA: CONSOLA + TXT + RDS PARA BLOQUE 2
# ══════════════════════════════════════════════════════════════

ruta_salida_maestra <- file.path(
  carpeta_salida,
  "logs",
  "SALIDA_MAESTRA_ICVLF_v6_3_BLOQUE1.txt"
)

ruta_objetos <- file.path(
  carpeta_salida,
  "diagnosticos",
  "OBJETOS_ICVLF_v6_3_BLOQUE1.rds"
)


imprimir_seccion <- function(titulo) {

  cat(
    "\n",
    strrep(
      "=",
      110
    ),
    "\n",
    titulo,
    "\n",
    strrep(
      "=",
      110
    ),
    "\n",
    sep = ""
  )
}


imprimir_objeto <- function(
    x,
    digits = 6
) {

  if (
    is.null(x)
  ) {

    cat(
      "NULL\n"
    )

  } else if (
    is.matrix(x)
  ) {

    print(
      round(
        x,
        digits
      )
    )

  } else {

    print(
      x,
      n = Inf,
      width = Inf
    )
  }
}


objetos_bloque1 <- list(

  # ==========================================================
  # DATOS / METADATA
  # ==========================================================

  metadata_ejecucion = metadata_ejecucion,
  diccionario = dic_ok,
  vars_indice = vars_indice,
  bloques_core = bloques_core,
  Base_muestra = Base_muestra,
  datos_risk_muestra = datos_risk_muestra,
  proxies = proxies,
  base_validacion = base_val,
  cobertura_catalogo = cobertura_catalogo,
  inventario_bloques = inventario_bloques,

  # ==========================================================
  # AUDITORÍA
  # ==========================================================

  auditoria_diccionario = auditoria_diccionario,
  auditoria_no_finitos = auditoria_no_finitos,
  auditoria_parseo = auditoria_parseo,
  cobertura_vars = cobertura_vars,
  tabla_marco_dimensiones = tabla_marco_dimensiones,
  tabla_orientacion_riesgo = tabla_orientacion_riesgo,

  # ==========================================================
  # EDA
  # ==========================================================

  estadisticas_ext = estadisticas_ext,
  test_adf_indicadores = test_adf,
  tabla_cor_bloques = tabla_cor_bloques,
  matrices_cor_bloque = matrices_cor_bloque,
  tabla_redundancia = tabla_redundancia,

  # ==========================================================
  # SUBÍNDICES
  # ==========================================================

  tabla_metodos_subindices = tabla_metodos_subindices,
  tabla_pesos_internos = tabla_pesos_internos,
  tabla_pesos_jerarquicos = tabla_pesos_jerarquicos,
  auditoria_reponderacion_interna = auditoria_reponderacion_interna,
  auditoria_subindices = auditoria_subindices,
  tabla_pesos_pca_internos = tabla_pesos_pca_internos,
  tabla_pesos_pca_bloques = tabla_pesos_pca_bloques,
  subindices = subindices,
  subindices_brutos = subindices_brutos,

  # ==========================================================
  # ICVLF
  # ==========================================================

  indices = indices,
  tabla_subperiodos = tabla_subperiodos,
  tabla_extremos_icvlf = tabla_extremos_icvlf,

  # ==========================================================
  # VALIDACIÓN
  # ==========================================================

  tabla_cobertura_proxies = tabla_cobertura_proxies,
  tabla_correlaciones = tabla_correlaciones,
  tabla_validacion_sin_solapamiento = tabla_validacion_sin_solapamiento,
  tabla_correlaciones_subperiodo = tabla_correlaciones_subperiodo,
  tabla_rezagos = tabla_rezagos,
  tabla_estacionariedad_icvlf_proxies = tabla_estacionariedad_icvlf_proxies,
  tabla_integracion_resumen = tabla_integracion_resumen,
  tabla_hac_consistencia = tabla_hac_consistencia,
  tabla_hac_diferencias = tabla_hac_diferencias,
  tabla_hac_diferencias_winsor = tabla_hac_diferencias_winsor,

  # ==========================================================
  # ROBUSTEZ
  # ==========================================================

  tabla_robustez = tabla_robustez,
  tabla_robustez_metodologica = tabla_robustez_metodologica,
  tabla_benchmark_alcance = tabla_benchmark_alcance,
  tabla_robustez_leave1_indicador = tabla_robustez_leave1_indicador,
  tabla_robustez_leave1_dimension = tabla_robustez_leave1_dimension,
  tabla_sensibilidad_pesos_internos = tabla_sensibilidad_pesos_internos,
  tabla_sensibilidad_normalizacion = tabla_sensibilidad_normalizacion,
  tabla_sensibilidad_signos_financieros = tabla_sensibilidad_signos_financieros,

  # ==========================================================
  # ROLLING
  # ==========================================================

  rolling_q = rolling_q,
  rolling_s = rolling_s,
  tabla_resumen_rolling_cor_60m = tabla_resumen_rolling_cor_60m,
  tabla_resumen_rolling_cor_24m = tabla_resumen_rolling_cor_24m,
  tabla_resumen_autocor_60m = tabla_resumen_autocor_60m,

  # ==========================================================
  # RUPTURAS
  # ==========================================================

  tabla_bic_rupturas = tabla_bic_rupturas,
  rupturas_df = rupturas_df,
  auditoria_confint_bai_perron = auditoria_confint_bai_perron,

  # ==========================================================
  # ESTRÉS
  # ==========================================================

  tabla_calibracion_estres_dim = tabla_calibracion_estres_dim,
  tabla_estres = tabla_estres,
  tabla_estres_sensibilidad_de = tabla_estres_sensibilidad_de,

  # ==========================================================
  # CONTRIBUCIONES
  # ==========================================================

  contribuciones = contribuciones,
  contrib_subperiodos = contrib_subperiodos,
  tabla_contribuciones_control = tabla_contribuciones_control
)


saveRDS(
  objetos_bloque1,
  ruta_objetos
)


con_salida <- file(
  ruta_salida_maestra,
  open = "wt",
  encoding = "UTF-8"
)

sink(
  con_salida,
  split = TRUE
)

old_width <- getOption(
  "width"
)

options(
  width = 240
)


tryCatch({

  imprimir_seccion(
    "ICVLF v6.3 — BLOQUE 1 METODOLÓGICO"
  )

  cat(
    "Fecha/hora de ejecución:",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),
    "\n"
  )

  cat(
    "Archivo base:",
    normalizePath(
      ruta,
      winslash = "/",
      mustWork = TRUE
    ),
    "\n"
  )

  cat(
    "MD5:",
    unname(
      tools::md5sum(ruta)
    ),
    "\n"
  )

  cat(
    "Interpretación: índice retrospectivo de vulnerabilidad histórica relativa.\n"
  )

  cat(
    "No es LCR/NSFR regulatorio, probabilidad de crisis, pronóstico ni estimación causal.\n"
  )


  imprimir_seccion(
    "A. METADATA, INTEGRIDAD TEMPORAL Y COBERTURA"
  )

  imprimir_objeto(
    metadata_ejecucion
  )

  imprimir_objeto(
    auditoria_diccionario
  )

  imprimir_objeto(
    auditoria_no_finitos
  )

  cat(
    "\nAuditoría de parseo numérico:\n"
  )

  imprimir_objeto(
    auditoria_parseo
  )

  imprimir_objeto(
    cobertura_vars
  )


  imprimir_seccion(
    "B. FUNDAMENTO FINANCIERO Y ORIENTACIÓN EX ANTE"
  )

  imprimir_objeto(
    tabla_marco_dimensiones
  )

  cat(
    "\nOrientación ex ante por indicador:\n"
  )

  imprimir_objeto(
    tabla_orientacion_riesgo %>%
      select(
        bloque,
        variable,
        indicador,
        direccion,
        fortaleza_signo,
        sensibilidad_especifica,
        fundamento_financiero
      )
  )


  imprimir_seccion(
    "C. DESCRIPTIVOS Y DISTRIBUCIÓN"
  )

  imprimir_objeto(
    estadisticas_ext %>%
      select(
        bloque_id,
        variable,
        etiqueta,
        n,
        n_na,
        media,
        sd,
        cv_pct,
        min,
        p5,
        p25,
        mediana,
        p75,
        p95,
        max,
        sesgo,
        curtosis,
        jb_pval,
        normalidad_jb
      )
  )

  imprimir_objeto(
    test_adf
  )


  imprimir_seccion(
    "D. CORRELACIONES EDA POR DIMENSIÓN — PEARSON, SPEARMAN Y N POR PAR"
  )

  imprimir_objeto(
    tabla_cor_bloques
  )


  imprimir_seccion(
    "E. NORMALIZACIÓN Y AUDITORÍA DE SIGNOS"
  )

  imprimir_objeto(
    auditoria_subindices
  )

  cat(
    "\nReponderación interna causada por faltantes (si existe):\n"
  )

  imprimir_objeto(
    auditoria_reponderacion_interna
  )


  imprimir_seccion(
    "F. PCA COMO DIAGNÓSTICO/SENSIBILIDAD"
  )

  imprimir_objeto(
    tabla_metodos_subindices
  )

  cat(
    "\nPesos PCA internos:\n"
  )

  imprimir_objeto(
    tabla_pesos_pca_internos
  )

  cat(
    "\nRedundancia |r|>=0.90:\n"
  )

  imprimir_objeto(
    tabla_redundancia
  )


  imprimir_seccion(
    "G. PONDERACIÓN JERÁRQUICA Y COEFICIENTES ANALÍTICOS"
  )

  imprimir_objeto(
    tabla_pesos_jerarquicos
  )

  cat(
    "\nIMPORTANTE: peso jerárquico nominal != coeficiente final por indicador debido a la reestandarización de cada dimensión.\n"
  )


  imprimir_seccion(
    "H. ICVLF PRINCIPAL, SUBÍNDICES Y SUBPERIODOS"
  )

  imprimir_objeto(
    indices %>%
      select(
        Fecha,
        all_of(bloques_core),
        ICVLF_equal,
        ICVLF_equal_100,
        nivel_vulnerabilidad
      )
  )

  imprimir_objeto(
    tabla_subperiodos
  )

  imprimir_objeto(
    tabla_extremos_icvlf
  )

  cat(
    "\nCorrelación entre subíndices:\n"
  )

  print(
    round(
      cor_subindices,
      4
    )
  )


  imprimir_seccion(
    "I. PROXIES Y CONSISTENCIA CONVERGENTE"
  )

  imprimir_objeto(
    tabla_cobertura_proxies
  )

  imprimir_objeto(
    tabla_correlaciones %>%
      filter(
        indice ==
          "ICVLF_equal_100"
      ) %>%
      select(
        proxy,
        n_obs,
        pearson,
        spearman,
        intensidad
      )
  )

  cat(
    "\nValidación sin solapamiento mecánico P1/P2:\n"
  )

  imprimir_objeto(
    tabla_validacion_sin_solapamiento
  )

  cat(
    "\nHeterogeneidad por subperiodo:\n"
  )

  imprimir_objeto(
    tabla_correlaciones_subperiodo
  )

  cat(
    "\nAsociaciones adelantadas descriptivas (NO predictivas):\n"
  )

  imprimir_objeto(
    tabla_rezagos
  )


  imprimir_seccion(
    "J. ESTACIONARIEDAD E INFERENCIA HAC"
  )

  imprimir_objeto(
    tabla_estacionariedad_icvlf_proxies
  )

  imprimir_objeto(
    tabla_integracion_resumen
  )

  cat(
    "\nHAC en niveles (complementario):\n"
  )

  imprimir_objeto(
    tabla_hac_consistencia
  )

  cat(
    "\nHAC en primeras diferencias del ICVLF z (principal inferencial de corto plazo):\n"
  )

  imprimir_objeto(
    tabla_hac_diferencias
  )

  cat(
    "\nSensibilidad a extremos 1%-99% + diagnóstico Cook:\n"
  )

  imprimir_objeto(
    tabla_hac_diferencias_winsor
  )


  imprimir_seccion(
    "K. ROBUSTEZ METODOLÓGICA Y BENCHMARK DE ALCANCE"
  )

  cat(
    "Matriz de sensibilidades metodológicas (sin benchmark estrecho):\n"
  )

  print(
    round(
      tabla_robustez_metodologica,
      4
    )
  )

  cat(
    "\nBenchmark de alcance Liquidez+Fondeo:\n"
  )

  imprimir_objeto(
    tabla_benchmark_alcance
  )

  cat(
    "\nLeave-one-indicator-out:\n"
  )

  imprimir_objeto(
    tabla_robustez_leave1_indicador
  )

  cat(
    "\nLeave-one-dimension-out:\n"
  )

  imprimir_objeto(
    tabla_robustez_leave1_dimension
  )

  imprimir_objeto(
    tabla_sensibilidad_pesos_internos
  )

  imprimir_objeto(
    tabla_sensibilidad_normalizacion
  )

  imprimir_objeto(
    tabla_sensibilidad_signos_financieros
  )


  imprimir_seccion(
    "L. ROLLING — COBERTURA, 60M PRINCIPAL Y 24M SENSIBILIDAD"
  )

  cat(
    "Proxies 60m admitidos:",
    paste(
      proxies_rolling_q,
      collapse = ", "
    ),
    "\n"
  )

  cat(
    "Proxies 24m admitidos:",
    paste(
      proxies_rolling_s,
      collapse = ", "
    ),
    "\n"
  )

  imprimir_objeto(
    tabla_resumen_rolling_cor_60m
  )

  imprimir_objeto(
    tabla_resumen_rolling_cor_24m
  )

  imprimir_objeto(
    tabla_resumen_autocor_60m
  )


  imprimir_seccion(
    "M. RUPTURAS ESTRUCTURALES BAI-PERRON"
  )

  imprimir_objeto(
    tabla_bic_rupturas
  )

  imprimir_objeto(
    rupturas_df
  )

  cat(
    "\nEstructura cruda de confint() para auditoría:\n"
  )

  print(
    auditoria_confint_bai_perron
  )


  imprimir_seccion(
    "N. ESTRÉS CONTRAFACTUAL"
  )

  cat(
    "Calibración dimensional actual -> Q95:\n"
  )

  imprimir_objeto(
    tabla_calibracion_estres_dim
  )

  cat(
    "\nEscenarios Q95 + peor configuración histórica:\n"
  )

  imprimir_objeto(
    tabla_estres
  )

  cat(
    "\nSensibilidad +1.5 d.e.:\n"
  )

  imprimir_objeto(
    tabla_estres_sensibilidad_de
  )


  imprimir_seccion(
    "O. CONTRIBUCIONES E IDENTIDAD CONTABLE DEL ÍNDICE"
  )

  imprimir_objeto(
    contrib_subperiodos
  )

  imprimir_objeto(
    tabla_contribuciones_control
  )


  imprimir_seccion(
    "P. SESSION INFO"
  )

  print(
    sessionInfo()
  )


  imprimir_seccion(
    "FIN BLOQUE 1"
  )

  cat(
    "TXT maestro:",
    ruta_salida_maestra,
    "\n"
  )

  cat(
    "RDS de objetos para Bloque 2:",
    ruta_objetos,
    "\n"
  )

}, finally = {

  options(
    width = old_width
  )

  sink()

  close(
    con_salida
  )
})


cat(
  "\n✓ BLOQUE 1 COMPLETADO.\n"
)

cat(
  "  Salida maestra:",
  ruta_salida_maestra,
  "\n"
)

cat(
  "  Objetos para Bloque 2:",
  ruta_objetos,
  "\n"
)

cat(
  "  Este script NO genera gráficos ni tablas publicables.\n"
)