# BLOQUE 1 METODOLÓGICO — ICVLF v6.3
# Índice Compuesto de Vulnerabilidad de Liquidez y Fondeo
# Motor analítico, auditoría y salidas completas en consola/TXT
# BLOQUE 2 (tablas, gráficos y anexos publicables) se construye por separado
# Sistema Bancario Boliviano · Enero 2010 – Diciembre 2025
# ============================================================
# PRINCIPIOS DE ESTA VERSIÓN
#   1. La teoría financiera define primero el constructo, las cinco dimensiones
#      y la dirección económica de cada indicador. La estadística NO decide el signo.
#   2. El modelo PRINCIPAL usa pesos iguales dentro de cada dimensión y 20% entre
#      dimensiones. Es una decisión de transparencia y neutralidad ex ante.
#   3. El PCA se conserva como sensibilidad empírica, no como fuente de verdad
#      económica. Se reportan KMO, Bartlett, varianza PC1 y coherencia de cargas.
#   4. Se audita redundancia/correlación para detectar doble conteo informativo.
#   5. Se incorpora normalización robusta mediana/MAD como sensibilidad a outliers.
#   6. La escala z es analítica; 0–100 es solo comunicacional y retrospectiva.
#   7. Los cuartiles son niveles históricos relativos: Baja, Moderada, Alta y Muy alta.
#      No son umbrales regulatorios ni probabilidades de crisis.
#   8. La consistencia con proxies es convergente. P1/P2 se contrastan también
#      contra un ICVLF reconstruido sin el indicador solapado para reducir circularidad.
#   9. ADF, PP y KPSS se aplican a ICVLF/proxies; HAC en primeras diferencias se
#      usa como sensibilidad inferencial cuando los niveles pueden ser no estacionarios.
#  10. Leave-one-indicator, leave-one-dimension y cambios de ponderación/normalización
#      documentan robustez conforme a buenas prácticas de índices compuestos.
#  11. Bai-Perron se reporta con partición BIC e intervalos de confianza; no causalidad.
#  12. El estrés es contrafactual determinístico: shocks Q95 por cada dimensión,
#      choque conjunto y peor configuración histórica observada.
#  13. Solvencia y cartera se interpretan como canales de transmisión/absorción que
#      condicionan la vulnerabilidad de liquidez; no como sustitutos de LCR/NSFR.
#  14. Toda conclusión empírica debe derivarse de la ejecución; no se hardcodean
#      explicaciones económicas en gráficos ni captions.
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

# Ruta de datos: prioridad 1) variable de entorno; 2) directorio de trabajo;
# 3) ruta histórica del equipo utilizado en el proyecto.
ruta_candidatas <- c(
  Sys.getenv("ICVLF_DATA_PATH", unset = ""),
  file.path(getwd(), "Datos RL SB.xlsm"),
  "C:\\Users\\USER\\OneDrive\\Desktop\\Tesis\\Datos RL SB.xlsm"
)
ruta_candidatas <- unique(ruta_candidatas[nzchar(ruta_candidatas)])
ruta_existente <- ruta_candidatas[file.exists(ruta_candidatas)]
if (length(ruta_existente) == 0) {
  stop(
    "No se encontró 'Datos RL SB.xlsm'. Defina ICVLF_DATA_PATH o coloque la base ",
    "en el directorio de trabajo."
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
umbral_var_pc1           <- 0.50  # criterio operativo de interpretabilidad, no regla universal
umbral_coherencia_pc1    <- 0.75  # proporción mínima de cargas orientadas con signo común
n_min_frac_subindice    <- 0.80  # principal: al menos 80% de indicadores de la dimensión
min_pares_validacion    <- 5
min_frac_rolling        <- 0.80  # al menos 80% de pares dentro de cada ventana
min_obs_proxy_rolling_q <- 60    # evita llamar 'quinquenal' a una proxy con <60 observaciones totales
lag_hac                 <- 12
ventana_rolling_q       <- 60
ventana_rolling_s       <- 24
percentil_estres        <- 0.95
shock_sensibilidad_de   <- 1.50
winsor_prob             <- c(0.01, 0.99)  # solo sensibilidad econométrica, nunca reemplaza la muestra principal

muestra_str   <- "Enero 2010 – Diciembre 2025"
muestra_short <- "2010–2025"
unidad_analisis_texto <- paste0(
  "Indicadores del sistema bancario reportados para la categoría agregada ",
  "'Todos' en la fuente utilizada"
)

fuente_asfi   <- paste0(
  "Fuente: Elaboración propia con indicadores publicados por ASFI ",
  "(Autoridad de Supervisión del Sistema Financiero de Bolivia)."
)
fuente_propia <- "Fuente: Elaboración propia."

# Directorio de salida: relativo al directorio de trabajo por defecto.
carpeta_salida <- file.path(getwd(), "ICVLF_v6_3_Bloque1_Metodologico")
for (sub in c("logs", "diagnosticos")) {
  dir.create(file.path(carpeta_salida, sub), showWarnings = FALSE, recursive = TRUE)
}

cat("✓ Parámetros definidos | Base:", normalizePath(ruta, winslash = "/", mustWork = TRUE), "\n")
cat("✓ Muestra objetivo:", muestra_str, "\n")
cat("✓ Bloque 1: motor metodológico + auditoría + salidas impresas; sin gráficos/tablas publicables.\n")
cat("✓ Lectura del índice: vulnerabilidad histórica relativa; no LCR/NSFR regulatorio, no probabilidad de crisis, no causalidad.\n")


# ══════════════════════════════════════════════════════════════
# 2.  FUNCIONES AUXILIARES
# ══════════════════════════════════════════════════════════════

normalizar_0100 <- function(x) {
  ok <- is.finite(x) & !is.na(x)
  if (!any(ok)) return(rep(NA_real_, length(x)))
  mn <- min(x[ok]); mx <- max(x[ok])
  if (isTRUE(all.equal(mx, mn))) return(ifelse(ok, 50, NA_real_))
  out <- rep(NA_real_, length(x))
  out[ok] <- 100 * (x[ok] - mn) / (mx - mn)
  out
}

# Aplica a nuevos valores la misma transformación 0–100 obtenida de una
# referencia histórica. Permite valores <0 o >100 si el escenario excede
# el rango histórico; no se trunca deliberadamente.
escalar_0100_con_referencia <- function(x, ref) {
  ref_ok <- ref[is.finite(ref) & !is.na(ref)]
  if (length(ref_ok) == 0) return(rep(NA_real_, length(x)))
  mn <- min(ref_ok); mx <- max(ref_ok)
  if (isTRUE(all.equal(mx, mn))) return(rep(50, length(x)))
  100 * (x - mn) / (mx - mn)
}

parse_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  
  x_chr <- stringr::str_squish(as.character(x))
  x_chr[x_chr %in% c("", "NA", "N/A", "-", "--")] <- NA_character_
  
  parse_one <- function(s) {
    if (is.na(s)) return(NA_real_)
    s <- gsub("\\s+", "", s)
    
    tiene_coma <- grepl(",", s, fixed = TRUE)
    tiene_punto <- grepl(".", s, fixed = TRUE)
    
    if (tiene_coma && tiene_punto) {
      pos_coma <- max(gregexpr(",", s, fixed = TRUE)[[1]])
      pos_punto <- max(gregexpr(".", s, fixed = TRUE)[[1]])
      if (pos_coma > pos_punto) {
        return(suppressWarnings(readr::parse_number(
          s, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
        )))
      } else {
        return(suppressWarnings(readr::parse_number(
          s, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
        )))
      }
    }
    
    if (tiene_coma) {
      return(suppressWarnings(readr::parse_number(
        s, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
      )))
    }
    
    suppressWarnings(readr::parse_number(
      s, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    ))
  }
  
  vapply(x_chr, parse_one, numeric(1))
}

mes_a_numero <- function(m) {
  meses <- c(
    Enero=1, Febrero=2, Marzo=3, Abril=4, Mayo=5, Junio=6,
    Julio=7, Agosto=8, Septiembre=9, Octubre=10, Noviembre=11, Diciembre=12
  )
  unname(meses[stringr::str_squish(as.character(m))])
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_cv <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(m) || abs(m) < .Machine$double.eps^0.5) return(NA_real_)
  s / abs(m) * 100
}

# Escala robusta de referencia. Se usa solo como sensibilidad a outliers.
# MAD usa el factor 1.4826 para consistencia bajo normalidad. Si MAD=0,
# se utiliza IQR/1.349 y, como último respaldo, la desviación estándar.
robust_scale_ref <- function(x_ref) {
  med <- median(x_ref, na.rm = TRUE)
  sc  <- stats::mad(x_ref, center = med, constant = 1.4826, na.rm = TRUE)
  if (!is.finite(sc) || sc <= 0) {
    sc <- IQR(x_ref, na.rm = TRUE) / 1.349
  }
  if (!is.finite(sc) || sc <= 0) {
    sc <- sd(x_ref, na.rm = TRUE)
  }
  if (!is.finite(sc) || sc <= 0) sc <- 1
  c(center = med, scale = sc)
}

intensidad_cor <- function(r) {
  dplyr::case_when(
    is.na(r)          ~ "S/D",
    abs(r) >= 0.70    ~ "Alta (|r| ≥ 0.70)",
    abs(r) >= 0.40    ~ "Moderada (0.40 ≤ |r| < 0.70)",
    TRUE              ~ "Baja (|r| < 0.40)"
  )
}

moda_factor <- function(x) {
  tx <- table(x, useNA = "no")
  if (length(tx) == 0) return(NA_character_)
  names(tx)[which.max(tx)]
}

validar_pesos <- function(w, cols, tol = 1e-10) {
  if (is.null(names(w))) stop("El vector de pesos debe estar nombrado.")
  if (!setequal(names(w), cols)) {
    stop("Los nombres de los pesos no coinciden con las columnas del bloque.")
  }
  w <- w[cols]
  if (any(!is.finite(w)) || any(w < 0)) stop("Pesos internos inválidos.")
  if (abs(sum(w) - 1) > tol) stop("Los pesos internos no suman 1.")
  w
}

suma_ponderada_robusto <- function(Xmat, w, n_min_frac = 0.5) {
  Xmat <- as.matrix(Xmat)
  if (is.null(colnames(Xmat))) stop("Xmat debe tener nombres de columnas.")
  w <- validar_pesos(w, colnames(Xmat))
  na_mask <- !is.na(Xmat) & is.finite(Xmat)
  Xsafe <- Xmat
  Xsafe[!na_mask] <- NA_real_
  num <- rowSums(sweep(Xsafe, 2, w, "*"), na.rm = TRUE)
  den <- rowSums(sweep(na_mask, 2, w, "*"), na.rm = TRUE)
  res <- num / den
  res[rowSums(na_mask) / ncol(Xmat) < n_min_frac] <- NA_real_
  res[den <= 0] <- NA_real_
  res
}

safe_cor <- function(x, y, method = "pearson", min_pares = min_pares_validacion) {
  idx <- complete.cases(x, y) & is.finite(x) & is.finite(y)
  if (sum(idx) < min_pares) return(NA_real_)
  suppressWarnings(tryCatch(
    cor(x[idx], y[idx], method = method),
    error = function(e) NA_real_
  ))
}

safe_n <- function(x, y) {
  sum(complete.cases(x, y) & is.finite(x) & is.finite(y))
}

# Regímenes relativos respecto a la distribución histórica completa.
clasificar_cuartiles <- function(x) {
  q <- quantile(x, c(.25, .50, .75), na.rm = TRUE, names = FALSE, type = 7)
  if (length(unique(q)) < 3) {
    warning("Cuartiles con empates; se usa ntile(4) como respaldo descriptivo.")
    nt <- dplyr::ntile(x, 4)
    return(factor(c("Baja", "Moderada", "Alta", "Muy alta")[nt],
                  levels = c("Baja", "Moderada", "Alta", "Muy alta")))
  }
  cut(
    x,
    breaks = c(-Inf, q, Inf),
    include.lowest = TRUE,
    labels = c("Baja", "Moderada", "Alta", "Muy alta"),
    ordered_result = TRUE
  )
}

# Nota de arquitectura: funciones de visualización, estilos y exportación publicable
# se reservan para el BLOQUE 2. El BLOQUE 1 no genera figuras ni HTML.

# ══════════════════════════════════════════════════════════════
# 3.  CARGAR BASE Y AUDITAR ESTRUCTURA TEMPORAL
# ══════════════════════════════════════════════════════════════

cat("\n── 3. Cargando y auditando base de datos ──\n")

Base_raw <- readxl::read_excel(path = ruta, sheet = hoja, skip = skip_filas)
names(Base_raw) <- stringr::str_squish(names(Base_raw))

if (ncol(Base_raw) < 3) stop("La hoja no contiene la estructura esperada.")

Base_raw <- Base_raw %>%
  rename(Anio = 1, Mes_nombre = 2) %>%
  mutate(
    anio_num = as.integer(parse_num(as.character(Anio))),
    mes_num  = as.integer(mes_a_numero(Mes_nombre)),
    Fecha    = as.Date(sprintf("%04d-%02d-01", anio_num, mes_num))
  ) %>%
  filter(!is.na(Fecha)) %>%
  arrange(Fecha)

if (anyDuplicated(Base_raw$Fecha) > 0) {
  dup <- unique(Base_raw$Fecha[duplicated(Base_raw$Fecha)])
  stop("Existen fechas mensuales duplicadas: ", paste(dup, collapse = ", "))
}

fechas_esperadas <- seq(fecha_inicio_muestra, fecha_fin_muestra, by = "month")
fechas_observadas <- Base_raw %>%
  filter(Fecha >= fecha_inicio_muestra, Fecha <= fecha_fin_muestra) %>%
  pull(Fecha)
fechas_faltantes <- setdiff(fechas_esperadas, fechas_observadas)
fechas_extra     <- setdiff(fechas_observadas, fechas_esperadas)

if (length(fechas_faltantes) > 0) {
  stop("Faltan meses dentro de la muestra 2010–2025: ",
       paste(format(fechas_faltantes, "%Y-%m"), collapse = ", "))
}
if (length(fechas_extra) > 0) {
  warning("Se detectaron fechas fuera de la secuencia mensual esperada.")
}

cat("  Base completa:", format(min(Base_raw$Fecha), "%b %Y"),
    "→", format(max(Base_raw$Fecha), "%b %Y"),
    "(", nrow(Base_raw), "obs.)\n")
cat("  Muestra objetivo: ", length(fechas_esperadas), " meses consecutivos.\n", sep = "")

# ══════════════════════════════════════════════════════════════
# 4.  DICCIONARIO METODOLÓGICO
# ══════════════════════════════════════════════════════════════

dic <- tibble::tribble(
  ~original, ~var, ~bloque_id, ~bloque, ~direccion_riesgo, ~entra_indice, ~entra_ipfe,
  
  "Disponibilidades/Oblig.a Corto Plazo",
  "liq_disp_oblig_cp","liquidez","Liquidez inmediata",-1,TRUE,FALSE,
  
  "Disponib.+Inv.Temp./Oblig.a Corto Plazo",
  "liq_disp_inv_oblig_cp","liquidez","Liquidez inmediata",-1,TRUE,FALSE,
  
  "Disponib.+Inv.Temp./Pasivo",
  "liq_disp_inv_pasivo","liquidez","Liquidez inmediata",-1,TRUE,FALSE,
  
  "Disponibilidades+Inv.Temporarias/Activo",
  "liq_disp_inv_activo","liquidez","Liquidez inmediata",-1,TRUE,FALSE,
  
  "Activos Liquidos/pasivos de corto plazo",
  "liq_activos_pasivos_cp","liquidez","Liquidez inmediata",-1,FALSE,FALSE,
  
  "Oblig.con el Público y con Empresas Públicas/Pasivo+Patrimonio",
  "fon_oblig_publico_emp_paspat","fondeo","Estructura de fondeo",1,TRUE,FALSE,
  
  "Oblig.con el Público/Pasivo+Patrimonio",
  "fon_oblig_publico_paspat","fondeo","Estructura de fondeo",1,TRUE,FALSE,
  
  "Oblig.con Bancos y Ent. Fin./Pasivo+Patrimonio",
  "fon_oblig_bancos_paspat","fondeo","Estructura de fondeo",1,TRUE,TRUE,
  
  "Obligaciones Subordinadas/Pasivo+Patrimonio",
  "fon_oblig_subordinadas_paspat","fondeo","Estructura de fondeo",-1,TRUE,TRUE,
  
  "Oblig. Pers. Jurídicas e Institucionales /Total Oblig. Publico",
  "fon_juridicas_institucionales","fondeo","Estructura de fondeo",1,TRUE,TRUE,
  
  "Oblig. Personas. Naturales /Total Oblig. Publico",
  "fon_personas_naturales","fondeo","Estructura de fondeo",-1,TRUE,TRUE,
  
  "Días de permanencia de los depósitos a plazo fijo",
  "fon_dias_permanencia_dpf","fondeo","Estructura de fondeo",-1,TRUE,TRUE,
  
  "Disponibilidades / Activos",
  "act_disp_activo","activos","Estructura de activos",-1,TRUE,FALSE,
  
  "Cartera Neta / Activo",
  "act_cartera_neta_activo","activos","Estructura de activos",1,TRUE,FALSE,
  
  "Activo Productivo/Activo+Contingente",
  "act_productivo_actcont","activos","Estructura de activos",-1,TRUE,FALSE,
  
  "Activo Improductivo/Patrimonio",
  "act_improductivo_patrimonio","activos","Estructura de activos",1,TRUE,FALSE,
  
  "Cartera Reprogramada o Reestructurada/ Cartera",
  "car_reprogramada_cartera","cartera","Presión de cartera",1,TRUE,FALSE,
  
  "Cartera Vencida Total+Ejecución Total /Cartera",
  "car_mora_cartera","cartera","Presión de cartera",1,TRUE,FALSE,
  
  "Cartera Reprog. o Reestruct. Vencida y Ejec./ Cartera Reprog. o Reestruct. Total",
  "car_reprog_venc_ejec","cartera","Presión de cartera",1,TRUE,FALSE,
  
  "Prev.Cartera Incobrable/Cartera",
  "car_prev_incobrable","cartera","Presión de cartera",1,TRUE,FALSE,
  
  "F Cartera con Requerimiento de Previsión del 100%",
  "car_categoria_f","cartera","Presión de cartera",1,TRUE,FALSE,
  
  "Patrimonio/Activo",
  "solv_patrimonio_activo","solvencia","Solvencia y absorción",-1,TRUE,TRUE,
  
  "Patrimonio/Activo+Contingente",
  "solv_patrimonio_actcont","solvencia","Solvencia y absorción",-1,TRUE,FALSE,
  
  "Coeficiente de Adecuación Patrimonial",
  "solv_cap","solvencia","Solvencia y absorción",-1,TRUE,FALSE,
  
  "Cartera Vencida Total + Ejecucion Total / Patrimonio",
  "solv_mora_patrimonio","solvencia","Solvencia y absorción",1,TRUE,FALSE,
  
  "Cartera Vencida Total + Ejecución Total - Prev/Patrimonio",
  "solv_mora_neta_patrimonio","solvencia","Solvencia y absorción",1,TRUE,FALSE,
  
  "Result.Neto de la Gestión/(Activo+Contingente) (ROA)",
  "rent_roa","rentabilidad","Rentabilidad",-1,FALSE,FALSE,
  
  "Result.Neto de la Gestión/Patrimonio (ROE)",
  "rent_roe","rentabilidad","Rentabilidad",-1,FALSE,FALSE,
  
  "Resultado de operación después de Incobrables /(Activo + Contingente)",
  "rent_resultado_operativo","rentabilidad","Rentabilidad",-1,FALSE,FALSE,
  
  "Gastos Financieros/Pasivos con costo promedio",
  "rent_gastos_fin_pasivos_costo","rentabilidad","Rentabilidad",1,FALSE,FALSE,
  
  "Gastos de Administración/Activo+Contingente.",
  "rent_gastos_admin_actcont","rentabilidad","Rentabilidad",1,FALSE,FALSE
)

etiquetas_var <- c(
  liq_disp_oblig_cp            = "Disponibilidades / Oblig. Corto Plazo",
  liq_disp_inv_oblig_cp        = "(Disp. + Inv. Temp.) / Oblig. Corto Plazo",
  liq_disp_inv_pasivo          = "(Disp. + Inv. Temp.) / Pasivo Total",
  liq_disp_inv_activo          = "(Disp. + Inv. Temp.) / Activo Total",
  liq_activos_pasivos_cp       = "Activos Líquidos / Pasivos Corto Plazo",
  fon_oblig_publico_emp_paspat = "Oblig. Público + Emp. Públicas / (Pas. + Pat.)",
  fon_oblig_publico_paspat     = "Oblig. con el Público / (Pas. + Pat.)",
  fon_oblig_bancos_paspat      = "Oblig. con Bancos y Ent. Fin. / (Pas. + Pat.)",
  fon_oblig_subordinadas_paspat= "Obligaciones Subordinadas / (Pas. + Pat.)",
  fon_juridicas_institucionales= "Oblig. Personas Jurídicas e Institucionales / Total Oblig.",
  fon_personas_naturales       = "Oblig. Personas Naturales / Total Oblig. Público",
  fon_dias_permanencia_dpf     = "Días de Permanencia Promedio de DPF",
  act_disp_activo              = "Disponibilidades / Activo Total",
  act_cartera_neta_activo      = "Cartera Neta / Activo Total",
  act_productivo_actcont       = "Activo Productivo / (Activo + Contingente)",
  act_improductivo_patrimonio  = "Activo Improductivo / Patrimonio",
  car_reprogramada_cartera     = "Cartera Reprog. o Reestruct. / Cartera Total",
  car_mora_cartera             = "Cartera Vencida y Ejecución / Cartera Total",
  car_reprog_venc_ejec         = "Cartera Reprog. Vencida y Ejec. / Cartera Reprog.",
  car_prev_incobrable          = "Previsión Cartera Incobrable / Cartera Total",
  car_categoria_f              = "Cartera Categoría F (previsión 100%) / Cartera",
  solv_patrimonio_activo       = "Patrimonio / Activo Total",
  solv_patrimonio_actcont      = "Patrimonio / (Activo + Contingente)",
  solv_cap                     = "Coeficiente de Adecuación Patrimonial (CAP)",
  solv_mora_patrimonio         = "(Cartera Vencida + Ejecución) / Patrimonio",
  solv_mora_neta_patrimonio    = "(Cartera Vencida + Ejecución – Prev.) / Patrimonio",
  rent_roa                     = "Resultado Neto / (Activo + Contingente) — ROA",
  rent_roe                     = "Resultado Neto / Patrimonio — ROE",
  rent_resultado_operativo     = "Result. Operativo (neto de incobrables) / (Activo + Cont.)",
  rent_gastos_fin_pasivos_costo= "Gastos Financieros / Pasivos con Costo Promedio",
  rent_gastos_admin_actcont    = "Gastos de Administración / (Activo + Contingente)"
)

etiquetas_bloque <- c(
  liquidez  = "Dimensión 1: Liquidez Inmediata",
  fondeo    = "Dimensión 2: Estructura de Fondeo",
  activos   = "Dimensión 3: Composición de Activos",
  cartera   = "Dimensión 4: Presión de Cartera Crediticia",
  solvencia = "Dimensión 5: Solvencia y Capacidad de Absorción"
)

etiquetas_bloque_corto <- c(
  liquidez  = "Liquidez",
  fondeo    = "Fondeo",
  activos   = "Activos",
  cartera   = "Cartera",
  solvencia = "Solvencia"
)

# Marco financiero del constructo: separa dimensiones directas de canales de transmisión/absorción.
tabla_marco_dimensiones <- tibble::tribble(
  ~bloque, ~naturaleza, ~mecanismo_financiero, ~limite_interpretativo,
  "liquidez", "Dimensión directa",
  "Capacidad inmediata de atender obligaciones mediante disponibilidades y activos líquidos/temporarios.",
  "No equivale al LCR regulatorio ni incorpora por sí sola todos los flujos de 30 días.",
  "fondeo", "Dimensión directa/estructural",
  "Estabilidad, concentración, composición y permanencia relativa de las fuentes de financiación.",
  "No equivale al NSFR regulatorio ni reconstruye ASF/RSF por vencimientos contractuales.",
  "activos", "Canal de transmisión",
  "Composición y monetización del activo: mayor inmovilización puede limitar la generación rápida de liquidez.",
  "La composición de activos condiciona la liquidez, pero no constituye por sí sola riesgo de liquidez regulatorio.",
  "cartera", "Canal de transmisión de flujos",
  "Deterioro, mora y reprogramación pueden reducir o retrasar entradas contractuales de caja y elevar necesidades de financiación.",
  "Es un canal crediticio hacia liquidez; no se interpreta como sustituto del riesgo de crédito ni como causalidad estimada.",
  "solvencia", "Canal de absorción/confianza",
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
  "liq_disp_oblig_cp", "Alta",
  "Mayor disponibilidad relativa a obligaciones de corto plazo aumenta la capacidad inmediata de pago; por ello reduce vulnerabilidad.", FALSE,
  "liq_disp_inv_oblig_cp", "Alta",
  "Mayor cobertura de obligaciones de corto plazo mediante disponibilidades e inversiones temporarias amplía el colchón líquido.", FALSE,
  "liq_disp_inv_pasivo", "Alta",
  "Una mayor proporción de activos líquidos/temporarios frente al pasivo total mejora la capacidad de respuesta ante salidas de fondos.", FALSE,
  "liq_disp_inv_activo", "Alta",
  "Una mayor fracción del activo en disponibilidades e inversiones temporarias incrementa la liquidez del balance.", FALSE,
  
  "fon_oblig_publico_emp_paspat", "Media",
  "Una mayor dependencia agregada de obligaciones con público y empresas públicas puede aumentar concentración/dependencia de fondeo; su estabilidad depende de composición y vencimiento.", TRUE,
  "fon_oblig_publico_paspat", "Media",
  "La participación total de depósitos/obligaciones con el público no es adversa por sí misma: su riesgo depende de estabilidad, concentración y plazo. Se conserva como señal estructural y se exige sensibilidad.", TRUE,
  "fon_oblig_bancos_paspat", "Alta",
  "Una mayor dependencia de fondeo bancario/interfinanciero aproxima el balance a fuentes mayoristas potencialmente más sensibles a condiciones de mercado.", FALSE,
  "fon_oblig_subordinadas_paspat", "Media-Alta",
  "El fondeo subordinado suele tener horizonte contractual más largo y mayor estabilidad relativa de financiación, aunque no sustituye liquidez inmediata.", FALSE,
  "fon_juridicas_institucionales", "Alta",
  "Una mayor participación de personas jurídicas/institucionales aproxima la base de depósitos a fondeo corporativo potencialmente menos granular y más sensible a retiros concentrados.", FALSE,
  "fon_personas_naturales", "Alta",
  "Una mayor participación de personas naturales aproxima la estructura a depósitos minoristas más granulares; se interpreta como mayor estabilidad relativa del fondeo.", FALSE,
  "fon_dias_permanencia_dpf", "Alta",
  "Una mayor permanencia de depósitos a plazo reduce presión de refinanciación de corto plazo y mejora estabilidad temporal del fondeo.", FALSE,
  
  "act_disp_activo", "Alta",
  "Mayor peso de disponibilidades dentro del activo aumenta capacidad de monetización inmediata y reduce vulnerabilidad de liquidez.", FALSE,
  "act_cartera_neta_activo", "Alta",
  "Una mayor concentración del activo en cartera crediticia incrementa la proporción de activos menos líquidos y dependientes de cobros contractuales.", FALSE,
  "act_productivo_actcont", "Media",
  "Un mayor activo productivo puede mejorar generación de ingresos, pero productivo no equivale necesariamente a líquido; el signo protector es una hipótesis de estructura del balance y requiere sensibilidad.", TRUE,
  "act_improductivo_patrimonio", "Media-Alta",
  "Un mayor activo improductivo respecto al patrimonio reduce flexibilidad del balance y puede elevar necesidades de financiación/absorción.", FALSE,
  
  "car_reprogramada_cartera", "Alta",
  "Una mayor cartera reprogramada/restructurada indica mayor incertidumbre sobre flujos contractuales y potencial presión sobre entradas de caja.", FALSE,
  "car_mora_cartera", "Alta",
  "Mayor mora reduce la realización esperada de flujos de caja de la cartera y eleva la vulnerabilidad financiera vinculada a liquidez.", FALSE,
  "car_reprog_venc_ejec", "Alta",
  "Mayor deterioro dentro de la cartera reprogramada implica menor recuperación esperada de flujos; su dinámica empírica distinta se controla con leave-one-out.", FALSE,
  "car_prev_incobrable", "Media",
  "Más previsiones sobre cartera pueden reflejar mayor deterioro crediticio reconocido, aunque simultáneamente constituyen un colchón contable; se usa como señal de deterioro y exige sensibilidad.", TRUE,
  "car_categoria_f", "Alta",
  "Mayor participación de cartera con requerimiento de previsión del 100% refleja deterioro crediticio severo y menor expectativa de recuperación de flujos.", FALSE,
  
  "solv_patrimonio_activo", "Alta",
  "Mayor patrimonio relativo al activo amplía capacidad de absorción de pérdidas y sostiene confianza/acceso a fondeo; reduce vulnerabilidad.", FALSE,
  "solv_patrimonio_actcont", "Alta",
  "Mayor patrimonio frente a activos y contingentes fortalece capacidad de absorción ante shocks y reduce vulnerabilidad.", FALSE,
  "solv_cap", "Alta",
  "Mayor suficiencia patrimonial incrementa capacidad de absorción y resiliencia; se orienta como factor protector.", FALSE,
  "solv_mora_patrimonio", "Alta",
  "Mayor mora relativa al patrimonio consume capacidad de absorción y puede deteriorar confianza y acceso a fondeo.", FALSE,
  "solv_mora_neta_patrimonio", "Alta",
  "Mayor mora neta de previsiones respecto al patrimonio implica mayor exposición residual de pérdidas frente al colchón patrimonial.", FALSE
)

if (anyDuplicated(auditoria_financiera_signos$variable) > 0) {
  stop("La auditoría financiera de signos contiene variables duplicadas.")
}


# ══════════════════════════════════════════════════════════════
# 5.  BASE_MODEL, DICCIONARIO Y MUESTRA ANALÍTICA
# ══════════════════════════════════════════════════════════════

cat("\n── 5. Construyendo y validando base modelo ──\n")

# Auditoría de correspondencia entre el diccionario y la base.
auditoria_diccionario <- dic %>%
  mutate(
    existe_en_base = original %in% names(Base_raw),
    obligatorio_nucleo = entra_indice
  )

faltantes_nucleo <- auditoria_diccionario %>%
  filter(obligatorio_nucleo, !existe_en_base)

if (nrow(faltantes_nucleo) > 0) {
  stop(
    "Faltan indicadores núcleo en la base: ",
    paste(faltantes_nucleo$original, collapse = " | ")
  )
}

# Variables complementarias pueden no existir; se documentan, pero no detienen.
dic_ok <- dic %>% filter(original %in% names(Base_raw))

if (anyDuplicated(dic_ok$var) > 0) stop("El diccionario contiene nombres internos duplicados.")
if (anyDuplicated(dic_ok$original) > 0) stop("El diccionario contiene columnas originales duplicadas.")

# Auditoría de parseo: distingue columnas ya numéricas de conversiones desde texto
# y detiene la ejecución si existen valores no vacíos que no pudieron convertirse.
auditoria_parseo <- purrr::map_dfr(dic_ok$original, function(nm) {
  raw <- Base_raw[[nm]]
  raw_chr <- stringr::str_squish(as.character(raw))
  vacio <- is.na(raw) | raw_chr %in% c("", "NA", "N/A", "-", "--")
  parsed <- parse_num(raw)
  tibble(
    variable_original = nm,
    clase_original = paste(class(raw), collapse = "/"),
    n_total = length(raw),
    n_no_vacios = sum(!vacio),
    n_parseados = sum(is.finite(parsed) & !is.na(parsed)),
    n_fallo_parseo = sum(!vacio & (is.na(parsed) | !is.finite(parsed)))
  )
})

if (any(auditoria_parseo$n_fallo_parseo > 0)) {
  stop("Existen valores no vacíos que no pudieron convertirse a numérico. Revise 'auditoria_parseo'.")
}

Base_model <- Base_raw %>% select(Fecha, all_of(dic_ok$original))
names(Base_model)[match(dic_ok$original, names(Base_model))] <- dic_ok$var
Base_model <- Base_model %>%
  mutate(across(-Fecha, parse_num)) %>%
  arrange(Fecha)

Base_muestra <- Base_model %>%
  filter(Fecha >= fecha_inicio_muestra, Fecha <= fecha_fin_muestra)

if (nrow(Base_muestra) != length(fechas_esperadas)) {
  stop("La muestra analítica no contiene exactamente ", length(fechas_esperadas), " meses.")
}
if (!identical(Base_muestra$Fecha, fechas_esperadas)) {
  stop("La secuencia temporal de la muestra no coincide con los 192 meses esperados.")
}

# Auditoría de valores no finitos.
auditoria_no_finitos <- tibble(
  variable = setdiff(names(Base_muestra), "Fecha"),
  n_na = vapply(Base_muestra[setdiff(names(Base_muestra), "Fecha")],
                function(x) sum(is.na(x)), numeric(1)),
  n_no_finitos = vapply(Base_muestra[setdiff(names(Base_muestra), "Fecha")],
                        function(x) sum(!is.na(x) & !is.finite(x)), numeric(1))
) %>%
  left_join(dic_ok %>% select(var, original, bloque_id), by = c("variable" = "var"))

if (any(auditoria_no_finitos$n_no_finitos > 0)) {
  stop("Existen valores Inf/-Inf en indicadores. Revise 'auditoria_no_finitos'.")
}

cat("  Base completa:", nrow(Base_model), "obs.\n")
cat("  Muestra analítica:", nrow(Base_muestra), "obs. (",
    format(min(Base_muestra$Fecha), "%b %Y"), "→",
    format(max(Base_muestra$Fecha), "%b %Y"), ")\n")


# ══════════════════════════════════════════════════════════════
# 6.  COBERTURA EN MUESTRA
# ══════════════════════════════════════════════════════════════

vars_cand <- dic_ok %>% filter(entra_indice) %>% pull(var)

cobertura_vars <- Base_muestra %>%
  summarise(across(all_of(vars_cand), ~ mean(is.finite(.) & !is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "cobertura") %>%
  left_join(dic_ok %>% select(var, original, bloque_id),
            by = c("variable" = "var")) %>%
  mutate(
    incluir           = cobertura >= umbral_cobertura,
    cobertura_pct     = round(cobertura * 100, 1),
    etiqueta_variable = etiquetas_var[variable]
  )

vars_excluidas_cob <- cobertura_vars %>% filter(!incluir) %>% pull(variable)

# CRÍTICO: se preserva el orden del diccionario; nunca se deriva el orden
# metodológico de un arrange(cobertura).
vars_indice <- dic_ok %>%
  filter(entra_indice, var %in% cobertura_vars$variable[cobertura_vars$incluir]) %>%
  pull(var)

if (length(vars_indice) == 0) stop("Ninguna variable supera el umbral de cobertura.")
if (anyDuplicated(vars_indice) > 0) stop("vars_indice contiene duplicados.")

cat("\n  Variables excluidas (cobertura <", umbral_cobertura * 100, "%):\n")
if (length(vars_excluidas_cob) == 0) {
  cat("    ninguna\n")
} else {
  for (v in vars_excluidas_cob) {
    cob <- cobertura_vars$cobertura[cobertura_vars$variable == v]
    cat("    –", v, sprintf("(%.1f%%)\n", cob * 100))
  }
}
cat("  Variables núcleo ICVLF:", length(vars_indice), "\n")

# ══════════════════════════════════════════════════════════════
# 7.  EDA — ANÁLISIS EXPLORATORIO
# ══════════════════════════════════════════════════════════════

cat("\n── 7. EDA ──\n")

estadisticas_ext <- Base_muestra %>%
  select(all_of(vars_indice)) %>%
  summarise(across(everything(), list(
    n        = ~sum(!is.na(.)),
    n_na     = ~sum(is.na(.)),
    media    = ~mean(., na.rm = TRUE),
    sd       = ~sd(., na.rm = TRUE),
    cv_pct   = ~safe_cv(.),
    min      = ~min(., na.rm = TRUE),
    p5       = ~quantile(., 0.05, na.rm = TRUE),
    p25      = ~quantile(., 0.25, na.rm = TRUE),
    mediana  = ~median(., na.rm = TRUE),
    p75      = ~quantile(., 0.75, na.rm = TRUE),
    p95      = ~quantile(., 0.95, na.rm = TRUE),
    max      = ~max(., na.rm = TRUE),
    sesgo    = ~moments::skewness(.[!is.na(.)]),
    curtosis = ~moments::kurtosis(.[!is.na(.)]) - 3,
    jb_stat  = ~{x <- .[!is.na(.)]; n <- length(x)
    s <- moments::skewness(x); k <- moments::kurtosis(x) - 3
    n / 6 * (s^2 + k^2 / 4)},
    jb_pval  = ~{x <- .[!is.na(.)]; n <- length(x)
    s <- moments::skewness(x); k <- moments::kurtosis(x) - 3
    jb <- n / 6 * (s^2 + k^2 / 4)
    pchisq(jb, df = 2, lower.tail = FALSE)}
  ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "estadistico"),
               names_sep = "__") %>%
  pivot_wider(names_from = estadistico, values_from = value) %>%
  left_join(dic_ok %>% select(var, original, bloque_id),
            by = c("variable" = "var")) %>%
  mutate(
    etiqueta          = etiquetas_var[variable],
    normalidad_jb     = ifelse(jb_pval >= 0.05, "Normal (JB ≥ 5%)",
                               "No normal (JB < 5%)"),
    orientacion_riesgo = ifelse(
      dic_ok$direccion_riesgo[match(variable, dic_ok$var)] == 1,
      "↑ mayor valor = mayor riesgo",
      "↓ mayor valor = menor riesgo")
  ) %>%
  arrange(bloque_id, variable)

cat("  Estadísticas ext:", nrow(estadisticas_ext), "variables\n")

test_adf <- lapply(vars_indice, function(v) {
  x <- Base_muestra[[v]]; x <- x[is.finite(x) & !is.na(x)]
  if (length(x) < 20)
    return(tibble(variable = v, adf_stat = NA, adf_pval = NA,
                  estacionaria = NA))
  res <- tryCatch(tseries::adf.test(x, alternative = "stationary"),
                  error = function(e) NULL)
  if (is.null(res))
    return(tibble(variable = v, adf_stat = NA, adf_pval = NA,
                  estacionaria = NA))
  tibble(variable    = v,
         adf_stat    = as.numeric(res$statistic),
         adf_pval    = as.numeric(res$p.value),
         estacionaria= as.numeric(res$p.value) < 0.05)
}) %>%
  bind_rows() %>%
  left_join(dic_ok %>% select(var, bloque_id), by = c("variable" = "var")) %>%
  mutate(
    etiqueta      = etiquetas_var[variable],
    interpretacion = ifelse(estacionaria, "Estacionaria (p < 5%)",
                            "No estacionaria (p ≥ 5%)"),
    nota = "Diagnóstico complementario."
  )

cat("  ADF estacionarias:", sum(test_adf$estacionaria, na.rm = TRUE),
    "/ no estacionarias:", sum(!test_adf$estacionaria, na.rm = TRUE), "\n")

matrices_cor_bloque <- list()
for (bl in unique(dic_ok$bloque_id[dic_ok$var %in% vars_indice])) {
  vars_bl <- dic_ok %>%
    filter(bloque_id == bl, var %in% vars_indice) %>%
    pull(var)
  if (length(vars_bl) < 2) next
  
  Xbl <- Base_muestra %>% select(all_of(vars_bl))
  
  C_pearson <- cor(Xbl, use = "pairwise.complete.obs", method = "pearson")
  C_spearman <- cor(Xbl, use = "pairwise.complete.obs", method = "spearman")
  
  N_pares <- outer(
    vars_bl, vars_bl,
    Vectorize(function(v1, v2) safe_n(Base_muestra[[v1]], Base_muestra[[v2]]))
  )
  dimnames(N_pares) <- list(vars_bl, vars_bl)
  
  matrices_cor_bloque[[bl]] <- list(
    pearson = C_pearson,
    spearman = C_spearman,
    n_pares = N_pares,
    n_min = min(N_pares[upper.tri(N_pares)], na.rm = TRUE),
    n_max = max(N_pares[upper.tri(N_pares)], na.rm = TRUE)
  )
}

tabla_cor_bloques <- lapply(names(matrices_cor_bloque), function(bl) {
  mp <- matrices_cor_bloque[[bl]]$pearson
  ms <- matrices_cor_bloque[[bl]]$spearman
  nn <- matrices_cor_bloque[[bl]]$n_pares
  
  df_p <- as.data.frame(mp) %>%
    tibble::rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "r_pearson") %>%
    filter(var1 < var2)
  
  df_s <- as.data.frame(ms) %>%
    tibble::rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "r_spearman") %>%
    filter(var1 < var2)
  
  df_n <- as.data.frame(nn) %>%
    tibble::rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "n_pares") %>%
    filter(var1 < var2)
  
  df_p %>%
    left_join(df_s, by = c("var1", "var2")) %>%
    left_join(df_n, by = c("var1", "var2")) %>%
    mutate(
      bloque = bl,
      etiq_var1 = etiquetas_var[var1],
      etiq_var2 = etiquetas_var[var2],
      intensidad_p = case_when(
        abs(r_pearson) >= 0.70 ~ "Alta (|r| ≥ 0.70)",
        abs(r_pearson) >= 0.40 ~ "Moderada (0.40 ≤ |r| < 0.70)",
        TRUE                   ~ "Baja (|r| < 0.40)"
      )
    )
}) %>% bind_rows()

cat("  Correlaciones EDA: pairwise.complete.obs + N por par (no listwise deletion).\n")


# ══════════════════════════════════════════════════════════════
# 8.  NORMALIZACIÓN Z-SCORE Y ORIENTACIÓN ECONÓMICA DEL RIESGO
# ══════════════════════════════════════════════════════════════

cat("\n── 8. Normalización y orientación de riesgo ──\n")

X_ref <- Base_muestra %>% select(all_of(vars_indice))
medias_ref <- vapply(X_ref, mean, numeric(1), na.rm = TRUE)
sds_ref    <- vapply(X_ref, sd,   numeric(1), na.rm = TRUE)

if (any(!is.finite(medias_ref))) stop("Hay medias no finitas en la muestra de referencia.")
if (any(!is.finite(sds_ref) | sds_ref <= 0)) {
  malas <- names(sds_ref)[!is.finite(sds_ref) | sds_ref <= 0]
  stop("Indicadores sin variación utilizable: ", paste(malas, collapse = ", "))
}

X_z <- sweep(
  sweep(Base_model %>% select(all_of(vars_indice)) %>% as.data.frame(),
        2, medias_ref, "-"),
  2, sds_ref, "/"
)
names(X_z) <- vars_indice

# CORRECCIÓN CRÍTICA: emparejamiento nominal explícito.
pos_dir <- match(vars_indice, dic_ok$var)
if (anyNA(pos_dir)) stop("No se pudo emparejar una variable con su dirección de riesgo.")

direccion <- setNames(dic_ok$direccion_riesgo[pos_dir], vars_indice)
if (!all(direccion %in% c(-1, 1))) stop("Las direcciones de riesgo deben ser -1 o +1.")
if (!identical(names(direccion), colnames(X_z))) {
  stop("Error de alineación entre variables y dirección de riesgo.")
}

# Prueba explícita contra el diccionario: evita reaparición del bug de v5.1.
stopifnot(all(
  direccion == dic_ok$direccion_riesgo[match(names(direccion), dic_ok$var)]
))

X_risk <- sweep(as.matrix(X_z), 2, direccion[colnames(X_z)], "*")
X_risk <- as.data.frame(X_risk)

# Tabla auditable para anexos/metodología.
tabla_orientacion_riesgo <- tibble(
  variable = vars_indice,
  indicador = etiquetas_var[vars_indice],
  bloque = dic_ok$bloque_id[match(vars_indice, dic_ok$var)],
  direccion = as.numeric(direccion[vars_indice]),
  lectura = ifelse(
    direccion[vars_indice] == 1,
    "Mayor valor original = mayor vulnerabilidad",
    "Mayor valor original = menor vulnerabilidad; se invierte el signo"
  )
) %>%
  left_join(auditoria_financiera_signos, by = "variable")

datos_risk_full <- bind_cols(Fecha = Base_model$Fecha, X_risk)
datos_risk_muestra <- datos_risk_full %>%
  filter(Fecha >= fecha_inicio_muestra, Fecha <= fecha_fin_muestra)

cat("  ✓ Direcciones de riesgo verificadas por nombre para", length(direccion), "variables.\n")


# ══════════════════════════════════════════════════════════════
# 9.  SUBÍNDICES: PRINCIPAL IGUAL + PCA COMO SENSIBILIDAD
# ══════════════════════════════════════════════════════════════

# Diagnóstico PCA: se ejecuta DESPUÉS de estandarizar y orientar al riesgo.
# El PCA no define la dirección económica. La coherencia de cargas se usa
# para evaluar interpretabilidad, no para modificar el modelo principal.
diagnosticar_pca_dimension <- function(data_risk, vars, nombre_bloque) {
  vars <- vars[vars %in% names(data_risk)]
  Xb <- data_risk %>% select(all_of(vars)) %>% as.data.frame()
  Xc <- Xb[complete.cases(Xb), , drop = FALSE]
  n_vars <- ncol(Xb)
  
  out <- list(
    cor_media_abs = NA_real_, cor_media_signed = NA_real_, prop_cor_positiva = NA_real_,
    KMO = NA_real_, Bartlett_p = NA_real_, var_exp_pc1 = NA_real_,
    coherencia_cargas = NA_real_, cargas_pc1 = setNames(rep(NA_real_, n_vars), vars),
    pesos_pca = setNames(rep(1 / n_vars, n_vars), vars), pca_interpretable = FALSE
  )
  
  if (n_vars < 2 || nrow(Xc) < max(20, 5 * n_vars)) return(out)
  
  C <- cor(Xc, method = "pearson")
  vals <- C[upper.tri(C)]
  out$cor_media_abs <- mean(abs(vals), na.rm = TRUE)
  out$cor_media_signed <- mean(vals, na.rm = TRUE)
  out$prop_cor_positiva <- mean(vals > 0, na.rm = TRUE)
  
  if (n_vars >= 3) {
    out$KMO <- tryCatch(psych::KMO(C)$MSA, error = function(e) NA_real_)
    out$Bartlett_p <- tryCatch(
      psych::cortest.bartlett(C, n = nrow(Xc))$p.value,
      error = function(e) NA_real_
    )
  }
  
  pca <- tryCatch(prcomp(Xc, center = FALSE, scale. = FALSE), error = function(e) NULL)
  if (is.null(pca)) return(out)
  
  out$var_exp_pc1 <- pca$sdev[1]^2 / sum(pca$sdev^2)
  score_pc1 <- pca$x[, 1]
  prom_riesgo <- rowMeans(Xc, na.rm = TRUE)
  r_pc1_media <- safe_cor(score_pc1, prom_riesgo, min_pares = 5)
  signo <- ifelse(is.finite(r_pc1_media) && r_pc1_media < 0, -1, 1)
  cargas <- signo * pca$rotation[, 1]
  out$cargas_pc1 <- cargas
  out$coherencia_cargas <- mean(cargas >= 0, na.rm = TRUE)
  out$pesos_pca <- abs(cargas) / sum(abs(cargas))
  
  out$pca_interpretable <-
    is.finite(out$cor_media_abs) && out$cor_media_abs >= umbral_cor_media_pca &&
    is.finite(out$KMO) && out$KMO >= umbral_kmo &&
    is.finite(out$Bartlett_p) && out$Bartlett_p < umbral_bartlett &&
    is.finite(out$var_exp_pc1) && out$var_exp_pc1 >= umbral_var_pc1 &&
    is.finite(out$coherencia_cargas) && out$coherencia_cargas >= umbral_coherencia_pc1
  
  out
}

estandarizar_score <- function(x, nombre) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) stop("El subíndice ", nombre, " no tiene variación suficiente.")
  (x - m) / s
}

crear_subindices_dimension <- function(data_risk, vars, nombre_bloque,
                                       n_min_frac = n_min_frac_subindice) {
  vars <- vars[vars %in% names(data_risk)]
  if (length(vars) == 0) return(NULL)
  Xb <- data_risk %>% select(all_of(vars)) %>% as.data.frame()
  n_vars <- ncol(Xb)
  
  diag <- diagnosticar_pca_dimension(data_risk, vars, nombre_bloque)
  
  # PRINCIPAL: igualdad dentro de la dimensión.
  w_eq <- setNames(rep(1 / n_vars, n_vars), vars)
  score_eq_raw <- suma_ponderada_robusto(as.matrix(Xb), w_eq, n_min_frac)
  score_eq <- estandarizar_score(score_eq_raw, paste0(nombre_bloque, " [equal]"))
  
  # SENSIBILIDAD: pesos informados por la magnitud del PC1.
  w_pca <- validar_pesos(diag$pesos_pca, vars)
  score_pca_raw <- suma_ponderada_robusto(as.matrix(Xb), w_pca, n_min_frac)
  score_pca <- estandarizar_score(score_pca_raw, paste0(nombre_bloque, " [PCA]"))
  
  list(
    score_equal = score_eq,
    score_equal_raw = score_eq_raw,
    score_pca = score_pca,
    score_pca_raw = score_pca_raw,
    metodo = tibble(
      bloque = nombre_bloque,
      n_indicadores = n_vars,
      n_casos_completos = sum(complete.cases(Xb)),
      cor_media_abs = diag$cor_media_abs,
      cor_media_orientada = diag$cor_media_signed,
      prop_cor_positiva = diag$prop_cor_positiva,
      KMO = diag$KMO,
      Bartlett_pvalue = diag$Bartlett_p,
      var_exp_pc1 = diag$var_exp_pc1,
      coherencia_cargas_pc1 = diag$coherencia_cargas,
      pca_interpretable = diag$pca_interpretable,
      media_subindice_bruto = mean(score_eq_raw, na.rm = TRUE),
      sd_subindice_bruto = sd(score_eq_raw, na.rm = TRUE),
      n_na_subindice_bruto = sum(is.na(score_eq_raw)),
      metodo_principal = "Promedio simple de indicadores orientados (pesos internos iguales)",
      rol_pca = "Sensibilidad empírica; no determina la especificación principal"
    ),
    pesos_equal = tibble(
      bloque = nombre_bloque, indicador = vars, etiqueta = etiquetas_var[vars],
      peso_interno = as.numeric(w_eq), metodo = "Pesos internos iguales — principal"
    ),
    pesos_pca = tibble(
      bloque = nombre_bloque, indicador = vars, etiqueta = etiquetas_var[vars],
      carga_pc1 = as.numeric(diag$cargas_pc1[vars]),
      peso_pca = as.numeric(w_pca[vars]),
      pca_interpretable = diag$pca_interpretable,
      metodo = "|coeficiente PC1| normalizado — sensibilidad"
    )
  )
}

# Helper para reconstrucciones leave-one-out y validación sin solapamiento.
crear_subindice_equal_simple <- function(data_risk, vars, nombre_bloque,
                                         n_min_frac = n_min_frac_subindice) {
  vars <- vars[vars %in% names(data_risk)]
  if (length(vars) == 0) return(NULL)
  Xb <- data_risk %>% select(all_of(vars)) %>% as.data.frame()
  w <- setNames(rep(1 / length(vars), length(vars)), vars)
  raw <- suma_ponderada_robusto(as.matrix(Xb), w, n_min_frac)
  estandarizar_score(raw, nombre_bloque)
}

# Normalización robusta mediana/MAD para sensibilidad.
param_rob <- lapply(Base_muestra %>% select(all_of(vars_indice)), robust_scale_ref)
centros_rob <- vapply(param_rob, function(z) z[["center"]], numeric(1))
escalas_rob <- vapply(param_rob, function(z) z[["scale"]], numeric(1))

X_z_rob <- sweep(
  sweep(Base_model %>% select(all_of(vars_indice)) %>% as.data.frame(),
        2, centros_rob, "-"),
  2, escalas_rob, "/"
)
X_risk_rob <- sweep(as.matrix(X_z_rob), 2, direccion[colnames(X_z_rob)], "*")
datos_risk_rob_muestra <- bind_cols(Fecha = Base_model$Fecha, as.data.frame(X_risk_rob)) %>%
  filter(Fecha >= fecha_inicio_muestra, Fecha <= fecha_fin_muestra)


# ══════════════════════════════════════════════════════════════
# 10.  CONSTRUCCIÓN DE SUBÍNDICES
# ══════════════════════════════════════════════════════════════

cat("\n── 10. Construyendo subíndices: Equal principal + PCA sensibilidad ──\n")

vars_por_bloque <- dic_ok %>%
  filter(var %in% vars_indice) %>%
  group_by(bloque_id) %>%
  summarise(vars = list(var), .groups = "drop")

subindices <- tibble(Fecha = datos_risk_muestra$Fecha)              # principal: internos iguales, reestandarizados
subindices_brutos <- tibble(Fecha = datos_risk_muestra$Fecha)        # promedios internos antes de reestandarizar
subindices_pca_interno <- tibble(Fecha = datos_risk_muestra$Fecha)  # sensibilidad PCA
subindices_robustos <- tibble(Fecha = datos_risk_muestra$Fecha)     # sensibilidad robust-z
metodos_subindices <- list()
pesos_equal_list <- list()
pesos_pca_list <- list()

for (i in seq_len(nrow(vars_por_bloque))) {
  bloque_i <- vars_por_bloque$bloque_id[i]
  vars_i <- vars_por_bloque$vars[[i]]
  obj <- crear_subindices_dimension(datos_risk_muestra, vars_i, bloque_i)
  
  if (!is.null(obj)) {
    subindices[[bloque_i]] <- obj$score_equal
    subindices_brutos[[bloque_i]] <- obj$score_equal_raw
    subindices_pca_interno[[bloque_i]] <- obj$score_pca
    subindices_robustos[[bloque_i]] <- crear_subindice_equal_simple(
      datos_risk_rob_muestra, vars_i, paste0(bloque_i, " [robust-z]")
    )
    metodos_subindices[[bloque_i]] <- obj$metodo
    pesos_equal_list[[bloque_i]] <- obj$pesos_equal
    pesos_pca_list[[bloque_i]] <- obj$pesos_pca
  }
}

tabla_metodos_subindices <- bind_rows(metodos_subindices) %>%
  mutate(bloque_nombre = etiquetas_bloque[bloque])
tabla_pesos_internos <- bind_rows(pesos_equal_list)
tabla_pesos_pca_internos <- bind_rows(pesos_pca_list)

bloques_core <- intersect(
  c("liquidez", "fondeo", "activos", "cartera", "solvencia"), names(subindices)
)
if (length(bloques_core) != 5) stop("No se construyeron las cinco dimensiones núcleo del ICVLF.")

if (anyNA(subindices[, bloques_core])) {
  stop(
    "Al menos una dimensión presenta NA tras aplicar el umbral interno. ",
    "No se permite reponderación automática ENTRE dimensiones en el ICVLF principal."
  )
}

tabla_pesos_jerarquicos <- tabla_pesos_internos %>%
  left_join(
    tabla_metodos_subindices %>% select(bloque, sd_subindice_bruto),
    by = "bloque"
  ) %>%
  mutate(
    peso_dimension = 1 / length(bloques_core),
    peso_jerarquico_nominal = peso_dimension * peso_interno,
    coeficiente_analitico_z = peso_dimension * peso_interno / sd_subindice_bruto,
    nota = paste0(
      "Peso jerárquico nominal = 20% × peso interno. ",
      "El coeficiente analítico incorpora además la reestandarización del subíndice bruto."
    )
  )

auditoria_reponderacion_interna <- map_dfr(bloques_core, function(bl) {
  vars_bl <- vars_por_bloque$vars[[match(bl, vars_por_bloque$bloque_id)]]
  X <- datos_risk_muestra %>% select(all_of(vars_bl))
  n_disp <- rowSums(is.finite(as.matrix(X)) & !is.na(as.matrix(X)))
  tibble(
    Fecha = datos_risk_muestra$Fecha,
    bloque = bl,
    n_indicadores_total = length(vars_bl),
    n_disponibles = n_disp,
    fraccion_disponible = n_disp / length(vars_bl),
    hubo_reponderacion_interna = n_disp < length(vars_bl)
  )
}) %>% filter(hubo_reponderacion_interna)


# Auditorías de pesos y estandarización.
tabla_suma_pesos <- tabla_pesos_internos %>%
  group_by(bloque) %>% summarise(suma_pesos = sum(peso_interno), .groups = "drop")
if (any(abs(tabla_suma_pesos$suma_pesos - 1) > 1e-10)) stop("Pesos iguales internos no suman 1.")

tabla_suma_pesos_pca <- tabla_pesos_pca_internos %>%
  group_by(bloque) %>% summarise(suma_pesos = sum(peso_pca), .groups = "drop")
if (any(abs(tabla_suma_pesos_pca$suma_pesos - 1) > 1e-10)) stop("Pesos PCA internos no suman 1.")

auditoria_subindices <- tibble(
  bloque = bloques_core,
  media = vapply(subindices[bloques_core], mean, numeric(1), na.rm = TRUE),
  sd = vapply(subindices[bloques_core], sd, numeric(1), na.rm = TRUE),
  n_na = vapply(subindices[bloques_core], function(x) sum(is.na(x)), numeric(1))
)

cor_subindices <- cor(subindices %>% select(all_of(bloques_core)), use = "pairwise.complete.obs")

# Redundancia: pares con |r|>=0.90 en datos ORIENTADOS al riesgo.
tabla_redundancia <- map_dfr(bloques_core, function(bl) {
  vars_bl <- vars_por_bloque$vars[[match(bl, vars_por_bloque$bloque_id)]]
  X <- datos_risk_muestra %>% select(all_of(vars_bl))
  C <- cor(X, use = "pairwise.complete.obs")
  as.data.frame(C) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
    filter(var1 < var2, abs(r) >= 0.90) %>%
    transmute(
      bloque = bl, indicador_1 = etiquetas_var[var1], indicador_2 = etiquetas_var[var2],
      r_orientado = r, advertencia = "Alta redundancia potencial; revisar doble conteo"
    )
})

cat("  Método principal dentro de cada dimensión: pesos iguales.\n")
cat("  PCA conservado como sensibilidad; diagnósticos exportados por dimensión.\n")


# ══════════════════════════════════════════════════════════════
# 11.  ICVLF — PRINCIPAL Y SENSIBILIDADES
# ══════════════════════════════════════════════════════════════

cat("\n── 11. Construyendo ICVLF y especificaciones de sensibilidad ──\n")

# PRINCIPAL: pesos iguales dentro de dimensión + 20% entre las cinco dimensiones.
# No se permite que un NA de una dimensión convierta silenciosamente 20% en 25%.
pesos_dimensionales <- setNames(
  rep(1 / length(bloques_core), length(bloques_core)),
  bloques_core
)
subindices$ICVLF_equal <- suma_ponderada_robusto(
  as.matrix(subindices[, bloques_core]),
  pesos_dimensionales,
  n_min_frac = 1
)
stopifnot(
  max(abs(subindices$ICVLF_equal - rowMeans(subindices[, bloques_core])), na.rm = TRUE) < 1e-10
)

# Sensibilidad A: pesos alternativos ENTRE dimensiones. No es ponderación regulatoria.
pesos_alt <- c(liquidez = 0.35, fondeo = 0.25, activos = 0.15,
               cartera = 0.15, solvencia = 0.10)
pesos_alt <- pesos_alt[bloques_core] / sum(pesos_alt[bloques_core])
subindices$ICVLF_alt_pesos <- suma_ponderada_robusto(as.matrix(subindices[, bloques_core]), pesos_alt)

# Sensibilidad B: núcleo más estrecho liquidez + fondeo.
subindices$ICVLF_nucleo <- 0.5 * subindices$liquidez + 0.5 * subindices$fondeo

# Sensibilidad C: PCA global de los cinco subíndices PRINCIPALES.
X_sub_imp <- subindices %>% select(all_of(bloques_core)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
pca_sub <- prcomp(X_sub_imp, center = TRUE, scale. = TRUE)
score_pca_sub <- pca_sub$x[, 1]
if (safe_cor(score_pca_sub, subindices$ICVLF_equal, min_pares = 5) < 0) {
  score_pca_sub <- -score_pca_sub
  pesos_sub_pca <- -pca_sub$rotation[, 1]
} else {
  pesos_sub_pca <- pca_sub$rotation[, 1]
}
subindices$ICVLF_pca_global <- as.numeric(scale(score_pca_sub))
var_exp_pca_global <- pca_sub$sdev[1]^2 / sum(pca_sub$sdev^2)

# Sensibilidad D: PCA para pesos DENTRO de cada dimensión.
if (anyNA(subindices_pca_interno[, bloques_core])) stop("NA en dimensiones PCA internas.")
subindices$ICVLF_pca_interno <- suma_ponderada_robusto(
  as.matrix(subindices_pca_interno[, bloques_core]), pesos_dimensionales, n_min_frac = 1
)

# Sensibilidad E: z robusto mediana/MAD, manteniendo pesos iguales.
if (anyNA(subindices_robustos[, bloques_core])) stop("NA en dimensiones robust-z.")
subindices$ICVLF_robust_z <- suma_ponderada_robusto(
  as.matrix(subindices_robustos[, bloques_core]), pesos_dimensionales, n_min_frac = 1
)

tabla_pesos_pca_bloques <- tibble(
  bloque = names(pesos_sub_pca),
  bloque_nombre = etiquetas_bloque_corto[names(pesos_sub_pca)],
  carga_pca_global = as.numeric(pesos_sub_pca),
  peso_abs_normalizado = abs(pesos_sub_pca) / sum(abs(pesos_sub_pca))
)

indices <- subindices %>%
  mutate(
    ICVLF_equal_100 = normalizar_0100(ICVLF_equal),
    ICVLF_alt_pesos_100 = normalizar_0100(ICVLF_alt_pesos),
    ICVLF_pca_global_100 = normalizar_0100(ICVLF_pca_global),
    ICVLF_nucleo_100 = normalizar_0100(ICVLF_nucleo),
    ICVLF_pca_interno_100 = normalizar_0100(ICVLF_pca_interno),
    ICVLF_robust_z_100 = normalizar_0100(ICVLF_robust_z),
    nivel_vulnerabilidad = clasificar_cuartiles(ICVLF_equal_100)
  )

# Helper para cualquier reconstrucción del ICVLF principal con subconjuntos de variables.
construir_icvlf_equal_desde_vars <- function(vars_keep) {
  vpb <- dic_ok %>%
    filter(var %in% vars_keep, bloque_id %in% bloques_core) %>%
    group_by(bloque_id) %>% summarise(vars = list(var), .groups = "drop")
  tmp <- tibble(Fecha = datos_risk_muestra$Fecha)
  for (i in seq_len(nrow(vpb))) {
    tmp[[vpb$bloque_id[i]]] <- crear_subindice_equal_simple(
      datos_risk_muestra, vpb$vars[[i]], vpb$bloque_id[i]
    )
  }
  if (!all(bloques_core %in% names(tmp))) return(rep(NA_real_, nrow(tmp)))
  w_dim <- setNames(rep(1 / length(bloques_core), length(bloques_core)), bloques_core)
  # En reconstrucciones no se permite que un NA en una dimensión repondere las restantes.
  suma_ponderada_robusto(as.matrix(tmp[, bloques_core]), w_dim, n_min_frac = 1)
}

cat("  Varianza explicada PCA global (PC1): ", sprintf("%.1f%%\n", var_exp_pca_global * 100), sep = "")
cat("  Distribución de niveles históricos relativos:\n")
print(table(indices$nivel_vulnerabilidad))
# ══════════════════════════════════════════════════════════════
# 12.  INVENTARIO, SUBPERIODOS Y TABLAS DESCRIPTIVAS
# ══════════════════════════════════════════════════════════════

metadata_ejecucion <- tibble(
  item = c(
    "Versión del script", "Archivo de datos", "MD5 de la base", "Fecha de inicio",
    "Fecha de fin", "Total de observaciones", "Frecuencia", "Unidad de análisis",
    "Umbral de cobertura", "Interpretación temporal", "Escala principal"
  ),
  valor = c(
    "ICVLF v6.3-B1",
    basename(ruta),
    unname(tools::md5sum(ruta)),
    format(fecha_inicio_muestra, "%d/%m/%Y"),
    format(fecha_fin_muestra, "%d/%m/%Y"),
    as.character(nrow(indices)),
    "Mensual",
    unidad_analisis_texto,
    paste0(umbral_cobertura * 100, "%"),
    "Retrospectiva: parámetros estimados sobre la muestra completa 2010–2025",
    "z-score orientado al riesgo; pesos internos iguales; 0–100 solo para comunicación"
  )
)

periodo_efectivo <- metadata_ejecucion %>%
  filter(item %in% c("Fecha de inicio", "Fecha de fin", "Total de observaciones",
                     "Frecuencia", "Unidad de análisis", "Umbral de cobertura"))

inventario_bloques <- dic_ok %>%
  filter(bloque_id %in% bloques_core) %>%
  mutate(
    incluido_nucleo = var %in% vars_indice,
    rol = case_when(
      entra_indice & incluido_nucleo ~ "Núcleo ICVLF",
      !entra_indice ~ "Complementario / proxy",
      TRUE ~ "Excluido por cobertura"
    )
  ) %>%
  group_by(bloque_id, bloque) %>%
  summarise(
    n_disponibles = n(),
    n_candidatos_nucleo = sum(entra_indice),
    n_nucleo_icvlf = sum(entra_indice & incluido_nucleo),
    n_complementarios = sum(!entra_indice),
    n_excluidos_cob = sum(entra_indice & !incluido_nucleo),
    .groups = "drop"
  ) %>%
  mutate(bloque_nombre = etiquetas_bloque[bloque_id])

# Catálogo completo de cobertura para las cinco dimensiones, incluyendo proxies complementarios.
vars_catalogo <- dic_ok %>% filter(bloque_id %in% bloques_core) %>% pull(var)
cobertura_catalogo <- Base_muestra %>%
  summarise(across(all_of(vars_catalogo), ~ mean(is.finite(.) & !is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "cobertura") %>%
  left_join(dic_ok %>% select(var, original, bloque_id, entra_indice, direccion_riesgo),
            by = c("variable" = "var")) %>%
  mutate(
    cobertura_pct = 100 * cobertura,
    indicador = etiquetas_var[variable],
    rol = case_when(
      entra_indice & variable %in% vars_indice ~ "Núcleo ICVLF",
      !entra_indice ~ "Complementario / proxy",
      TRUE ~ "Excluido por cobertura"
    ),
    direccion = ifelse(direccion_riesgo == 1, "+1", "−1"),
    lectura_direccion = ifelse(
      direccion_riesgo == 1,
      "Mayor valor = mayor vulnerabilidad",
      "Mayor valor = menor vulnerabilidad"
    )
  )

indices_subperiodos <- indices %>%
  mutate(subperiodo = case_when(
    Fecha <= as.Date("2014-12-01") ~ "2010–2014",
    Fecha <= as.Date("2019-12-01") ~ "2015–2019",
    Fecha <= as.Date("2021-12-01") ~ "2020–2021",
    TRUE ~ "2022–2025"
  ))

tabla_subperiodos <- indices_subperiodos %>%
  group_by(subperiodo) %>%
  summarise(
    n = n(),
    icvlf_media = mean(ICVLF_equal_100, na.rm = TRUE),
    icvlf_sd = sd(ICVLF_equal_100, na.rm = TRUE),
    icvlf_min = min(ICVLF_equal_100, na.rm = TRUE),
    icvlf_max = max(ICVLF_equal_100, na.rm = TRUE),
    icvlf_sesgo = moments::skewness(na.omit(ICVLF_equal_100)),
    regimen_modal = moda_factor(nivel_vulnerabilidad),
    pct_alta_muy_alta = mean(
      nivel_vulnerabilidad %in% c("Alta", "Muy alta"), na.rm = TRUE
    ) * 100,
    .groups = "drop"
  )

cat("\n  Estadísticas por subperiodo:\n")
print(tabla_subperiodos %>% select(subperiodo, n, icvlf_media, regimen_modal))


# ══════════════════════════════════════════════════════════════
# 13.  PROXIES DE CONSISTENCIA PRUDENCIAL
# ══════════════════════════════════════════════════════════════

cat("\n── 13. Construyendo proxies de consistencia prudencial ──\n")

proxies <- Base_muestra %>% select(Fecha)

# P1 y P2 son proxies de cobertura construidos con la base disponible; NO son el LCR regulatorio de Basilea III.
if ("liq_disp_oblig_cp" %in% names(Base_muestra))
  proxies$P1 <- Base_muestra$liq_disp_oblig_cp
if ("liq_disp_inv_oblig_cp" %in% names(Base_muestra))
  proxies$P2 <- Base_muestra$liq_disp_inv_oblig_cp
if ("liq_activos_pasivos_cp" %in% names(Base_muestra))
  proxies$CLA <- Base_muestra$liq_activos_pasivos_cp

vars_ipfe <- intersect(
  c("fon_personas_naturales", "fon_dias_permanencia_dpf",
    "solv_patrimonio_activo", "fon_oblig_subordinadas_paspat",
    "fon_juridicas_institucionales", "fon_oblig_bancos_paspat"),
  names(Base_muestra)
)

if (length(vars_ipfe) >= 2) {
  X_ipfe <- Base_muestra %>% select(all_of(vars_ipfe))
  ok_ipfe <- complete.cases(X_ipfe)
  X_ok <- X_ipfe[ok_ipfe, , drop = FALSE]
  X_ipfe_z <- as.data.frame(scale(X_ok))
  
  # +1 = mayor estabilidad; -1 = menor estabilidad.
  dir_est <- c(
    fon_personas_naturales = 1,
    fon_dias_permanencia_dpf = 1,
    solv_patrimonio_activo = 1,
    fon_oblig_subordinadas_paspat = 1,
    fon_juridicas_institucionales = -1,
    fon_oblig_bancos_paspat = -1
  )
  if (anyNA(match(names(X_ipfe_z), names(dir_est)))) {
    stop("No se pudo emparejar la orientación del proxy de fondeo estable.")
  }
  dir_est <- dir_est[names(X_ipfe_z)]
  X_ipfe_st <- sweep(as.matrix(X_ipfe_z), 2, dir_est, "*")
  ipfe_raw <- rowMeans(X_ipfe_st)
  
  IPFE_df <- tibble(
    Fecha = Base_muestra$Fecha[ok_ipfe],
    IPFE = as.numeric(scale(ipfe_raw)),
    IPFE_100 = normalizar_0100(ipfe_raw)
  )
  proxies <- proxies %>% left_join(IPFE_df, by = "Fecha")
}

proxies_disponibles <- intersect(c("P1", "P2", "CLA", "IPFE_100"), names(proxies))

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

cat("  Proxies disponibles:", paste(proxies_disponibles, collapse = ", "), "\n")


# ══════════════════════════════════════════════════════════════
# 14.  VALIDACIÓN CONVERGENTE Y CONSISTENCIA PRUDENCIAL
# ══════════════════════════════════════════════════════════════

cat("\n── 14. Validación convergente / consistencia prudencial ──\n")

base_val <- indices %>%
  select(
    Fecha, ICVLF_equal, ICVLF_equal_100, ICVLF_alt_pesos_100, ICVLF_pca_global_100,
    ICVLF_nucleo_100, ICVLF_pca_interno_100, ICVLF_robust_z_100, nivel_vulnerabilidad
  ) %>%
  left_join(proxies, by = "Fecha")

indices_icvlf <- c(
  "ICVLF_equal_100", "ICVLF_alt_pesos_100", "ICVLF_pca_global_100",
  "ICVLF_nucleo_100", "ICVLF_pca_interno_100", "ICVLF_robust_z_100"
)

etiq_icvlf <- c(
  ICVLF_equal_100         = "ICVLF principal: igualdad interna + 20% × 5",
  ICVLF_alt_pesos_100    = "Pesos dimensionales alternativos (35–25–15–15–10)",
  ICVLF_pca_global_100   = "PCA global de subíndices — sensibilidad",
  ICVLF_nucleo_100       = "Benchmark de alcance estrecho: Liquidez + Fondeo (50–50)",
  ICVLF_pca_interno_100  = "Pesos internos PCA — sensibilidad",
  ICVLF_robust_z_100     = "Normalización robusta mediana/MAD — sensibilidad"
)

tabla_correlaciones <- expand.grid(
  indice = indices_icvlf,
  proxy = proxies_disponibles,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    n_obs = map2_int(indice, proxy, ~ safe_n(base_val[[.x]], base_val[[.y]])),
    pearson = map2_dbl(indice, proxy,
                       ~ safe_cor(base_val[[.x]], base_val[[.y]], "pearson")),
    spearman = map2_dbl(indice, proxy,
                        ~ safe_cor(base_val[[.x]], base_val[[.y]], "spearman")),
    intensidad = intensidad_cor(pearson),
    etiq_indice = etiq_icvlf[indice],
    etiq_proxy = etiquetas_proxy[proxy]
  )

# Validación convergente reduciendo solapamiento mecánico con P1/P2.
proxy_var_solapada <- c(P1 = "liq_disp_oblig_cp", P2 = "liq_disp_inv_oblig_cp")
tabla_validacion_sin_solapamiento <- map_dfr(names(proxy_var_solapada), function(px) {
  v <- unname(proxy_var_solapada[px])
  if (!(px %in% names(base_val)) || !(v %in% vars_indice)) return(tibble())
  alt_z <- construir_icvlf_equal_desde_vars(setdiff(vars_indice, v))
  tibble(
    proxy = px,
    indicador_retirado = etiquetas_var[v],
    n_obs = safe_n(alt_z, base_val[[px]]),
    pearson = safe_cor(alt_z, base_val[[px]], "pearson"),
    spearman = safe_cor(alt_z, base_val[[px]], "spearman"),
    lectura = "ICVLF reconstruido sin el indicador que coincide con el proxy"
  )
}) %>% mutate(etiq_proxy = etiquetas_proxy[proxy])

base_val_sp <- base_val %>%
  left_join(indices_subperiodos %>% select(Fecha, subperiodo), by = "Fecha")

tabla_correlaciones_subperiodo <- expand.grid(
  proxy = proxies_disponibles,
  subperiodo = unique(indices_subperiodos$subperiodo),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    n_obs = map2_int(proxy, subperiodo, function(px, sp) {
      df <- base_val_sp %>% filter(subperiodo == sp)
      safe_n(df$ICVLF_equal_100, df[[px]])
    }),
    pearson = map2_dbl(proxy, subperiodo, function(px, sp) {
      df <- base_val_sp %>% filter(subperiodo == sp)
      safe_cor(df$ICVLF_equal_100, df[[px]], "pearson")
    }),
    spearman = map2_dbl(proxy, subperiodo, function(px, sp) {
      df <- base_val_sp %>% filter(subperiodo == sp)
      safe_cor(df$ICVLF_equal_100, df[[px]], "spearman")
    }),
    intensidad = intensidad_cor(pearson),
    etiq_proxy = etiquetas_proxy[proxy]
  )

# Asociación adelantada descriptiva. NO se interpreta como prueba predictiva.
tabla_rezagos <- expand.grid(
  proxy = proxies_disponibles,
  h = c(1, 3, 6),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    n_obs = map2_int(proxy, h, function(px, h_) {
      safe_n(base_val$ICVLF_equal_100, dplyr::lead(base_val[[px]], h_))
    }),
    pearson_h = map2_dbl(proxy, h, function(px, h_) {
      safe_cor(base_val$ICVLF_equal_100, dplyr::lead(base_val[[px]], h_), "pearson")
    }),
    spearman_h = map2_dbl(proxy, h, function(px, h_) {
      safe_cor(base_val$ICVLF_equal_100, dplyr::lead(base_val[[px]], h_), "spearman")
    }),
    intensidad = intensidad_cor(pearson_h),
    etiq_proxy = etiquetas_proxy[proxy],
    horizonte = paste0(h, " mes", ifelse(h == 1, "", "es")),
    nota = "Asociación adelantada descriptiva; no constituye validación predictiva"
  )

# Regresión de consistencia con errores Newey-West (HAC) a 12 rezagos.
# Se usa como inferencia complementaria; no transforma la relación en causal.
tabla_hac_consistencia <- map_dfr(proxies_disponibles, function(px) {
  df <- base_val %>%
    select(ICVLF_equal, all_of(px)) %>%
    rename(proxy = all_of(px)) %>%
    filter(complete.cases(.), is.finite(ICVLF_equal), is.finite(proxy))
  
  if (nrow(df) < max(24, lag_hac + 5)) {
    return(tibble(
      proxy = px, n_obs = nrow(df), beta_icvlf = NA_real_, se_hac = NA_real_,
      t_hac = NA_real_, p_hac = NA_real_, r2 = NA_real_
    ))
  }
  
  fit <- lm(proxy ~ ICVLF_equal, data = df)
  V <- sandwich::NeweyWest(fit, lag = lag_hac, prewhite = FALSE, adjust = TRUE)
  ct <- lmtest::coeftest(fit, vcov. = V)
  tibble(
    proxy = px,
    n_obs = nrow(df),
    beta_icvlf = unname(coef(fit)["ICVLF_equal"]),
    se_hac = unname(ct["ICVLF_equal", "Std. Error"]),
    t_hac = unname(ct["ICVLF_equal", "t value"]),
    p_hac = unname(ct["ICVLF_equal", "Pr(>|t|)"]),
    r2 = summary(fit)$r.squared
  )
}) %>%
  mutate(
    etiq_proxy = etiquetas_proxy[proxy],
    nota = "Inferencia HAC Newey-West; asociación, no causalidad"
  )

# ── Estacionariedad ICVLF/proxies y HAC en primeras diferencias ─────────────
# ADF y PP: H0 = raíz unitaria. KPSS: H0 = estacionariedad de nivel.
test_estacionariedad_serie <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) < 25 || sd(x) == 0) {
    return(tibble(
      n = length(x),
      adf_stat = NA_real_, adf_p = NA_real_,
      pp_stat = NA_real_, pp_p = NA_real_,
      kpss_level_stat = NA_real_, kpss_level_p = NA_real_,
      kpss_trend_stat = NA_real_, kpss_trend_p = NA_real_
    ))
  }
  
  adf <- tryCatch(suppressWarnings(tseries::adf.test(x, alternative = "stationary")), error = function(e) NULL)
  pp  <- tryCatch(suppressWarnings(tseries::pp.test(x, alternative = "stationary")), error = function(e) NULL)
  kp_l <- tryCatch(suppressWarnings(tseries::kpss.test(x, null = "Level")), error = function(e) NULL)
  kp_t <- tryCatch(suppressWarnings(tseries::kpss.test(x, null = "Trend")), error = function(e) NULL)
  
  tibble(
    n = length(x),
    adf_stat = if (is.null(adf)) NA_real_ else as.numeric(adf$statistic),
    adf_p = if (is.null(adf)) NA_real_ else as.numeric(adf$p.value),
    pp_stat = if (is.null(pp)) NA_real_ else as.numeric(pp$statistic),
    pp_p = if (is.null(pp)) NA_real_ else as.numeric(pp$p.value),
    kpss_level_stat = if (is.null(kp_l)) NA_real_ else as.numeric(kp_l$statistic),
    kpss_level_p = if (is.null(kp_l)) NA_real_ else as.numeric(kp_l$p.value),
    kpss_trend_stat = if (is.null(kp_t)) NA_real_ else as.numeric(kp_t$statistic),
    kpss_trend_p = if (is.null(kp_t)) NA_real_ else as.numeric(kp_t$p.value)
  )
}

series_estacionariedad <- c("ICVLF_equal", proxies_disponibles)
tabla_estacionariedad_icvlf_proxies <- map_dfr(series_estacionariedad, function(nm) {
  x <- base_val[[nm]]
  lvl <- test_estacionariedad_serie(x) %>% mutate(transformacion = "Nivel")
  dx <- diff(x)
  dif <- test_estacionariedad_serie(dx) %>% mutate(transformacion = "Primera diferencia")
  bind_rows(lvl, dif) %>% mutate(serie = nm, .before = 1)
}) %>%
  mutate(
    evidencia_estacionaria = case_when(
      is.na(adf_p) | is.na(pp_p) | is.na(kpss_level_p) ~ "S/D",
      adf_p < 0.05 & pp_p < 0.05 & kpss_level_p >= 0.05 ~ "Convergente: estacionaria",
      adf_p >= 0.05 & pp_p >= 0.05 & kpss_level_p < 0.05 ~ "Convergente: no estacionaria",
      TRUE ~ "Mixta / inconclusa"
    ),
    evidencia_kpss_tendencia = case_when(
      is.na(kpss_trend_p) ~ "S/D",
      kpss_trend_p >= 0.05 ~ "No rechaza estacionariedad alrededor de tendencia",
      TRUE ~ "Rechaza estacionariedad alrededor de tendencia"
    )
  )

tabla_integracion_resumen <- tabla_estacionariedad_icvlf_proxies %>%
  select(serie, transformacion, evidencia_estacionaria) %>%
  pivot_wider(names_from = transformacion, values_from = evidencia_estacionaria) %>%
  mutate(
    lectura = case_when(
      `Nivel` == "Convergente: estacionaria" ~ "Compatible con I(0)",
      `Nivel` == "Convergente: no estacionaria" & `Primera diferencia` == "Convergente: estacionaria" ~ "Compatible con I(1)",
      TRUE ~ "Resultado mixto: interpretar con cautela"
    )
  )

tabla_hac_diferencias <- map_dfr(proxies_disponibles, function(px) {
  df <- base_val %>% select(Fecha, ICVLF_equal, all_of(px)) %>%
    rename(proxy = all_of(px)) %>% arrange(Fecha) %>%
    mutate(d_icvlf = ICVLF_equal - lag(ICVLF_equal),
           d_proxy = proxy - lag(proxy)) %>%
    filter(complete.cases(d_icvlf, d_proxy), is.finite(d_icvlf), is.finite(d_proxy))
  if (nrow(df) < max(24, lag_hac + 5)) {
    return(tibble(proxy = px, n_obs = nrow(df), beta_delta = NA_real_, se_hac = NA_real_,
                  t_hac = NA_real_, p_hac = NA_real_, r2 = NA_real_))
  }
  fit <- lm(d_proxy ~ d_icvlf, data = df)
  V <- sandwich::NeweyWest(fit, lag = lag_hac, prewhite = FALSE, adjust = TRUE)
  ct <- lmtest::coeftest(fit, vcov. = V)
  tibble(
    proxy = px, n_obs = nrow(df), beta_delta = unname(coef(fit)["d_icvlf"]),
    se_hac = unname(ct["d_icvlf", "Std. Error"]),
    t_hac = unname(ct["d_icvlf", "t value"]),
    p_hac = unname(ct["d_icvlf", "Pr(>|t|)"]), r2 = summary(fit)$r.squared
  )
}) %>% mutate(
  etiq_proxy = etiquetas_proxy[proxy],
  nota = "HAC Newey-West sobre primeras diferencias del ICVLF analítico z; asociación de corto plazo, no causalidad"
)

winsorizar <- function(x, probs = winsor_prob) {
  q <- quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  pmin(pmax(x, q[1]), q[2])
}

tabla_hac_diferencias_winsor <- map_dfr(proxies_disponibles, function(px) {
  df <- base_val %>%
    select(Fecha, ICVLF_equal, all_of(px)) %>%
    rename(proxy = all_of(px)) %>%
    arrange(Fecha) %>%
    mutate(
      d_icvlf = ICVLF_equal - lag(ICVLF_equal),
      d_proxy = proxy - lag(proxy)
    ) %>%
    filter(complete.cases(d_icvlf, d_proxy), is.finite(d_icvlf), is.finite(d_proxy))
  
  if (nrow(df) < max(24, lag_hac + 5)) {
    return(tibble(
      proxy = px, n_obs = nrow(df), beta_delta_w = NA_real_,
      se_hac_w = NA_real_, t_hac_w = NA_real_, p_hac_w = NA_real_, r2_w = NA_real_,
      n_cook_gt_4n = NA_integer_, max_cook = NA_real_
    ))
  }
  
  fit_base <- lm(d_proxy ~ d_icvlf, data = df)
  cook <- cooks.distance(fit_base)
  
  dfw <- df %>%
    mutate(
      d_icvlf_w = winsorizar(d_icvlf),
      d_proxy_w = winsorizar(d_proxy)
    )
  
  fit <- lm(d_proxy_w ~ d_icvlf_w, data = dfw)
  V <- sandwich::NeweyWest(fit, lag = lag_hac, prewhite = FALSE, adjust = TRUE)
  ct <- lmtest::coeftest(fit, vcov. = V)
  
  tibble(
    proxy = px,
    n_obs = nrow(df),
    beta_delta_w = unname(coef(fit)["d_icvlf_w"]),
    se_hac_w = unname(ct["d_icvlf_w", "Std. Error"]),
    t_hac_w = unname(ct["d_icvlf_w", "t value"]),
    p_hac_w = unname(ct["d_icvlf_w", "Pr(>|t|)"]),
    r2_w = summary(fit)$r.squared,
    n_cook_gt_4n = sum(cook > 4 / nrow(df), na.rm = TRUE),
    max_cook = max(cook, na.rm = TRUE)
  )
}) %>%
  mutate(
    etiq_proxy = etiquetas_proxy[proxy],
    nota = "Sensibilidad 1%-99%; no sustituye la estimación principal y no altera los datos originales."
  )

tabla_regimenes <- base_val %>%
  filter(!is.na(nivel_vulnerabilidad)) %>%
  group_by(nivel_vulnerabilidad) %>%
  summarise(
    across(all_of(proxies_disponibles), ~ safe_mean(.), .names = "media_{.col}"),
    n = n(), .groups = "drop"
  )

tabla_robustez <- cor(
  base_val %>% select(all_of(indices_icvlf)),
  use = "pairwise.complete.obs"
)

indices_robustez_metodologica <- setdiff(indices_icvlf, "ICVLF_nucleo_100")
tabla_robustez_metodologica <- cor(
  base_val %>% select(all_of(indices_robustez_metodologica)),
  use = "pairwise.complete.obs"
)

tabla_benchmark_alcance <- tibble(
  benchmark = "Liquidez + Fondeo (50/50)",
  pearson_vs_icvlf_integral = safe_cor(
    base_val$ICVLF_equal_100, base_val$ICVLF_nucleo_100, "pearson"
  ),
  spearman_vs_icvlf_integral = safe_cor(
    base_val$ICVLF_equal_100, base_val$ICVLF_nucleo_100, "spearman"
  ),
  lectura = paste0(
    "Benchmark de constructo estrecho; una correlación menor no es un fallo de robustez, ",
    "porque elimina Activos, Cartera y Solvencia."
  )
)

cat("  Correlaciones globales ICVLF Equal vs proxies:\n")
print(tabla_correlaciones %>%
        filter(indice == "ICVLF_equal_100") %>%
        select(proxy, n_obs, pearson, spearman, intensidad))


# ══════════════════════════════════════════════════════════════
# 15.  ANÁLISIS ROLLING
# ══════════════════════════════════════════════════════════════

cat("\n── 15. Rolling windows ──\n")

tabla_cobertura_proxies <- map_dfr(proxies_disponibles, function(px) {
  x <- proxies[[px]]
  ok <- is.finite(x) & !is.na(x)
  tibble(
    proxy = px,
    descripcion = etiquetas_proxy[px],
    n = sum(ok),
    cobertura_pct = 100 * mean(ok),
    fecha_inicio = if (any(ok)) min(proxies$Fecha[ok]) else as.Date(NA),
    fecha_fin = if (any(ok)) max(proxies$Fecha[ok]) else as.Date(NA)
  )
})

proxies_rolling_q <- tabla_cobertura_proxies %>%
  filter(n >= min_obs_proxy_rolling_q) %>%
  pull(proxy)

proxies_rolling_s <- tabla_cobertura_proxies %>%
  filter(n >= ventana_rolling_s) %>%
  pull(proxy)

calcular_rolling <- function(df_i, df_v, prx, ventana, min_frac = min_frac_rolling) {
  rc <- tibble(Fecha = df_i$Fecha)
  
  for (px in prx) {
    pv <- df_v[[px]]
    iv <- df_v$ICVLF_equal
    
    pares <- zoo::rollapply(
      data.frame(x = iv, y = pv),
      width = ventana,
      FUN = function(m) sum(complete.cases(m)),
      by.column = FALSE, fill = NA, align = "right"
    )
    
    r <- zoo::rollapply(
      data.frame(x = iv, y = pv),
      width = ventana,
      FUN = function(m) {
        ok <- complete.cases(m)
        if (sum(ok) < ceiling(ventana * min_frac)) return(NA_real_)
        cor(m[ok, "x"], m[ok, "y"])
      },
      by.column = FALSE, fill = NA, align = "right"
    )
    
    rc[[paste0("n_", px)]] <- as.integer(pares)
    rc[[paste0("r_", px)]] <- as.numeric(r)
  }
  
  rb <- tibble(Fecha = df_i$Fecha)
  for (bl in bloques_core) {
    rb[[paste0("rmean_", bl)]] <- as.numeric(
      zoo::rollapply(
        df_i[[bl]], width = ventana,
        FUN = function(x) if (sum(is.finite(x)) < ceiling(ventana * min_frac)) NA_real_ else mean(x, na.rm = TRUE),
        fill = NA, align = "right"
      )
    )
  }
  
  rb$rolling_sd_icvlf <- as.numeric(zoo::rollapply(
    df_i$ICVLF_equal, width = ventana,
    FUN = function(x) if (sum(is.finite(x)) < ceiling(ventana * min_frac)) NA_real_ else sd(x, na.rm = TRUE),
    fill = NA, align = "right"
  ))
  
  ic <- df_i$ICVLF_equal
  beta_ac <- zoo::rollapply(
    data.frame(y = ic[-1], x = ic[-length(ic)]),
    width = ventana - 1,
    FUN = function(m) {
      ok <- complete.cases(m)
      if (sum(ok) < ceiling((ventana - 1) * min_frac)) return(NA_real_)
      coef(lm(y ~ x, data = as.data.frame(m[ok, , drop = FALSE])))[["x"]]
    },
    by.column = FALSE, fill = NA, align = "right"
  )
  rb$beta_autocorrelacion <- c(NA_real_, as.numeric(beta_ac))
  
  list(cor = rc, bloques = rb, proxies = prx, ventana = ventana)
}

rolling_q <- calcular_rolling(indices, base_val, proxies_rolling_q, ventana_rolling_q)
rolling_s <- calcular_rolling(indices, base_val, proxies_rolling_s, ventana_rolling_s)

cat("  Proxies rolling 60m:", ifelse(length(proxies_rolling_q)==0, "ninguno", paste(proxies_rolling_q, collapse=", ")), "\n")
cat("  Proxies rolling 24m:", ifelse(length(proxies_rolling_s)==0, "ninguno", paste(proxies_rolling_s, collapse=", ")), "\n")
cat("  Regla mínima dentro de ventana:", min_frac_rolling * 100, "% de pares completos.\n")

# ══════════════════════════════════════════════════════════════
# 15B. ROBUSTEZ ESTRUCTURAL: LEAVE-ONE-OUT
# ══════════════════════════════════════════════════════════════

cat("\n── 15B. Robustez leave-one-out ──\n")

tabla_robustez_leave1_indicador <- map_dfr(vars_indice, function(v_omitida) {
  alt <- construir_icvlf_equal_desde_vars(setdiff(vars_indice, v_omitida))
  tibble(
    variable_omitida = v_omitida,
    indicador_omitido = etiquetas_var[v_omitida],
    bloque = dic_ok$bloque_id[match(v_omitida, dic_ok$var)],
    n_comparables = safe_n(indices$ICVLF_equal, alt),
    pearson_vs_base = safe_cor(indices$ICVLF_equal, alt, "pearson"),
    spearman_vs_base = safe_cor(indices$ICVLF_equal, alt, "spearman"),
    dam_z = mean(abs(indices$ICVLF_equal - alt), na.rm = TRUE)
  )
}) %>% arrange(pearson_vs_base)

tabla_robustez_leave1_dimension <- map_dfr(bloques_core, function(bl_omitido) {
  bl_keep <- setdiff(bloques_core, bl_omitido)
  alt <- rowMeans(subindices[, bl_keep], na.rm = TRUE)
  tibble(
    dimension_omitida = bl_omitido,
    dimension = etiquetas_bloque_corto[bl_omitido],
    n_comparables = safe_n(indices$ICVLF_equal, alt),
    pearson_vs_base = safe_cor(indices$ICVLF_equal, alt, "pearson"),
    spearman_vs_base = safe_cor(indices$ICVLF_equal, alt, "spearman"),
    dam_z = mean(abs(indices$ICVLF_equal - alt), na.rm = TRUE)
  )
}) %>% arrange(pearson_vs_base)

tabla_sensibilidad_pesos_internos <- tibble(
  especificacion = "Pesos internos iguales (principal) vs pesos internos PCA (sensibilidad)",
  pearson = safe_cor(indices$ICVLF_equal, indices$ICVLF_pca_interno, "pearson"),
  spearman = safe_cor(indices$ICVLF_equal, indices$ICVLF_pca_interno, "spearman"),
  dam_z = mean(abs(indices$ICVLF_equal - indices$ICVLF_pca_interno), na.rm = TRUE)
)

tabla_sensibilidad_normalizacion <- tibble(
  especificacion = "z clásico (principal) vs z robusto mediana/MAD",
  pearson = safe_cor(indices$ICVLF_equal, indices$ICVLF_robust_z, "pearson"),
  spearman = safe_cor(indices$ICVLF_equal, indices$ICVLF_robust_z, "spearman"),
  dam_z = mean(abs(indices$ICVLF_equal - indices$ICVLF_robust_z), na.rm = TRUE)
)

# Sensibilidad financiera conjunta: se retiran simultáneamente los indicadores
# cuya dirección de riesgo depende más del contexto financiero. Esto prueba que
# la conclusión no descansa en supuestos conceptuales discutibles.
vars_signo_sensible <- auditoria_financiera_signos %>%
  filter(sensibilidad_especifica, variable %in% vars_indice) %>%
  pull(variable)

icvlf_sin_signos_sensibles <- construir_icvlf_equal_desde_vars(
  setdiff(vars_indice, vars_signo_sensible)
)

tabla_sensibilidad_signos_financieros <- tibble(
  especificacion = "Excluir simultáneamente indicadores con signo/contexto más discutible",
  variables_excluidas = paste(vars_signo_sensible, collapse = "; "),
  n_variables_excluidas = length(vars_signo_sensible),
  pearson = safe_cor(indices$ICVLF_equal, icvlf_sin_signos_sensibles, "pearson"),
  spearman = safe_cor(indices$ICVLF_equal, icvlf_sin_signos_sensibles, "spearman"),
  dam_z = mean(abs(indices$ICVLF_equal - icvlf_sin_signos_sensibles), na.rm = TRUE)
)

cat("  Menor correlación leave-one-indicator-out: ",
    round(min(tabla_robustez_leave1_indicador$pearson_vs_base, na.rm = TRUE), 3), "\n", sep = "")
cat("  Menor correlación leave-one-dimension-out: ",
    round(min(tabla_robustez_leave1_dimension$pearson_vs_base, na.rm = TRUE), 3), "\n", sep = "")

# ══════════════════════════════════════════════════════════════
# 16.  RUPTURAS ESTRUCTURALES — BAI-PERRON, BIC E IC 95%
# ══════════════════════════════════════════════════════════════

cat("\n── 16. Rupturas estructurales Bai-Perron ──\n")

rupturas_df <- tibble(
  numero = integer(),
  fecha = as.Date(character()),
  ic95_inf = as.Date(character()),
  ic95_sup = as.Date(character()),
  ancho_ic_meses_aprox = numeric(),
  ic_degenerado = logical()
)
tabla_bic_rupturas <- tibble(n_rupturas = integer(), BIC = numeric())
auditoria_confint_bai_perron <- list()

tryCatch({
  # Se estima sobre la escala analítica z. Una transformación lineal 0-100 no cambia las fechas,
  # pero z mantiene coherencia con el motor estadístico.
  bp_full <- strucchange::breakpoints(ICVLF_equal ~ 1, data = indices, h = 0.15)
  sm_bp <- summary(bp_full)
  
  if (!is.null(sm_bp$RSS) && "BIC" %in% rownames(sm_bp$RSS)) {
    tabla_bic_rupturas <- tibble(
      n_rupturas = suppressWarnings(as.integer(colnames(sm_bp$RSS))),
      BIC = as.numeric(sm_bp$RSS["BIC", ])
    ) %>%
      filter(is.finite(BIC), !is.na(n_rupturas))
  }
  
  if (nrow(tabla_bic_rupturas) == 0) {
    stop("No fue posible extraer la trayectoria BIC de breakpoints().")
  }
  
  m_opt <- tabla_bic_rupturas$n_rupturas[which.min(tabla_bic_rupturas$BIC)]
  bp_opt <- strucchange::breakpoints(bp_full, breaks = m_opt)
  bp_pos <- bp_opt$breakpoints[!is.na(bp_opt$breakpoints)]
  
  if (m_opt > 0 && length(bp_pos) > 0) {
    ci_obj <- confint(bp_full, breaks = m_opt, level = 0.95)
    auditoria_confint_bai_perron$clase <- class(ci_obj)
    auditoria_confint_bai_perron$estructura <- capture.output(str(ci_obj))
    
    ci_mat <- if (is.matrix(ci_obj)) {
      ci_obj
    } else if (!is.null(ci_obj$confint)) {
      as.matrix(ci_obj$confint)
    } else {
      tryCatch(as.matrix(ci_obj), error = function(e) NULL)
    }
    
    if (is.null(ci_mat) || ncol(ci_mat) < 3 || nrow(ci_mat) != length(bp_pos)) {
      warning(
        "No se pudo verificar de forma segura la estructura de confint.breakpointsfull. ",
        "Se reportan fechas puntuales y los IC quedan como NA hasta revisar el objeto crudo."
      )
      pos_bp <- bp_pos
      pos_inf <- rep(NA_integer_, length(bp_pos))
      pos_sup <- rep(NA_integer_, length(bp_pos))
    } else {
      ci_num <- apply(ci_mat[, 1:3, drop = FALSE], 2, as.numeric)
      pos_inf <- pmax(1L, pmin(nrow(indices), as.integer(round(ci_num[, 1]))))
      pos_bp  <- pmax(1L, pmin(nrow(indices), as.integer(round(ci_num[, 2]))))
      pos_sup <- pmax(1L, pmin(nrow(indices), as.integer(round(ci_num[, 3]))))
    }
    
    fecha_inf <- ifelse(is.na(pos_inf), NA, as.character(indices$Fecha[pos_inf]))
    fecha_bp  <- as.character(indices$Fecha[pos_bp])
    fecha_sup <- ifelse(is.na(pos_sup), NA, as.character(indices$Fecha[pos_sup]))
    
    rupturas_df <- tibble(
      numero = seq_along(pos_bp),
      fecha = as.Date(fecha_bp),
      ic95_inf = as.Date(fecha_inf),
      ic95_sup = as.Date(fecha_sup)
    ) %>%
      mutate(
        ancho_ic_meses_aprox = ifelse(
          is.na(ic95_inf) | is.na(ic95_sup),
          NA_real_,
          as.numeric(ic95_sup - ic95_inf) / 30.4375
        ),
        ic_degenerado = !is.na(ancho_ic_meses_aprox) & ancho_ic_meses_aprox == 0
      )
    
    if (nrow(rupturas_df) > 0 && all(rupturas_df$ic_degenerado, na.rm = TRUE)) {
      warning(
        "Todos los IC95% Bai-Perron tienen ancho cero. ",
        "No deben presentarse como evidencia de precisión extrema sin inspeccionar auditoria_confint_bai_perron."
      )
    }
  }
  
  cat("  Número de rupturas BIC mínimo:", m_opt, "\n")
  cat("  Fechas estimadas:",
      ifelse(nrow(rupturas_df) == 0, "ninguna",
             paste(format(rupturas_df$fecha, "%Y-%m"), collapse = ", ")), "\n")
}, error = function(e) {
  cat("  ⚠ Bai-Perron:", conditionMessage(e), "\n")
})

# ══════════════════════════════════════════════════════════════
# 17.  ESCENARIOS CONTRAFACTUALES DE ESTRÉS
# ══════════════════════════════════════════════════════════════

cat("\n── 17. Escenarios contrafactuales de estrés ──\n")

ultimo <- indices %>% slice_tail(n = 1)
base_ult <- as.numeric(unlist(ultimo[1, bloques_core], use.names = FALSE))
names(base_ult) <- bloques_core
base_icvlf_z <- mean(base_ult, na.rm = TRUE)

q_adverso <- vapply(
  subindices[bloques_core], quantile, numeric(1),
  probs = percentil_estres, na.rm = TRUE, names = FALSE
)

calc_stress_q <- function(bloques_afectados) {
  x <- base_ult
  for (bl in bloques_afectados) x[bl] <- max(x[bl], q_adverso[bl], na.rm = TRUE)
  mean(x, na.rm = TRUE)
}

escenarios_q <- list(
  E0 = character(0), E1 = "liquidez", E2 = "fondeo", E3 = "activos",
  E4 = "cartera", E5 = "solvencia", E6 = bloques_core
)

nombres_esc <- c(
  paste0("E0 — Estado observado (", format(ultimo$Fecha, "%B %Y"), ")"),
  "E1 — Choque adverso en liquidez",
  "E2 — Choque adverso en fondeo",
  "E3 — Choque adverso en composición de activos",
  "E4 — Choque adverso en presión de cartera",
  "E5 — Choque adverso en solvencia/capacidad de absorción",
  "E6 — Choque adverso conjunto en las cinco dimensiones"
)

tabla_estres <- tibble(
  codigo = names(escenarios_q), escenario = nombres_esc,
  dimensiones_afectadas = vapply(
    escenarios_q, function(x) if (length(x) == 0) "Ninguna" else paste(x, collapse = ", "), character(1)
  ),
  calibracion = c("Estado observado", rep(paste0("Percentil histórico ", percentil_estres * 100, " del subíndice"), 6)),
  ICVLF_simulado_z = vapply(
    escenarios_q, function(bl) if (length(bl) == 0) base_icvlf_z else calc_stress_q(bl), numeric(1)
  )
) %>%
  mutate(
    delta_z = ICVLF_simulado_z - first(ICVLF_simulado_z),
    ICVLF_simulado_100_ref = escalar_0100_con_referencia(ICVLF_simulado_z, indices$ICVLF_equal),
    tipo = "Contrafactual Q95"
  )

# Stress histórico observado: conserva la dependencia real entre dimensiones.
pos_peor <- which.max(indices$ICVLF_equal)
vector_peor_hist <- as.numeric(unlist(indices[pos_peor, bloques_core], use.names = FALSE))
icvlf_peor_hist <- mean(vector_peor_hist, na.rm = TRUE)
tabla_estres_historico <- tibble(
  codigo = "EH",
  escenario = paste0("EH — Peor configuración conjunta observada (", format(indices$Fecha[pos_peor], "%B %Y"), ")"),
  dimensiones_afectadas = "Configuración conjunta observada",
  calibracion = "Mes histórico de máximo ICVLF",
  ICVLF_simulado_z = icvlf_peor_hist,
  delta_z = icvlf_peor_hist - base_icvlf_z,
  ICVLF_simulado_100_ref = escalar_0100_con_referencia(icvlf_peor_hist, indices$ICVLF_equal),
  tipo = "Histórico observado"
)
tabla_estres <- bind_rows(tabla_estres, tabla_estres_historico)

# Sensibilidad +1.5 d.e. simétrica para las cinco dimensiones y choque conjunto.
calc_stress_de <- function(bloques_afectados, shock = shock_sensibilidad_de) {
  x <- base_ult
  if (length(bloques_afectados) > 0) x[bloques_afectados] <- x[bloques_afectados] + shock
  mean(x, na.rm = TRUE)
}

tabla_estres_sensibilidad_de <- tibble(
  codigo = names(escenarios_q), escenario = nombres_esc,
  dimensiones_afectadas = vapply(
    escenarios_q, function(x) if (length(x) == 0) "Ninguna" else paste(x, collapse = ", "), character(1)
  ),
  calibracion = c("Estado observado", rep(paste0("+", shock_sensibilidad_de, " d.e. sobre subíndice orientado"), 6)),
  ICVLF_simulado_z = vapply(
    escenarios_q, function(bl) if (length(bl) == 0) base_icvlf_z else calc_stress_de(bl), numeric(1)
  )
) %>%
  mutate(
    delta_z = ICVLF_simulado_z - first(ICVLF_simulado_z),
    ICVLF_simulado_100_ref = escalar_0100_con_referencia(ICVLF_simulado_z, indices$ICVLF_equal)
  )

tabla_calibracion_estres_dim <- tibble(
  dimension = bloques_core,
  dimension_label = etiquetas_bloque_corto[bloques_core],
  estado_actual_z = as.numeric(base_ult[bloques_core]),
  q95_historico_z = as.numeric(q_adverso[bloques_core])
) %>%
  mutate(
    brecha_hasta_q95 = pmax(q95_historico_z - estado_actual_z, 0),
    aporte_delta_icvlf_z = brecha_hasta_q95 / length(bloques_core),
    nota = "La brecha mide distancia al Q95 histórico, no importancia estructural ni probabilidad."
  )

print(tabla_estres %>% select(codigo, escenario, ICVLF_simulado_z, delta_z, ICVLF_simulado_100_ref))

# ══════════════════════════════════════════════════════════════
# 18.  CONTRIBUCIONES POR DIMENSIÓN
# ══════════════════════════════════════════════════════════════

contribuciones <- indices %>%
  select(Fecha, all_of(bloques_core)) %>%
  mutate(across(all_of(bloques_core), ~ .x / length(bloques_core))) %>%
  pivot_longer(-Fecha, names_to = "bloque", values_to = "contribucion") %>%
  left_join(indices_subperiodos %>% select(Fecha, subperiodo), by = "Fecha") %>%
  mutate(
    bloque_label = etiquetas_bloque_corto[bloque],
    bloque_label = factor(
      bloque_label, levels = etiquetas_bloque_corto[bloques_core]
    )
  )

contrib_subperiodos <- contribuciones %>%
  group_by(subperiodo, bloque) %>%
  summarise(
    contribucion_media = mean(contribucion, na.rm = TRUE),
    contribucion_max = max(contribucion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    bloque_label = etiquetas_bloque_corto[bloque],
    bloque_label = factor(
      bloque_label, levels = etiquetas_bloque_corto[bloques_core]
    )
  )

tabla_contribuciones_control <- contrib_subperiodos %>%
  select(subperiodo, bloque, contribucion_media) %>%
  pivot_wider(names_from = bloque, values_from = contribucion_media) %>%
  mutate(
    suma_contribuciones = rowSums(across(all_of(bloques_core))),
    icvlf_medio_z = vapply(
      subperiodo,
      function(sp) mean(indices_subperiodos$ICVLF_equal[indices_subperiodos$subperiodo == sp], na.rm = TRUE),
      numeric(1)
    ),
    error_identidad = suma_contribuciones - icvlf_medio_z
  )

if (max(abs(tabla_contribuciones_control$error_identidad), na.rm = TRUE) > 1e-10) {
  stop("La descomposición por contribuciones no reproduce exactamente el ICVLF medio.")
}



# ══════════════════════════════════════════════════════════════
# 19.  AUDITORÍAS NUMÉRICAS COMPLEMENTARIAS DEL BLOQUE 1
# ══════════════════════════════════════════════════════════════

tabla_extremos_icvlf <- bind_rows(
  indices %>%
    slice_max(ICVLF_equal_100, n = 10, with_ties = FALSE) %>%
    transmute(tipo = "Máximos históricos", Fecha, ICVLF_equal_z = ICVLF_equal,
              ICVLF_equal_100, nivel_vulnerabilidad),
  indices %>%
    slice_min(ICVLF_equal_100, n = 10, with_ties = FALSE) %>%
    transmute(tipo = "Mínimos históricos", Fecha, ICVLF_equal_z = ICVLF_equal,
              ICVLF_equal_100, nivel_vulnerabilidad)
)

resumen_serie_rolling <- function(fecha, x, nombre) {
  ok <- is.finite(x) & !is.na(x)
  if (!any(ok)) {
    return(tibble(
      serie = nombre, n = 0, media = NA_real_, minimo = NA_real_,
      fecha_min = as.Date(NA), maximo = NA_real_, fecha_max = as.Date(NA),
      ultimo = NA_real_, fecha_ultimo = as.Date(NA)
    ))
  }
  ix <- which(ok)
  tibble(
    serie = nombre,
    n = length(ix),
    media = mean(x[ix]),
    minimo = min(x[ix]),
    fecha_min = fecha[ix[which.min(x[ix])]],
    maximo = max(x[ix]),
    fecha_max = fecha[ix[which.max(x[ix])]],
    ultimo = x[tail(ix, 1)],
    fecha_ultimo = fecha[tail(ix, 1)]
  )
}

tabla_resumen_rolling_cor_60m <- if (length(proxies_rolling_q) > 0) {
  map_dfr(proxies_rolling_q, function(px) {
    resumen_serie_rolling(
      rolling_q$cor$Fecha,
      rolling_q$cor[[paste0("r_", px)]],
      paste0("ICVLF vs ", px, " — 60m")
    )
  })
} else tibble()

tabla_resumen_rolling_cor_24m <- if (length(proxies_rolling_s) > 0) {
  map_dfr(proxies_rolling_s, function(px) {
    resumen_serie_rolling(
      rolling_s$cor$Fecha,
      rolling_s$cor[[paste0("r_", px)]],
      paste0("ICVLF vs ", px, " — 24m")
    )
  })
} else tibble()

tabla_resumen_autocor_60m <- resumen_serie_rolling(
  rolling_q$bloques$Fecha,
  rolling_q$bloques$beta_autocorrelacion,
  "Beta AR(1) rolling 60 meses"
)

# ══════════════════════════════════════════════════════════════
# 20.  SALIDA MAESTRA: CONSOLA + TXT + RDS PARA BLOQUE 2
# ══════════════════════════════════════════════════════════════

ruta_salida_maestra <- file.path(
  carpeta_salida, "logs", "SALIDA_MAESTRA_ICVLF_v6_3_BLOQUE1.txt"
)
ruta_objetos <- file.path(
  carpeta_salida, "diagnosticos", "OBJETOS_ICVLF_v6_3_BLOQUE1.rds"
)

imprimir_seccion <- function(titulo) {
  cat("\n", strrep("=", 110), "\n", titulo, "\n", strrep("=", 110), "\n", sep = "")
}

imprimir_objeto <- function(x, digits = 6) {
  if (is.null(x)) {
    cat("NULL\n")
  } else if (is.matrix(x)) {
    print(round(x, digits))
  } else {
    print(x, n = Inf, width = Inf)
  }
}

objetos_bloque1 <- list(
  metadata_ejecucion = metadata_ejecucion,
  auditoria_diccionario = auditoria_diccionario,
  auditoria_no_finitos = auditoria_no_finitos,
  auditoria_parseo = auditoria_parseo,
  cobertura_vars = cobertura_vars,
  tabla_marco_dimensiones = tabla_marco_dimensiones,
  tabla_orientacion_riesgo = tabla_orientacion_riesgo,
  estadisticas_ext = estadisticas_ext,
  test_adf_indicadores = test_adf,
  tabla_cor_bloques = tabla_cor_bloques,
  matrices_cor_bloque = matrices_cor_bloque,
  tabla_metodos_subindices = tabla_metodos_subindices,
  tabla_pesos_internos = tabla_pesos_internos,
  tabla_pesos_jerarquicos = tabla_pesos_jerarquicos,
  auditoria_reponderacion_interna = auditoria_reponderacion_interna,
  tabla_pesos_pca_internos = tabla_pesos_pca_internos,
  tabla_pesos_pca_bloques = tabla_pesos_pca_bloques,
  subindices = subindices,
  subindices_brutos = subindices_brutos,
  indices = indices,
  tabla_subperiodos = tabla_subperiodos,
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
  tabla_robustez = tabla_robustez,
  tabla_robustez_metodologica = tabla_robustez_metodologica,
  tabla_benchmark_alcance = tabla_benchmark_alcance,
  tabla_robustez_leave1_indicador = tabla_robustez_leave1_indicador,
  tabla_robustez_leave1_dimension = tabla_robustez_leave1_dimension,
  tabla_sensibilidad_pesos_internos = tabla_sensibilidad_pesos_internos,
  tabla_sensibilidad_normalizacion = tabla_sensibilidad_normalizacion,
  tabla_sensibilidad_signos_financieros = tabla_sensibilidad_signos_financieros,
  tabla_bic_rupturas = tabla_bic_rupturas,
  rupturas_df = rupturas_df,
  auditoria_confint_bai_perron = auditoria_confint_bai_perron,
  tabla_calibracion_estres_dim = tabla_calibracion_estres_dim,
  tabla_estres = tabla_estres,
  tabla_estres_sensibilidad_de = tabla_estres_sensibilidad_de,
  contribuciones = contribuciones,
  contrib_subperiodos = contrib_subperiodos,
  tabla_contribuciones_control = tabla_contribuciones_control,
  rolling_q = rolling_q,
  rolling_s = rolling_s,
  tabla_resumen_rolling_cor_60m = tabla_resumen_rolling_cor_60m,
  tabla_resumen_rolling_cor_24m = tabla_resumen_rolling_cor_24m,
  tabla_resumen_autocor_60m = tabla_resumen_autocor_60m,
  tabla_extremos_icvlf = tabla_extremos_icvlf
)

saveRDS(objetos_bloque1, ruta_objetos)

con_salida <- file(ruta_salida_maestra, open = "wt", encoding = "UTF-8")
sink(con_salida, split = TRUE)
old_width <- getOption("width")
options(width = 240)

tryCatch({
  imprimir_seccion("ICVLF v6.3 — BLOQUE 1 METODOLÓGICO")
  cat("Fecha/hora de ejecución:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Archivo base:", normalizePath(ruta, winslash = "/", mustWork = TRUE), "\n")
  cat("MD5:", unname(tools::md5sum(ruta)), "\n")
  cat("Interpretación: índice retrospectivo de vulnerabilidad histórica relativa.\n")
  cat("No es LCR/NSFR regulatorio, probabilidad de crisis, pronóstico ni estimación causal.\n")
  
  imprimir_seccion("A. METADATA, INTEGRIDAD TEMPORAL Y COBERTURA")
  imprimir_objeto(metadata_ejecucion)
  imprimir_objeto(auditoria_diccionario)
  imprimir_objeto(auditoria_no_finitos)
  cat("\nAuditoría de parseo numérico:\n")
  imprimir_objeto(auditoria_parseo)
  imprimir_objeto(cobertura_vars)
  
  imprimir_seccion("B. FUNDAMENTO FINANCIERO Y ORIENTACIÓN EX ANTE")
  imprimir_objeto(tabla_marco_dimensiones)
  cat("\nOrientación ex ante por indicador:\n")
  imprimir_objeto(tabla_orientacion_riesgo %>%
                    select(bloque, variable, indicador, direccion, fortaleza_signo,
                           sensibilidad_especifica, fundamento_financiero))
  
  imprimir_seccion("C. DESCRIPTIVOS Y DISTRIBUCIÓN")
  imprimir_objeto(estadisticas_ext %>%
                    select(bloque_id, variable, etiqueta, n, n_na, media, sd, cv_pct,
                           min, p5, p25, mediana, p75, p95, max, sesgo, curtosis, jb_pval, normalidad_jb))
  imprimir_objeto(test_adf)
  
  imprimir_seccion("D. CORRELACIONES EDA POR DIMENSIÓN — PEARSON, SPEARMAN Y N POR PAR")
  imprimir_objeto(tabla_cor_bloques)
  
  imprimir_seccion("E. NORMALIZACIÓN Y AUDITORÍA DE SIGNOS")
  imprimir_objeto(auditoria_subindices)
  cat("\nReponderación interna causada por faltantes (si existe):\n")
  imprimir_objeto(auditoria_reponderacion_interna)
  
  imprimir_seccion("F. PCA COMO DIAGNÓSTICO/SENSIBILIDAD")
  imprimir_objeto(tabla_metodos_subindices)
  cat("\nPesos PCA internos:\n")
  imprimir_objeto(tabla_pesos_pca_internos)
  cat("\nRedundancia |r|>=0.90:\n")
  imprimir_objeto(tabla_redundancia)
  
  imprimir_seccion("G. PONDERACIÓN JERÁRQUICA Y COEFICIENTES ANALÍTICOS")
  imprimir_objeto(tabla_pesos_jerarquicos)
  cat("\nIMPORTANTE: peso jerárquico nominal != coeficiente final por indicador debido a la reestandarización de cada dimensión.\n")
  
  imprimir_seccion("H. ICVLF PRINCIPAL, SUBÍNDICES Y SUBPERIODOS")
  imprimir_objeto(indices %>%
                    select(Fecha, all_of(bloques_core), ICVLF_equal, ICVLF_equal_100, nivel_vulnerabilidad))
  imprimir_objeto(tabla_subperiodos)
  imprimir_objeto(tabla_extremos_icvlf)
  cat("\nCorrelación entre subíndices:\n")
  print(round(cor_subindices, 4))
  
  imprimir_seccion("I. PROXIES Y CONSISTENCIA CONVERGENTE")
  imprimir_objeto(tabla_cobertura_proxies)
  imprimir_objeto(tabla_correlaciones %>%
                    filter(indice == "ICVLF_equal_100") %>%
                    select(proxy, n_obs, pearson, spearman, intensidad))
  cat("\nValidación sin solapamiento mecánico P1/P2:\n")
  imprimir_objeto(tabla_validacion_sin_solapamiento)
  cat("\nHeterogeneidad por subperiodo:\n")
  imprimir_objeto(tabla_correlaciones_subperiodo)
  cat("\nAsociaciones adelantadas descriptivas (NO predictivas):\n")
  imprimir_objeto(tabla_rezagos)
  
  imprimir_seccion("J. ESTACIONARIEDAD E INFERENCIA HAC")
  imprimir_objeto(tabla_estacionariedad_icvlf_proxies)
  imprimir_objeto(tabla_integracion_resumen)
  cat("\nHAC en niveles (complementario):\n")
  imprimir_objeto(tabla_hac_consistencia)
  cat("\nHAC en primeras diferencias del ICVLF z (principal inferencial de corto plazo):\n")
  imprimir_objeto(tabla_hac_diferencias)
  cat("\nSensibilidad a extremos 1%-99% + diagnóstico Cook:\n")
  imprimir_objeto(tabla_hac_diferencias_winsor)
  
  imprimir_seccion("K. ROBUSTEZ METODOLÓGICA Y BENCHMARK DE ALCANCE")
  cat("Matriz de sensibilidades metodológicas (sin benchmark estrecho):\n")
  print(round(tabla_robustez_metodologica, 4))
  cat("\nBenchmark de alcance Liquidez+Fondeo:\n")
  imprimir_objeto(tabla_benchmark_alcance)
  cat("\nLeave-one-indicator-out:\n")
  imprimir_objeto(tabla_robustez_leave1_indicador)
  cat("\nLeave-one-dimension-out:\n")
  imprimir_objeto(tabla_robustez_leave1_dimension)
  imprimir_objeto(tabla_sensibilidad_pesos_internos)
  imprimir_objeto(tabla_sensibilidad_normalizacion)
  imprimir_objeto(tabla_sensibilidad_signos_financieros)
  
  imprimir_seccion("L. ROLLING — COBERTURA, 60M PRINCIPAL Y 24M SENSIBILIDAD")
  cat("Proxies 60m admitidos:", paste(proxies_rolling_q, collapse = ", "), "\n")
  cat("Proxies 24m admitidos:", paste(proxies_rolling_s, collapse = ", "), "\n")
  imprimir_objeto(tabla_resumen_rolling_cor_60m)
  imprimir_objeto(tabla_resumen_rolling_cor_24m)
  imprimir_objeto(tabla_resumen_autocor_60m)
  
  imprimir_seccion("M. RUPTURAS ESTRUCTURALES BAI-PERRON")
  imprimir_objeto(tabla_bic_rupturas)
  imprimir_objeto(rupturas_df)
  cat("\nEstructura cruda de confint() para auditoría:\n")
  print(auditoria_confint_bai_perron)
  
  imprimir_seccion("N. ESTRÉS CONTRAFACTUAL")
  cat("Calibración dimensional actual -> Q95:\n")
  imprimir_objeto(tabla_calibracion_estres_dim)
  cat("\nEscenarios Q95 + peor configuración histórica:\n")
  imprimir_objeto(tabla_estres)
  cat("\nSensibilidad +1.5 d.e.:\n")
  imprimir_objeto(tabla_estres_sensibilidad_de)
  
  imprimir_seccion("O. CONTRIBUCIONES E IDENTIDAD CONTABLE DEL ÍNDICE")
  imprimir_objeto(contrib_subperiodos)
  imprimir_objeto(tabla_contribuciones_control)
  
  imprimir_seccion("P. SESSION INFO")
  print(sessionInfo())
  
  imprimir_seccion("FIN BLOQUE 1")
  cat("TXT maestro:", ruta_salida_maestra, "\n")
  cat("RDS de objetos para Bloque 2:", ruta_objetos, "\n")
}, finally = {
  options(width = old_width)
  sink()
  close(con_salida)
})

cat("\n✓ BLOQUE 1 COMPLETADO.\n")
cat("  Salida maestra:", ruta_salida_maestra, "\n")
cat("  Objetos para Bloque 2:", ruta_objetos, "\n")
cat("  Este script NO genera gráficos ni tablas publicables.\n")










# =============================================================================
# ICVLF v6.3 — BLOQUE 2 FINAL
# SALIDAS PUBLICABLES PARA TESIS / PAPER
# Sistema Bancario Boliviano · Enero 2010 – Diciembre 2025
# =============================================================================
#
# PROPÓSITO
#   - NO recalcula el ICVLF.
#   - NO vuelve a leer la base Excel.
#   - Consume exclusivamente OBJETOS_ICVLF_v6_3_BLOQUE1.rds.
#   - Genera tablas, figuras, anexos, CSV, Excel y salida maestra TXT.
#
# ARQUITECTURA EDITORIAL
#   CUERPO:
#     5 tablas + 6 figuras.
#   ANEXOS:
#     evidencia descriptiva, diagnóstica y de robustez completa.
#
# REGLAS:
#   1) Nombres internos ASCII para evitar errores por tildes.
#   2) Funciones de dplyr/tidyr calificadas explícitamente para evitar masking.
#   3) El HTML se genera con R base: no depende de knitr/kableExtra.
#   4) Coeficientes/correlaciones: 4 decimales.
#   5) p-values: 4 decimales; <0.0001 se muestra como "<0.0001".
#   6) Escala 0–100: 2 decimales.
#   7) La escala 0–100 es histórica/retrospectiva, no probabilidad ni umbral regulatorio.
#   8) El agregado analítico es un promedio de cinco subíndices estandarizados;
#      no se denomina "z-score final".
# =============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

# -----------------------------------------------------------------------------
# 0. PAQUETES
# -----------------------------------------------------------------------------

paquetes <- c(
  "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "ggplot2", "openxlsx"
)

instalar_faltantes <- FALSE

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  if (isTRUE(instalar_faltantes)) {
    install.packages(faltantes, repos = "https://cloud.r-project.org")
  } else {
    stop(
      "Faltan paquetes requeridos para Bloque 2: ",
      paste(faltantes, collapse = ", "),
      ". Instálelos o cambie instalar_faltantes <- TRUE."
    )
  }
}

# No se usa library() para minimizar conflictos de namespace.

# -----------------------------------------------------------------------------
# 1. LOCALIZAR Y CARGAR EL RDS DEL BLOQUE 1
# -----------------------------------------------------------------------------

nombre_rds <- "OBJETOS_ICVLF_v6_3_BLOQUE1.rds"

ruta_rds_candidatas <- unique(c(
  Sys.getenv("ICVLF_B1_RDS", unset = ""),
  file.path(
    getwd(),
    "ICVLF_v6_3_Bloque1_Metodologico",
    "diagnosticos",
    nombre_rds
  ),
  file.path(getwd(), nombre_rds),
  file.path(
    path.expand("~"),
    "OneDrive", "Documents",
    "ICVLF_v6_3_Bloque1_Metodologico",
    "diagnosticos",
    nombre_rds
  ),
  file.path(
    path.expand("~"),
    "OneDrive", "Desktop", "Tesis",
    "ICVLF_v6_3_Bloque1_Metodologico",
    "diagnosticos",
    nombre_rds
  )
))

ruta_rds_candidatas <- ruta_rds_candidatas[
  nzchar(ruta_rds_candidatas)
]

ruta_rds_existente <- ruta_rds_candidatas[
  file.exists(ruta_rds_candidatas)
]

if (length(ruta_rds_existente) == 0) {
  encontrados <- list.files(
    path = getwd(),
    pattern = paste0("^", nombre_rds, "$"),
    recursive = TRUE,
    full.names = TRUE
  )
  ruta_rds_existente <- encontrados[file.exists(encontrados)]
}

if (length(ruta_rds_existente) == 0) {
  stop(
    "No se encontró ", nombre_rds, ".\n",
    "Opciones:\n",
    "1) coloque el RDS dentro del proyecto;\n",
    "2) defina Sys.setenv(ICVLF_B1_RDS='ruta/completa/al/RDS')."
  )
}

ruta_rds <- normalizePath(
  ruta_rds_existente[1],
  winslash = "/",
  mustWork = TRUE
)

B1 <- readRDS(ruta_rds)

if (!is.list(B1)) {
  stop("El archivo RDS no contiene una lista válida del Bloque 1.")
}

# -----------------------------------------------------------------------------
# 2. PRE-FLIGHT: VALIDACIÓN DE OBJETOS Y ESQUEMAS
# -----------------------------------------------------------------------------

requerir_objeto <- function(nombre, columnas = NULL) {
  
  if (!nombre %in% names(B1)) {
    stop(
      "RDS incompatible: falta el objeto '", nombre, "'."
    )
  }
  
  x <- B1[[nombre]]
  
  if (!is.null(columnas)) {
    if (!is.data.frame(x)) {
      stop(
        "El objeto '", nombre,
        "' debía ser data.frame/tibble para validar columnas."
      )
    }
    
    faltan <- setdiff(columnas, names(x))
    
    if (length(faltan) > 0) {
      stop(
        "RDS incompatible: en '", nombre,
        "' faltan columnas: ",
        paste(faltan, collapse = ", ")
      )
    }
  }
  
  invisible(TRUE)
}

# Objetos principales.
requerir_objeto("metadata_ejecucion", c("item", "valor"))
requerir_objeto("tabla_marco_dimensiones",
                c("bloque", "naturaleza", "mecanismo_financiero", "limite_interpretativo"))
requerir_objeto("cobertura_vars",
                c("variable", "bloque_id", "cobertura_pct", "incluir"))
requerir_objeto("tabla_orientacion_riesgo",
                c("variable", "indicador", "bloque", "direccion",
                  "fortaleza_signo", "sensibilidad_especifica",
                  "fundamento_financiero"))
requerir_objeto("estadisticas_ext",
                c("variable", "bloque_id", "etiqueta", "n", "n_na",
                  "media", "sd", "min", "p5", "p25", "mediana",
                  "p75", "p95", "max", "sesgo", "curtosis", "jb_pval"))
requerir_objeto("test_adf_indicadores",
                c("variable", "bloque_id", "etiqueta",
                  "adf_stat", "adf_pval", "estacionaria"))
requerir_objeto("tabla_cor_bloques",
                c("bloque", "var1", "var2", "r_pearson", "r_spearman",
                  "n_pares", "etiq_var1", "etiq_var2"))
requerir_objeto("tabla_metodos_subindices",
                c("bloque", "n_indicadores", "n_casos_completos",
                  "cor_media_abs", "KMO", "Bartlett_pvalue",
                  "var_exp_pc1", "coherencia_cargas_pc1",
                  "pca_interpretable"))
requerir_objeto("tabla_pesos_jerarquicos",
                c("bloque", "indicador", "etiqueta", "peso_interno",
                  "peso_dimension", "peso_jerarquico_nominal",
                  "coeficiente_analitico_z"))
requerir_objeto("tabla_pesos_pca_internos",
                c("bloque", "indicador", "etiqueta",
                  "carga_pc1", "peso_pca", "pca_interpretable"))
requerir_objeto("subindices",
                c("Fecha", "liquidez", "fondeo", "activos",
                  "cartera", "solvencia", "ICVLF_equal"))
requerir_objeto("indices",
                c("Fecha", "liquidez", "fondeo", "activos", "cartera",
                  "solvencia", "ICVLF_equal", "ICVLF_equal_100",
                  "ICVLF_alt_pesos_100", "ICVLF_pca_global_100",
                  "ICVLF_nucleo_100", "ICVLF_pca_interno_100",
                  "ICVLF_robust_z_100", "nivel_vulnerabilidad"))
requerir_objeto("tabla_subperiodos",
                c("subperiodo", "n", "icvlf_media", "icvlf_sd",
                  "icvlf_min", "icvlf_max", "regimen_modal",
                  "pct_alta_muy_alta"))
requerir_objeto("tabla_cobertura_proxies",
                c("proxy", "descripcion", "n", "cobertura_pct",
                  "fecha_inicio", "fecha_fin"))
requerir_objeto("tabla_correlaciones",
                c("indice", "proxy", "n_obs", "pearson", "spearman"))
requerir_objeto("tabla_validacion_sin_solapamiento",
                c("proxy", "n_obs", "pearson", "spearman"))
requerir_objeto("tabla_correlaciones_subperiodo",
                c("proxy", "subperiodo", "n_obs",
                  "pearson", "spearman", "intensidad"))
requerir_objeto("tabla_rezagos",
                c("proxy", "h", "n_obs", "pearson_h", "spearman_h"))
requerir_objeto("tabla_estacionariedad_icvlf_proxies",
                c("serie", "transformacion", "n",
                  "adf_stat", "adf_p", "pp_stat", "pp_p",
                  "kpss_level_stat", "kpss_level_p",
                  "kpss_trend_stat", "kpss_trend_p",
                  "evidencia_estacionaria"))
requerir_objeto("tabla_integracion_resumen",
                c("serie", "Nivel", "Primera diferencia", "lectura"))
requerir_objeto("tabla_hac_consistencia",
                c("proxy", "n_obs", "beta_icvlf", "se_hac",
                  "t_hac", "p_hac", "r2"))
requerir_objeto("tabla_hac_diferencias",
                c("proxy", "n_obs", "beta_delta", "se_hac",
                  "t_hac", "p_hac", "r2"))
requerir_objeto("tabla_hac_diferencias_winsor",
                c("proxy", "n_obs", "beta_delta_w", "se_hac_w",
                  "t_hac_w", "p_hac_w", "r2_w",
                  "n_cook_gt_4n", "max_cook"))
requerir_objeto("tabla_benchmark_alcance",
                c("benchmark", "pearson_vs_icvlf_integral",
                  "spearman_vs_icvlf_integral", "lectura"))
requerir_objeto("tabla_robustez_leave1_indicador",
                c("variable_omitida", "indicador_omitido", "bloque",
                  "n_comparables", "pearson_vs_base",
                  "spearman_vs_base", "dam_z"))
requerir_objeto("tabla_robustez_leave1_dimension",
                c("dimension_omitida", "dimension", "n_comparables",
                  "pearson_vs_base", "spearman_vs_base", "dam_z"))
requerir_objeto("tabla_sensibilidad_signos_financieros",
                c("n_variables_excluidas", "pearson", "spearman", "dam_z"))
requerir_objeto("tabla_bic_rupturas",
                c("n_rupturas", "BIC"))
requerir_objeto("rupturas_df",
                c("numero", "fecha", "ic95_inf",
                  "ic95_sup", "ancho_ic_meses_aprox"))
requerir_objeto("tabla_calibracion_estres_dim",
                c("dimension", "dimension_label",
                  "estado_actual_z", "q95_historico_z",
                  "brecha_hasta_q95", "aporte_delta_icvlf_z"))
requerir_objeto("tabla_estres",
                c("codigo", "escenario", "dimensiones_afectadas",
                  "calibracion", "ICVLF_simulado_z",
                  "delta_z", "ICVLF_simulado_100_ref", "tipo"))
requerir_objeto("tabla_estres_sensibilidad_de",
                c("codigo", "escenario", "dimensiones_afectadas",
                  "calibracion", "ICVLF_simulado_z",
                  "delta_z", "ICVLF_simulado_100_ref"))
requerir_objeto("contrib_subperiodos",
                c("subperiodo", "bloque",
                  "contribucion_media", "contribucion_max"))
requerir_objeto("tabla_resumen_rolling_cor_60m",
                c("serie", "n", "media", "minimo", "fecha_min",
                  "maximo", "fecha_max", "ultimo", "fecha_ultimo"))
requerir_objeto("tabla_resumen_rolling_cor_24m",
                c("serie", "n", "media", "minimo", "fecha_min",
                  "maximo", "fecha_max", "ultimo", "fecha_ultimo"))
requerir_objeto("tabla_resumen_autocor_60m",
                c("serie", "n", "media", "minimo", "fecha_min",
                  "maximo", "fecha_max", "ultimo", "fecha_ultimo"))
requerir_objeto("tabla_extremos_icvlf",
                c("tipo", "Fecha", "ICVLF_equal_z",
                  "ICVLF_equal_100", "nivel_vulnerabilidad"))

if (!"tabla_robustez_metodologica" %in% names(B1)) {
  stop("RDS incompatible: falta 'tabla_robustez_metodologica'.")
}

if (!is.matrix(B1$tabla_robustez_metodologica)) {
  stop("'tabla_robustez_metodologica' debe ser una matriz de correlaciones.")
}

cat("✓ Pre-flight RDS superado: objetos y columnas principales verificados.\n")
cat("✓ RDS:", ruta_rds, "\n")

# -----------------------------------------------------------------------------
# 3. OBJETOS LOCALES — SIN list2env()
# -----------------------------------------------------------------------------

metadata                    <- B1$metadata_ejecucion
marco_dimensiones           <- B1$tabla_marco_dimensiones
cobertura                   <- B1$cobertura_vars
orientacion                 <- B1$tabla_orientacion_riesgo
descriptivos                <- B1$estadisticas_ext
adf_indicadores             <- B1$test_adf_indicadores
cor_bloques                 <- B1$tabla_cor_bloques
metodos_subindices          <- B1$tabla_metodos_subindices
pesos_jerarquicos           <- B1$tabla_pesos_jerarquicos
pesos_pca_internos          <- B1$tabla_pesos_pca_internos
subindices                  <- B1$subindices
indices                     <- B1$indices
subperiodos                 <- B1$tabla_subperiodos
cobertura_proxies           <- B1$tabla_cobertura_proxies
correlaciones               <- B1$tabla_correlaciones
validacion_sin_solap        <- B1$tabla_validacion_sin_solapamiento
correlaciones_subperiodo    <- B1$tabla_correlaciones_subperiodo
rezagos                     <- B1$tabla_rezagos
estacionariedad             <- B1$tabla_estacionariedad_icvlf_proxies
integracion_resumen         <- B1$tabla_integracion_resumen
hac_niveles                 <- B1$tabla_hac_consistencia
hac_diferencias             <- B1$tabla_hac_diferencias
hac_winsor                  <- B1$tabla_hac_diferencias_winsor
rob_matrix                  <- B1$tabla_robustez_metodologica
benchmark_alcance           <- B1$tabla_benchmark_alcance
loo_indicador               <- B1$tabla_robustez_leave1_indicador
loo_dimension               <- B1$tabla_robustez_leave1_dimension
sens_signos                 <- B1$tabla_sensibilidad_signos_financieros
bic_rupturas                <- B1$tabla_bic_rupturas
rupturas                    <- B1$rupturas_df
calibracion_estres          <- B1$tabla_calibracion_estres_dim
estres_q95                  <- B1$tabla_estres
estres_de                   <- B1$tabla_estres_sensibilidad_de
contrib_subperiodos         <- B1$contrib_subperiodos
rolling_q                   <- B1$rolling_q
rolling_s                   <- B1$rolling_s
rolling_60_resumen          <- B1$tabla_resumen_rolling_cor_60m
rolling_24_resumen          <- B1$tabla_resumen_rolling_cor_24m
rolling_ar1_resumen         <- B1$tabla_resumen_autocor_60m
extremos_icvlf              <- B1$tabla_extremos_icvlf

# -----------------------------------------------------------------------------
# 4. PARÁMETROS EDITORIALES
# -----------------------------------------------------------------------------

capitulo <- 4L
muestra_str <- "Enero 2010 – Diciembre 2025"

fuente_general <- paste0(
  "Fuente: Elaboración propia con base en información publicada por ",
  "la Autoridad de Supervisión del Sistema Financiero (ASFI)."
)

nota_escala_100 <- paste0(
  "La escala 0–100 es una normalización histórica retrospectiva respecto ",
  "de 2010–2025; no representa probabilidad de crisis ni umbral regulatorio."
)

nota_analitica <- paste0(
  "La escala analítica corresponde al promedio de cinco subíndices ",
  "estandarizados y orientados a vulnerabilidad."
)

bloques <- c("liquidez", "fondeo", "activos", "cartera", "solvencia")

etiquetas_bloque <- c(
  liquidez  = "Liquidez",
  fondeo    = "Fondeo",
  activos   = "Activos",
  cartera   = "Cartera",
  solvencia = "Solvencia"
)

proxy_label <- c(
  P1       = "P1: Disp. / Oblig. CP",
  P2       = "P2: (Disp. + Inv. Temp.) / Oblig. CP",
  CLA      = "CLA",
  IPFE_100 = "IPFE"
)

col_principal <- "#1F4E79"
col_acento    <- "#A23E48"
col_neutro    <- "#4B5563"
col_grid      <- "#D9DEE5"

col_dim <- c(
  Liquidez  = "#0072B2",
  Fondeo    = "#E69F00",
  Activos   = "#009E73",
  Cartera   = "#D55E00",
  Solvencia = "#CC79A7"
)

col_proxy <- c(
  P1 = "#0072B2",
  P2 = "#D55E00"
)

# -----------------------------------------------------------------------------
# 5. FUNCIONES DE FORMATO
# -----------------------------------------------------------------------------

fmt4 <- function(x) {
  ifelse(is.na(x), "S/D", sprintf("%.4f", x))
}

fmt2 <- function(x) {
  ifelse(is.na(x), "S/D", sprintf("%.2f", x))
}

fmt1 <- function(x) {
  ifelse(is.na(x), "S/D", sprintf("%.1f", x))
}

fmt_p <- function(x) {
  ifelse(
    is.na(x),
    "S/D",
    ifelse(x < 0.0001, "<0.0001", sprintf("%.4f", x))
  )
}

fmt_p_tseries <- function(x) {
  ifelse(
    is.na(x), "S/D",
    ifelse(
      x <= 0.01, "<0.01",
      ifelse(x >= 0.10, ">0.10", sprintf("%.4f", x))
    )
  )
}

safe_label_proxy <- function(x) {
  out <- unname(proxy_label[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

# -----------------------------------------------------------------------------
# 6. CARPETAS DE SALIDA
# -----------------------------------------------------------------------------

carpeta_b2 <- file.path(
  getwd(),
  "ICVLF_v6_3_Bloque2_Tesis_FINAL"
)

dirs <- c(
  file.path(carpeta_b2, "01_CUERPO", "TABLAS_HTML"),
  file.path(carpeta_b2, "01_CUERPO", "FIGURAS"),
  file.path(carpeta_b2, "02_ANEXOS", "TABLAS_HTML"),
  file.path(carpeta_b2, "02_ANEXOS", "FIGURAS"),
  file.path(carpeta_b2, "03_CSV"),
  file.path(carpeta_b2, "04_EXCEL"),
  file.path(carpeta_b2, "05_LOGS")
)

invisible(lapply(
  dirs,
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

dir_tab_cuerpo <- file.path(carpeta_b2, "01_CUERPO", "TABLAS_HTML")
dir_fig_cuerpo <- file.path(carpeta_b2, "01_CUERPO", "FIGURAS")
dir_tab_anexo  <- file.path(carpeta_b2, "02_ANEXOS", "TABLAS_HTML")
dir_fig_anexo  <- file.path(carpeta_b2, "02_ANEXOS", "FIGURAS")
dir_csv        <- file.path(carpeta_b2, "03_CSV")
dir_excel      <- file.path(carpeta_b2, "04_EXCEL")
dir_logs       <- file.path(carpeta_b2, "05_LOGS")

ruta_log <- file.path(
  dir_logs,
  "LOG_EJECUCION_BLOQUE2.txt"
)

if (file.exists(ruta_log)) {
  file.remove(ruta_log)
}

log_line <- function(...) {
  txt <- paste0(..., collapse = "")
  cat(txt, "\n")
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ", txt, "\n",
    file = ruta_log,
    append = TRUE,
    sep = ""
  )
}

# -----------------------------------------------------------------------------
# 7. EXPORTACIÓN ROBUSTA DE TABLAS: HTML + CSV
# -----------------------------------------------------------------------------

html_escape <- function(x) {
  
  x <- as.character(x)
  x[is.na(x)] <- "S/D"
  
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  
  x
}

tabla_a_html <- function(df, titulo, nota) {
  
  df_html <- as.data.frame(
    lapply(df, html_escape),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  th <- paste0(
    "<th>",
    html_escape(names(df_html)),
    "</th>",
    collapse = ""
  )
  
  filas <- character(nrow(df_html))
  
  if (nrow(df_html) > 0) {
    for (i in seq_len(nrow(df_html))) {
      td <- paste0(
        "<td>",
        unlist(df_html[i, , drop = TRUE], use.names = FALSE),
        "</td>",
        collapse = ""
      )
      filas[i] <- paste0("<tr>", td, "</tr>")
    }
  }
  
  paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
    "<style>",
    "body{font-family:Arial,sans-serif;margin:24px;color:#111827;}",
    ".wrap{overflow-x:auto;}",
    "table{border-collapse:collapse;width:auto;max-width:100%;font-size:10pt;}",
    "caption{caption-side:top;text-align:left;font-weight:bold;",
    "font-size:11pt;margin-bottom:8px;}",
    "th{background:#E9EEF4;border-bottom:1.5px solid #64748B;",
    "padding:6px 8px;text-align:center;vertical-align:bottom;}",
    "td{border-bottom:1px solid #E5E7EB;padding:5px 8px;",
    "vertical-align:top;}",
    "tr:last-child td{border-bottom:1.5px solid #64748B;}",
    ".note{font-size:8.5pt;color:#4B5563;margin-top:8px;",
    "max-width:1200px;line-height:1.35;}",
    "</style></head><body><div class='wrap'><table>",
    "<caption>", html_escape(titulo), "</caption>",
    "<thead><tr>", th, "</tr></thead>",
    "<tbody>", paste(filas, collapse = ""), "</tbody>",
    "</table></div>",
    "<div class='note'><b>Nota:</b> ", html_escape(nota), "</div>",
    "</body></html>"
  )
}

guardar_tabla <- function(
    df,
    codigo,
    titulo,
    nota,
    destino = c("cuerpo", "anexo")) {
  
  destino <- match.arg(destino)
  
  carpeta_html <- if (destino == "cuerpo") {
    dir_tab_cuerpo
  } else {
    dir_tab_anexo
  }
  
  ruta_html <- file.path(
    carpeta_html,
    paste0(codigo, ".html")
  )
  
  ruta_csv <- file.path(
    dir_csv,
    paste0(codigo, ".csv")
  )
  
  writeLines(
    tabla_a_html(df, titulo, nota),
    con = ruta_html,
    useBytes = TRUE
  )
  
  utils::write.csv(
    df,
    file = ruta_csv,
    row.names = FALSE,
    na = "S/D",
    fileEncoding = "UTF-8"
  )
  
  log_line("✓ Tabla ", codigo, " exportada.")
}

# -----------------------------------------------------------------------------
# 8. ESTILO Y EXPORTACIÓN DE FIGURAS
# -----------------------------------------------------------------------------

theme_paper <- function(base_size = 10.5) {
  
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = "sans"
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 1.5,
        hjust = 0,
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size - 0.3,
        color = "#4B5563",
        hjust = 0,
        margin = ggplot2::margin(b = 8)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size - 2.0,
        color = "#4B5563",
        hjust = 0,
        lineheight = 1.15,
        margin = ggplot2::margin(t = 8)
      ),
      axis.title = ggplot2::element_text(
        size = base_size
      ),
      axis.text = ggplot2::element_text(
        size = base_size - 1,
        color = "#1F2937"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        color = col_grid,
        linewidth = 0.30
      ),
      strip.background = ggplot2::element_rect(
        fill = "#F3F4F6",
        color = NA
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = base_size - 0.2
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 10, 8, 8)
    )
}

guardar_figura <- function(
    grafico,
    codigo,
    ancho = 7.2,
    alto = 4.5,
    destino = c("cuerpo", "anexo")) {
  
  destino <- match.arg(destino)
  
  carpeta <- if (destino == "cuerpo") {
    dir_fig_cuerpo
  } else {
    dir_fig_anexo
  }
  
  ruta_png <- file.path(carpeta, paste0(codigo, ".png"))
  ruta_pdf <- file.path(carpeta, paste0(codigo, ".pdf"))
  
  ggplot2::ggsave(
    filename = ruta_png,
    plot = grafico,
    width = ancho,
    height = alto,
    units = "in",
    dpi = 400,
    bg = "white"
  )
  
  if (isTRUE(capabilities("cairo"))) {
    ggplot2::ggsave(
      filename = ruta_pdf,
      plot = grafico,
      width = ancho,
      height = alto,
      units = "in",
      device = grDevices::cairo_pdf,
      bg = "white"
    )
  } else {
    grDevices::pdf(
      ruta_pdf,
      width = ancho,
      height = alto,
      family = "sans",
      useDingbats = FALSE
    )
    print(grafico)
    grDevices::dev.off()
  }
  
  log_line("✓ Figura ", codigo, " exportada en PNG 400 dpi + PDF.")
}

# -----------------------------------------------------------------------------
# 9. HECHOS ESTILIZADOS — ANTES DEL CORE DEL ICVLF
# -----------------------------------------------------------------------------

log_line("── 9. Hechos estilizados ──")

hechos_cob <- cobertura |>
  dplyr::group_by(bloque_id) |>
  dplyr::summarise(
    n_indicadores = dplyr::n(),
    cobertura_min = min(cobertura_pct, na.rm = TRUE),
    cobertura_media = mean(cobertura_pct, na.rm = TRUE),
    .groups = "drop"
  )

hechos_dist <- descriptivos |>
  dplyr::group_by(bloque_id) |>
  dplyr::summarise(
    n_no_normal = sum(jb_pval < 0.05, na.rm = TRUE),
    pct_no_normal = 100 * mean(jb_pval < 0.05, na.rm = TRUE),
    mediana_abs_sesgo = median(abs(sesgo), na.rm = TRUE),
    mediana_abs_curtosis = median(abs(curtosis), na.rm = TRUE),
    .groups = "drop"
  )

hechos_adf <- adf_indicadores |>
  dplyr::group_by(bloque_id) |>
  dplyr::summarise(
    n_adf_est = sum(estacionaria %in% TRUE, na.rm = TRUE),
    n_adf_validas = sum(!is.na(estacionaria)),
    .groups = "drop"
  )

hechos_cor <- cor_bloques |>
  dplyr::group_by(bloque) |>
  dplyr::summarise(
    cor_media_abs = mean(abs(r_pearson), na.rm = TRUE),
    n_pares_090 = sum(abs(r_pearson) >= 0.90, na.rm = TRUE),
    n_pares = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::rename(bloque_id = bloque)

hechos_pca <- metodos_subindices |>
  dplyr::transmute(
    bloque_id = bloque,
    KMO = KMO,
    Bartlett_pvalue = Bartlett_pvalue,
    var_exp_pc1 = var_exp_pc1,
    coherencia_pc1 = coherencia_cargas_pc1,
    pca_interpretable = pca_interpretable
  )

hechos <- hechos_cob |>
  dplyr::left_join(hechos_dist, by = "bloque_id") |>
  dplyr::left_join(hechos_adf, by = "bloque_id") |>
  dplyr::left_join(hechos_cor, by = "bloque_id") |>
  dplyr::left_join(hechos_pca, by = "bloque_id") |>
  dplyr::mutate(
    dimension_label = unname(etiquetas_bloque[bloque_id])
  )

T41 <- hechos |>
  dplyr::transmute(
    `Dimensión` = dimension_label,
    `N indicadores` = n_indicadores,
    `Cobertura mínima` = paste0(fmt1(cobertura_min), "%"),
    `No normal JB` = paste0(
      n_no_normal, "/", n_indicadores,
      " (", fmt1(pct_no_normal), "%)"
    ),
    `ADF estacionarias` = paste0(
      n_adf_est, "/", n_adf_validas
    ),
    `|r| media` = fmt4(cor_media_abs),
    `Pares |r| ≥ 0.90` = n_pares_090,
    `Varianza PC1` = paste0(fmt1(100 * var_exp_pc1), "%"),
    `PCA interpretable` = ifelse(
      pca_interpretable, "Sí", "No"
    )
  )

guardar_tabla(
  T41,
  "Tabla_4_1_Hechos_Estilizados",
  "Tabla 4.1. Hechos estilizados y diagnóstico estadístico de las series de entrada",
  paste0(
    "La tabla resume las 25 series núcleo antes de su agregación. ",
    "JB = Jarque–Bera; ADF = Dickey–Fuller aumentado. ",
    "La estacionariedad individual es un diagnóstico complementario y no un criterio ",
    "de inclusión/exclusión. El PCA se usa como diagnóstico/sensibilidad, no como ",
    "regla para definir el modelo principal. ", fuente_general
  ),
  "cuerpo"
)

# Figura 4.1: P5–P95 estandarizado por serie.
perfil_series <- descriptivos |>
  dplyr::filter(
    is.finite(sd),
    sd > 0
  ) |>
  dplyr::mutate(
    z_p5 = (p5 - media) / sd,
    z_med = (mediana - media) / sd,
    z_p95 = (p95 - media) / sd,
    dimension_label = unname(etiquetas_bloque[bloque_id]),
    indicador_label = stringr::str_wrap(etiqueta, width = 38)
  ) |>
  dplyr::arrange(
    factor(
      bloque_id,
      levels = bloques
    ),
    z_med
  ) |>
  dplyr::mutate(
    dimension_label = factor(
      dimension_label,
      levels = unname(etiquetas_bloque[bloques])
    ),
    indicador_label = factor(
      indicador_label,
      levels = rev(unique(indicador_label))
    )
  )

g41 <- ggplot2::ggplot(
  perfil_series,
  ggplot2::aes(y = indicador_label)
) +
  ggplot2::geom_vline(
    xintercept = 0,
    color = "#6B7280",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = z_p5,
      xend = z_p95,
      yend = indicador_label,
      color = dimension_label
    ),
    linewidth = 0.9,
    alpha = 0.85,
    show.legend = FALSE
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      x = z_med,
      color = dimension_label
    ),
    size = 2,
    show.legend = FALSE
  ) +
  ggplot2::facet_grid(
    dimension_label ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  ggplot2::scale_color_manual(values = col_dim) +
  ggplot2::labs(
    title = "Figura 4.1. Perfil distributivo estandarizado de los 25 indicadores",
    subtitle = "Percentiles 5–95 y mediana, expresados en desviaciones estándar respecto de la media de cada serie",
    x = "Desviaciones estándar respecto de la media",
    y = NULL,
    caption = paste0(
      fuente_general, "\n",
      "Nota: la línea de cada indicador une P5 y P95; el punto corresponde a la mediana. ",
      "Esta transformación es exclusivamente descriptiva y no sustituye la normalización ",
      "empleada en la construcción del ICVLF."
    )
  ) +
  theme_paper(9.2) +
  ggplot2::theme(
    strip.text.y = ggplot2::element_text(angle = 0),
    axis.text.y = ggplot2::element_text(size = 7.4),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(
      color = col_grid,
      linewidth = 0.3
    )
  )

guardar_figura(
  g41,
  "Figura_4_1_Perfil_Series_Entrada",
  ancho = 7.2,
  alto = 9.2,
  destino = "cuerpo"
)

# -----------------------------------------------------------------------------
# 10. RESULTADO CENTRAL — EVOLUCIÓN DEL ICVLF
# -----------------------------------------------------------------------------

log_line("── 10. Evolución del ICVLF ──")

T42 <- subperiodos |>
  dplyr::transmute(
    `Subperiodo` = subperiodo,
    `N` = n,
    `Media ICVLF (0–100)` = round(icvlf_media, 2),
    `D.E.` = round(icvlf_sd, 2),
    `Mínimo` = round(icvlf_min, 2),
    `Máximo` = round(icvlf_max, 2),
    `Cuartil modal` = regimen_modal,
    `% Alta + Muy alta` = round(pct_alta_muy_alta, 1)
  )

guardar_tabla(
  T42,
  "Tabla_4_2_ICVLF_Subperiodos",
  "Tabla 4.2. Evolución del ICVLF por subperiodo histórico",
  paste0(
    nota_escala_100,
    " Los niveles Baja, Moderada, Alta y Muy alta son cuartiles históricos relativos. ",
    fuente_general
  ),
  "cuerpo"
)

q_icvlf <- as.numeric(
  stats::quantile(
    indices$ICVLF_equal_100,
    probs = c(0.25, 0.50, 0.75),
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )
)

max_row <- indices |>
  dplyr::filter(is.finite(ICVLF_equal_100)) |>
  dplyr::slice_max(
    ICVLF_equal_100,
    n = 1,
    with_ties = FALSE
  )

last_row <- indices |>
  dplyr::filter(is.finite(ICVLF_equal_100)) |>
  dplyr::slice_tail(n = 1)

g42 <- ggplot2::ggplot(
  indices,
  ggplot2::aes(x = Fecha, y = ICVLF_equal_100)
) +
  ggplot2::geom_hline(
    yintercept = q_icvlf,
    color = "#B6BDC7",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(
    color = col_principal,
    linewidth = 0.85
  ) +
  ggplot2::geom_vline(
    data = rupturas,
    ggplot2::aes(xintercept = fecha),
    color = "#7C8491",
    linetype = "dotted",
    linewidth = 0.5
  ) +
  ggplot2::geom_point(
    data = max_row,
    color = col_acento,
    size = 2.4
  ) +
  ggplot2::geom_point(
    data = last_row,
    color = "#111827",
    size = 2.4
  ) +
  ggplot2::annotate(
    "text",
    x = max_row$Fecha[1],
    y = max_row$ICVLF_equal_100[1],
    label = paste0(
      "Máximo: ",
      format(max_row$Fecha[1], "%Y-%m"),
      "\n",
      fmt2(max_row$ICVLF_equal_100[1])
    ),
    hjust = 1.05,
    vjust = -0.55,
    size = 3,
    color = col_acento
  ) +
  ggplot2::annotate(
    "text",
    x = last_row$Fecha[1],
    y = last_row$ICVLF_equal_100[1],
    label = paste0(
      "Dic-2025\n",
      fmt2(last_row$ICVLF_equal_100[1])
    ),
    hjust = 1.08,
    vjust = 1.45,
    size = 3,
    color = "#111827"
  ) +
  ggplot2::scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = ggplot2::expansion(mult = c(0.01, 0.04))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 106),
    breaks = seq(0, 100, 20),
    expand = ggplot2::expansion(mult = c(0, 0.01))
  ) +
  ggplot2::labs(
    title = "Figura 4.2. Evolución histórica del ICVLF y rupturas estimadas",
    subtitle = "Sistema bancario boliviano · 2010–2025",
    x = NULL,
    y = "ICVLF (escala histórica 0–100)",
    caption = paste0(
      fuente_general, "\n",
      "Nota: las líneas horizontales corresponden a Q25, Q50 y Q75; ",
      "las líneas verticales a rupturas Bai–Perron seleccionadas por BIC. ",
      nota_escala_100
    )
  ) +
  theme_paper()

guardar_figura(
  g42,
  "Figura_4_2_Trayectoria_ICVLF_Rupturas",
  7.2, 4.6, "cuerpo"
)

# -----------------------------------------------------------------------------
# 11. ANATOMÍA DEL ÍNDICE — DIMENSIONES Y CONTRIBUCIONES
# -----------------------------------------------------------------------------

log_line("── 11. Dimensiones y contribuciones ──")

sub_long <- subindices |>
  dplyr::select(
    Fecha,
    dplyr::all_of(bloques)
  ) |>
  tidyr::pivot_longer(
    cols = -Fecha,
    names_to = "bloque_id",
    values_to = "valor"
  ) |>
  dplyr::mutate(
    dimension_label = unname(etiquetas_bloque[bloque_id]),
    dimension_label = factor(
      dimension_label,
      levels = unname(etiquetas_bloque[bloques])
    )
  )

g43 <- ggplot2::ggplot(
  sub_long,
  ggplot2::aes(
    x = Fecha,
    y = valor,
    color = dimension_label
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = "#9CA3AF",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(
    linewidth = 0.65,
    show.legend = FALSE
  ) +
  ggplot2::facet_wrap(
    ~ dimension_label,
    ncol = 2,
    scales = "fixed"
  ) +
  ggplot2::scale_color_manual(values = col_dim) +
  ggplot2::scale_x_date(
    date_breaks = "4 years",
    date_labels = "%Y"
  ) +
  ggplot2::labs(
    title = "Figura 4.3. Evolución de las cinco dimensiones del ICVLF",
    subtitle = "Subíndices estandarizados y orientados a mayor vulnerabilidad",
    x = NULL,
    y = "Subíndice estandarizado",
    caption = paste0(
      fuente_general, "\n",
      "Nota: valores positivos representan posiciones por encima de la referencia histórica ",
      "de vulnerabilidad de cada dimensión. Las cinco dimensiones reciben 20% en el índice principal."
    )
  ) +
  theme_paper()

guardar_figura(
  g43,
  "Figura_4_3_Subindices_Dimensiones",
  7.2, 6.3, "cuerpo"
)

contrib_plot <- contrib_subperiodos |>
  dplyr::mutate(
    dimension_label = unname(etiquetas_bloque[bloque]),
    dimension_label = factor(
      dimension_label,
      levels = unname(etiquetas_bloque[bloques])
    ),
    subperiodo = factor(
      subperiodo,
      levels = c(
        "2010–2014",
        "2015–2019",
        "2020–2021",
        "2022–2025"
      )
    )
  )

g44 <- ggplot2::ggplot(
  contrib_plot,
  ggplot2::aes(
    x = dimension_label,
    y = contribucion_media,
    fill = dimension_label
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = col_neutro,
    linewidth = 0.45
  ) +
  ggplot2::geom_col(
    width = 0.68,
    show.legend = FALSE
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf("%+.4f", contribucion_media),
      vjust = ifelse(contribucion_media >= 0, -0.35, 1.25)
    ),
    size = 2.8,
    color = "#111827"
  ) +
  ggplot2::facet_wrap(
    ~ subperiodo,
    ncol = 2
  ) +
  ggplot2::scale_fill_manual(values = col_dim) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.16, 0.18))
  ) +
  ggplot2::labs(
    title = "Figura 4.4. Contribución media de las dimensiones por subperiodo",
    subtitle = "Descomposición exacta del ICVLF en su escala analítica",
    x = NULL,
    y = "Contribución media al ICVLF",
    caption = paste0(
      fuente_general, "\n",
      "Nota: contribuciones positivas elevan el índice respecto de su referencia histórica; ",
      "contribuciones negativas lo atenúan. La suma reproduce exactamente el ICVLF medio del subperiodo."
    )
  ) +
  theme_paper() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 25,
      hjust = 1
    )
  )

guardar_figura(
  g44,
  "Figura_4_4_Contribuciones_Subperiodo",
  7.2, 6.1, "cuerpo"
)

# -----------------------------------------------------------------------------
# 12. VALIDACIÓN CONVERGENTE — P1/P2
# -----------------------------------------------------------------------------

log_line("── 12. Validación convergente ──")

val_global <- correlaciones |>
  dplyr::filter(
    indice == "ICVLF_equal_100",
    proxy %in% c("P1", "P2")
  ) |>
  dplyr::select(
    proxy,
    n_obs,
    pearson,
    spearman
  )

val_sin <- validacion_sin_solap |>
  dplyr::filter(
    proxy %in% c("P1", "P2")
  ) |>
  dplyr::select(
    proxy,
    pearson_sin = pearson,
    spearman_sin = spearman
  )

val_hac <- hac_diferencias |>
  dplyr::filter(
    proxy %in% c("P1", "P2")
  ) |>
  dplyr::select(
    proxy,
    beta_delta,
    se_hac,
    t_hac,
    p_hac,
    r2
  )

val_win <- hac_winsor |>
  dplyr::filter(
    proxy %in% c("P1", "P2")
  ) |>
  dplyr::select(
    proxy,
    beta_delta_w,
    p_hac_w,
    n_cook_gt_4n
  )

T43_raw <- val_global |>
  dplyr::left_join(val_sin, by = "proxy") |>
  dplyr::left_join(val_hac, by = "proxy") |>
  dplyr::left_join(val_win, by = "proxy")

T43 <- T43_raw |>
  dplyr::transmute(
    `Proxy` = safe_label_proxy(proxy),
    `N` = n_obs,
    `Pearson r` = fmt4(pearson),
    `Spearman ρ` = fmt4(spearman),
    `r sin solapamiento` = fmt4(pearson_sin),
    `β ΔICVLF` = fmt4(beta_delta),
    `p HAC` = fmt_p(p_hac),
    `β winsor 1–99%` = fmt4(beta_delta_w),
    `p winsor` = fmt_p(p_hac_w)
  )

guardar_tabla(
  T43,
  "Tabla_4_3_Validacion_P1_P2",
  "Tabla 4.3. Validación convergente y consistencia de corto plazo del ICVLF",
  paste0(
    "P1 y P2 son proxies internos de cobertura líquida y no corresponden al LCR regulatorio. ",
    "La validación sin solapamiento reconstruye el índice retirando el indicador coincidente. ",
    "Las regresiones HAC se estiman en primeras diferencias con Newey–West a 12 rezagos; ",
    "evalúan asociación, no causalidad. La winsorización 1–99% es sensibilidad. ",
    fuente_general
  ),
  "cuerpo"
)

rolling_cor_60 <- rolling_q$cor

cols_roll <- intersect(
  c("r_P1", "r_P2"),
  names(rolling_cor_60)
)

if (length(cols_roll) == 2) {
  
  roll_plot <- rolling_cor_60 |>
    dplyr::select(
      Fecha,
      dplyr::all_of(cols_roll)
    ) |>
    tidyr::pivot_longer(
      cols = -Fecha,
      names_to = "proxy_id",
      values_to = "correlacion"
    ) |>
    dplyr::mutate(
      proxy_id = stringr::str_remove(proxy_id, "^r_"),
      proxy_id = factor(
        proxy_id,
        levels = c("P1", "P2")
      )
    )
  
  g45 <- ggplot2::ggplot(
    roll_plot,
    ggplot2::aes(
      x = Fecha,
      y = correlacion,
      color = proxy_id
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = col_neutro,
      linewidth = 0.45
    ) +
    ggplot2::geom_line(
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::scale_color_manual(values = col_proxy) +
    ggplot2::scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-1, 1),
      breaks = seq(-1, 1, 0.25)
    ) +
    ggplot2::labs(
      title = "Figura 4.5. Estabilidad temporal de la validación convergente",
      subtitle = "Correlación móvil de 60 meses entre el ICVLF y P1/P2",
      x = NULL,
      y = "Correlación de Pearson",
      caption = paste0(
        fuente_general, "\n",
        "Nota: cada ventana requiere al menos 80% de pares completos. ",
        "La variación temporal refleja heterogeneidad de la asociación; ",
        "no constituye evidencia causal ni predictiva."
      )
    ) +
    theme_paper()
  
  guardar_figura(
    g45,
    "Figura_4_5_Rolling_P1_P2_60m",
    7.2, 4.5, "cuerpo"
  )
  
} else {
  log_line(
    "ADVERTENCIA: no se generó Figura 4.5 porque rolling_q$cor ",
    "no contiene simultáneamente r_P1 y r_P2."
  )
}

# -----------------------------------------------------------------------------
# 13. ROBUSTEZ EJECUTIVA
# -----------------------------------------------------------------------------

log_line("── 13. Robustez ──")

extraer_cor_rob <- function(serie) {
  
  if (
    "ICVLF_equal_100" %in% rownames(rob_matrix) &&
    serie %in% colnames(rob_matrix)
  ) {
    return(
      as.numeric(
        rob_matrix["ICVLF_equal_100", serie]
      )
    )
  }
  
  NA_real_
}

spearman_directo <- function(serie) {
  stats::cor(
    indices$ICVLF_equal_100,
    indices[[serie]],
    method = "spearman",
    use = "complete.obs"
  )
}

loo_i_min <- loo_indicador |>
  dplyr::filter(is.finite(pearson_vs_base)) |>
  dplyr::slice_min(
    pearson_vs_base,
    n = 1,
    with_ties = FALSE
  )

loo_d_min <- loo_dimension |>
  dplyr::filter(is.finite(pearson_vs_base)) |>
  dplyr::slice_min(
    pearson_vs_base,
    n = 1,
    with_ties = FALSE
  )

rob_resumen <- dplyr::bind_rows(
  tibble::tibble(
    prueba = c(
      "Pesos dimensionales alternativos",
      "PCA global de subíndices",
      "Pesos internos PCA",
      "Normalización robusta mediana/MAD"
    ),
    detalle = c(
      "35–25–15–15–10",
      "PC1 global como sensibilidad",
      "PCA dentro de cada dimensión",
      "Misma arquitectura; escala robusta"
    ),
    pearson = c(
      extraer_cor_rob("ICVLF_alt_pesos_100"),
      extraer_cor_rob("ICVLF_pca_global_100"),
      extraer_cor_rob("ICVLF_pca_interno_100"),
      extraer_cor_rob("ICVLF_robust_z_100")
    ),
    spearman = c(
      spearman_directo("ICVLF_alt_pesos_100"),
      spearman_directo("ICVLF_pca_global_100"),
      spearman_directo("ICVLF_pca_interno_100"),
      spearman_directo("ICVLF_robust_z_100")
    )
  ),
  tibble::tibble(
    prueba = "Excluir signos/contextos sensibles",
    detalle = paste0(
      sens_signos$n_variables_excluidas[1],
      " indicadores excluidos"
    ),
    pearson = sens_signos$pearson[1],
    spearman = sens_signos$spearman[1]
  ),
  tibble::tibble(
    prueba = "Peor leave-one-indicator-out",
    detalle = loo_i_min$indicador_omitido[1],
    pearson = loo_i_min$pearson_vs_base[1],
    spearman = loo_i_min$spearman_vs_base[1]
  ),
  tibble::tibble(
    prueba = "Peor leave-one-dimension-out",
    detalle = as.character(loo_d_min$dimension[1]),
    pearson = loo_d_min$pearson_vs_base[1],
    spearman = loo_d_min$spearman_vs_base[1]
  )
)

T44 <- rob_resumen |>
  dplyr::transmute(
    `Prueba` = prueba,
    `Detalle` = detalle,
    `Pearson vs. principal` = fmt4(pearson),
    `Spearman vs. principal` = fmt4(spearman)
  )

guardar_tabla(
  T44,
  "Tabla_4_4_Robustez_Ejecutiva",
  "Tabla 4.4. Síntesis de robustez del ICVLF principal",
  paste0(
    "Se resumen sensibilidades que preservan el constructo integral y las pruebas ",
    "leave-one-out. El benchmark Liquidez + Fondeo se reporta por separado en anexos ",
    "porque cambia el alcance conceptual del índice. ",
    fuente_general
  ),
  "cuerpo"
)

# -----------------------------------------------------------------------------
# 14. RUPTURAS BAI–PERRON
# -----------------------------------------------------------------------------

log_line("── 14. Rupturas estructurales ──")

bic_min <- bic_rupturas |>
  dplyr::filter(is.finite(BIC)) |>
  dplyr::slice_min(
    BIC,
    n = 1,
    with_ties = FALSE
  )

T45 <- rupturas |>
  dplyr::transmute(
    `Ruptura` = numero,
    `Fecha estimada` = format(fecha, "%Y-%m"),
    `IC 95% inferior` = ifelse(
      is.na(ic95_inf),
      "S/D",
      format(ic95_inf, "%Y-%m")
    ),
    `IC 95% superior` = ifelse(
      is.na(ic95_sup),
      "S/D",
      format(ic95_sup, "%Y-%m")
    ),
    `Ancho IC (meses aprox.)` = fmt2(ancho_ic_meses_aprox)
  )

guardar_tabla(
  T45,
  "Tabla_4_5_Rupturas_Bai_Perron",
  "Tabla 4.5. Rupturas estimadas en el nivel medio del ICVLF",
  paste0(
    "Bai–Perron sobre la escala analítica del ICVLF. ",
    if (nrow(bic_min) == 1) {
      paste0(
        "La partición BIC mínima contiene ",
        bic_min$n_rupturas[1],
        " rupturas (BIC = ",
        fmt4(bic_min$BIC[1]),
        "). "
      )
    } else {
      ""
    },
    "Los intervalos reflejan incertidumbre estadística en la fecha; ",
    "no identifican causalidad económica. ",
    fuente_general
  ),
  "cuerpo"
)

# -----------------------------------------------------------------------------
# 15. ESTRÉS CONTRAFACTUAL
# -----------------------------------------------------------------------------

log_line("── 15. Estrés contrafactual ──")

labels_estres <- c(
  E0 = "Base dic-2025",
  E1 = "Liquidez Q95",
  E2 = "Fondeo Q95",
  E3 = "Activos Q95",
  E4 = "Cartera Q95",
  E5 = "Solvencia Q95",
  E6 = "Conjunto Q95",
  EH = "Máximo histórico"
)

stress_plot <- estres_q95 |>
  dplyr::filter(
    codigo %in% names(labels_estres)
  ) |>
  dplyr::mutate(
    escenario_label = unname(labels_estres[codigo]),
    escenario_label = factor(
      escenario_label,
      levels = rev(unname(labels_estres))
    ),
    grupo = dplyr::case_when(
      codigo == "E0" ~ "Estado observado",
      codigo == "EH" ~ "Histórico observado",
      TRUE ~ "Contrafactual Q95"
    )
  )

col_stress <- c(
  "Estado observado" = "#4B5563",
  "Contrafactual Q95" = col_principal,
  "Histórico observado" = col_acento
)

g46 <- ggplot2::ggplot(
  stress_plot,
  ggplot2::aes(
    x = escenario_label,
    y = ICVLF_simulado_100_ref,
    fill = grupo
  )
) +
  ggplot2::geom_col(width = 0.68) +
  ggplot2::geom_hline(
    yintercept = 100,
    color = "#6B7280",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf("%.2f", ICVLF_simulado_100_ref)
    ),
    hjust = -0.12,
    size = 3
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_fill_manual(values = col_stress) +
  ggplot2::scale_y_continuous(
    limits = c(
      0,
      max(
        stress_plot$ICVLF_simulado_100_ref,
        na.rm = TRUE
      ) * 1.12
    ),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::labs(
    title = "Figura 4.6. Escenarios contrafactuales de estrés del ICVLF",
    subtitle = "Shock adverso hasta el percentil histórico 95 por dimensión",
    x = NULL,
    y = "ICVLF simulado sobre referencia histórica 0–100",
    fill = NULL,
    caption = paste0(
      fuente_general, "\n",
      "Nota: 100 corresponde al máximo histórico observado. Un contrafactual puede ",
      "superar 100 porque la escala se calibra sobre la muestra histórica y no se trunca. ",
      "Los escenarios no son probabilidades."
    )
  ) +
  theme_paper()

guardar_figura(
  g46,
  "Figura_4_6_Estres_Q95",
  7.2, 4.9, "cuerpo"
)

# =============================================================================
# 16. ANEXOS — TABLAS
# =============================================================================

log_line("── 16. Anexos tabulares ──")

# A1. Marco conceptual de las dimensiones.
A1 <- marco_dimensiones |>
  dplyr::transmute(
    `Dimensión` = unname(etiquetas_bloque[bloque]),
    `Naturaleza` = naturaleza,
    `Mecanismo financiero` = mecanismo_financiero,
    `Límite interpretativo` = limite_interpretativo
  )

guardar_tabla(
  A1,
  "Anexo_A1_Marco_Dimensiones",
  "Anexo A1. Marco financiero y alcance interpretativo de las cinco dimensiones",
  paste0(
    "Liquidez y fondeo son dimensiones directas/estructurales; activos y cartera actúan ",
    "como canales de transmisión y solvencia como canal de absorción/confianza. ",
    fuente_general
  ),
  "anexo"
)

# A2. Arquitectura completa de indicadores.
A2 <- orientacion |>
  dplyr::left_join(
    cobertura |>
      dplyr::select(
        variable,
        cobertura_pct
      ),
    by = "variable"
  ) |>
  dplyr::left_join(
    pesos_jerarquicos |>
      dplyr::select(
        variable = indicador,
        peso_interno,
        peso_dimension,
        peso_jerarquico_nominal,
        coeficiente_analitico_z
      ),
    by = "variable"
  ) |>
  dplyr::transmute(
    `Dimensión` = unname(etiquetas_bloque[bloque]),
    `Indicador` = indicador,
    `d_i` = direccion,
    `Cobertura (%)` = round(cobertura_pct, 1),
    `Peso interno` = round(peso_interno, 4),
    `Peso dimensión` = round(peso_dimension, 4),
    `Peso jerárquico nominal` = round(peso_jerarquico_nominal, 4),
    `Coef. analítico` = round(coeficiente_analitico_z, 4),
    `Fortaleza del signo` = fortaleza_signo,
    `Signo sensible` = ifelse(
      sensibilidad_especifica,
      "Sí",
      "No"
    ),
    `Fundamento financiero` = fundamento_financiero
  )

guardar_tabla(
  A2,
  "Anexo_A2_Arquitectura_25_Indicadores",
  "Anexo A2. Arquitectura, orientación y ponderación de los 25 indicadores",
  paste0(
    "d_i = +1 implica que un mayor valor original se orienta a mayor vulnerabilidad; ",
    "d_i = −1 implica inversión del signo. Los pesos principales son iguales dentro ",
    "de dimensión y 20% entre dimensiones. ",
    fuente_general
  ),
  "anexo"
)

# A3. Descriptivos completos.
A3 <- descriptivos |>
  dplyr::transmute(
    `Dimensión` = unname(etiquetas_bloque[bloque_id]),
    `Indicador` = etiqueta,
    `N` = as.integer(n),
    `Faltantes` = as.integer(n_na),
    `Media` = round(media, 4),
    `D.E.` = round(sd, 4),
    `P5` = round(p5, 4),
    `P25` = round(p25, 4),
    `Mediana` = round(mediana, 4),
    `P75` = round(p75, 4),
    `P95` = round(p95, 4),
    `Mínimo` = round(min, 4),
    `Máximo` = round(max, 4),
    `Sesgo` = round(sesgo, 4),
    `Curtosis exc.` = round(curtosis, 4),
    `p JB` = fmt_p(jb_pval)
  )

guardar_tabla(
  A3,
  "Anexo_A3_Descriptivos_25_Indicadores",
  "Anexo A3. Estadísticos descriptivos completos de los indicadores núcleo",
  paste0(
    "JB = Jarque–Bera. Se reportan cuatro decimales cuando son informativos. ",
    fuente_general
  ),
  "anexo"
)

# A4. ADF individual.
A4 <- adf_indicadores |>
  dplyr::transmute(
    `Dimensión` = unname(etiquetas_bloque[bloque_id]),
    `Indicador` = etiqueta,
    `ADF` = round(adf_stat, 4),
    `p ADF` = fmt_p_tseries(adf_pval),
    `Lectura` = ifelse(
      estacionaria %in% TRUE,
      "Estacionaria al 5%",
      ifelse(
        estacionaria %in% FALSE,
        "No estacionaria al 5%",
        "S/D"
      )
    )
  )

guardar_tabla(
  A4,
  "Anexo_A4_ADF_Indicadores",
  "Anexo A4. Diagnóstico ADF individual de las 25 series de entrada",
  paste0(
    "H0: raíz unitaria. Los límites <0.01 y >0.10 reflejan la forma en que ",
    "tseries reporta p-values fuera de su tabla. Este diagnóstico no determina ",
    "la inclusión en el índice. ",
    fuente_general
  ),
  "anexo"
)

# A5. Correlaciones intradimensionales completas.
A5 <- cor_bloques |>
  dplyr::transmute(
    `Dimensión` = unname(etiquetas_bloque[bloque]),
    `Indicador 1` = etiq_var1,
    `Indicador 2` = etiq_var2,
    `N pares` = n_pares,
    `Pearson` = round(r_pearson, 4),
    `Spearman` = round(r_spearman, 4),
    `Alta redundancia |r|≥0.90` = ifelse(
      abs(r_pearson) >= 0.90,
      "Sí",
      "No"
    )
  )

guardar_tabla(
  A5,
  "Anexo_A5_Correlaciones_Intradimensionales",
  "Anexo A5. Correlaciones intradimensionales y redundancia potencial",
  paste0(
    "Las correlaciones se calculan por pares completos. Una correlación elevada no ",
    "produce eliminación automática; la robustez se evalúa con leave-one-out. ",
    fuente_general
  ),
  "anexo"
)

# A6. PCA por dimensión.
A6a <- metodos_subindices |>
  dplyr::transmute(
    `Dimensión` = unname(etiquetas_bloque[bloque]),
    `N indicadores` = n_indicadores,
    `Casos completos` = n_casos_completos,
    `|r| media` = round(cor_media_abs, 4),
    `KMO` = round(KMO, 4),
    `p Bartlett` = fmt_p(Bartlett_pvalue),
    `Var. PC1 (%)` = round(100 * var_exp_pc1, 2),
    `Coherencia cargas (%)` = round(
      100 * coherencia_cargas_pc1,
      2
    ),
    `PCA interpretable` = ifelse(
      pca_interpretable,
      "Sí",
      "No"
    )
  )

guardar_tabla(
  A6a,
  "Anexo_A6a_Diagnostico_PCA",
  "Anexo A6a. Diagnóstico PCA por dimensión",
  paste0(
    "El PCA se conserva como sensibilidad. La especificación principal no deriva ",
    "sus signos ni ponderaciones del PCA. ",
    fuente_general
  ),
  "anexo"
)

A6b <- pesos_pca_internos |>
  dplyr::transmute(
    `Dimensión` = unname(etiquetas_bloque[bloque]),
    `Indicador` = etiqueta,
    `Carga PC1 orientada` = round(carga_pc1, 4),
    `Peso PCA` = round(peso_pca, 4),
    `PCA interpretable en dimensión` = ifelse(
      pca_interpretable,
      "Sí",
      "No"
    )
  )

guardar_tabla(
  A6b,
  "Anexo_A6b_Pesos_PCA",
  "Anexo A6b. Cargas y pesos PCA utilizados únicamente como sensibilidad",
  paste0(
    "Los pesos PCA se construyen con la magnitud de las cargas PC1 y no sustituyen ",
    "los pesos iguales del índice principal. ",
    fuente_general
  ),
  "anexo"
)

# A7. Estacionariedad ICVLF/proxies.
A7a <- estacionariedad |>
  dplyr::transmute(
    `Serie` = serie,
    `Transformación` = transformacion,
    `N` = n,
    `ADF` = round(adf_stat, 4),
    `p ADF` = fmt_p_tseries(adf_p),
    `PP` = round(pp_stat, 4),
    `p PP` = fmt_p_tseries(pp_p),
    `KPSS nivel` = round(kpss_level_stat, 4),
    `p KPSS nivel` = fmt_p_tseries(kpss_level_p),
    `KPSS tendencia` = round(kpss_trend_stat, 4),
    `p KPSS tendencia` = fmt_p_tseries(kpss_trend_p),
    `Evidencia` = evidencia_estacionaria
  )

guardar_tabla(
  A7a,
  "Anexo_A7a_Estacionariedad_ICVLF_Proxies",
  "Anexo A7a. Diagnóstico ADF, PP y KPSS del ICVLF y proxies",
  paste0(
    "ADF/PP contrastan raíz unitaria; KPSS contrasta estacionariedad. ",
    "Se interpretan conjuntamente y con cautela ante rupturas. ",
    fuente_general
  ),
  "anexo"
)

A7b <- integracion_resumen |>
  dplyr::transmute(
    `Serie` = serie,
    `Nivel` = Nivel,
    `Primera diferencia` = `Primera diferencia`,
    `Lectura` = lectura
  )

guardar_tabla(
  A7b,
  "Anexo_A7b_Resumen_Integracion",
  "Anexo A7b. Síntesis de integración del ICVLF y proxies",
  paste0(
    "La clasificación resume la evidencia conjunta de las pruebas de estacionariedad. ",
    fuente_general
  ),
  "anexo"
)

# A8. Validación completa y heterogeneidad.
A8a <- correlaciones |>
  dplyr::filter(
    indice == "ICVLF_equal_100"
  ) |>
  dplyr::transmute(
    `Proxy` = safe_label_proxy(proxy),
    `N` = n_obs,
    `Pearson` = round(pearson, 4),
    `Spearman` = round(spearman, 4),
    `Intensidad` = intensidad
  )

guardar_tabla(
  A8a,
  "Anexo_A8a_Validacion_Global_Todos_Proxies",
  "Anexo A8a. Validación global del ICVLF principal frente a todos los proxies disponibles",
  paste0(
    "La evidencia es convergente/interna y no constituye validación externa independiente. ",
    fuente_general
  ),
  "anexo"
)

A8b <- correlaciones_subperiodo |>
  dplyr::transmute(
    `Proxy` = safe_label_proxy(proxy),
    `Subperiodo` = subperiodo,
    `N` = n_obs,
    `Pearson` = round(pearson, 4),
    `Spearman` = round(spearman, 4),
    `Intensidad` = intensidad
  )

guardar_tabla(
  A8b,
  "Anexo_A8b_Validacion_Subperiodos",
  "Anexo A8b. Heterogeneidad temporal de la validación convergente",
  paste0(
    "Las asociaciones por subperiodo describen heterogeneidad temporal; no causalidad. ",
    fuente_general
  ),
  "anexo"
)

A8c <- rezagos |>
  dplyr::transmute(
    `Proxy` = safe_label_proxy(proxy),
    `Horizonte (meses)` = h,
    `N` = n_obs,
    `Pearson adelantado` = round(pearson_h, 4),
    `Spearman adelantado` = round(spearman_h, 4)
  )

guardar_tabla(
  A8c,
  "Anexo_A8c_Asociaciones_Adelantadas",
  "Anexo A8c. Asociaciones adelantadas descriptivas a 1, 3 y 6 meses",
  paste0(
    "Estas correlaciones son descriptivas y no constituyen validación predictiva. ",
    fuente_general
  ),
  "anexo"
)

# A9. HAC niveles, diferencias y winsor.
A9 <- hac_niveles |>
  dplyr::select(
    proxy,
    n_obs,
    beta_icvlf,
    se_niv = se_hac,
    t_niv = t_hac,
    p_niv = p_hac,
    r2_niv = r2
  ) |>
  dplyr::left_join(
    hac_diferencias |>
      dplyr::select(
        proxy,
        beta_delta,
        se_dif = se_hac,
        t_dif = t_hac,
        p_dif = p_hac,
        r2_dif = r2
      ),
    by = "proxy"
  ) |>
  dplyr::left_join(
    hac_winsor |>
      dplyr::select(
        proxy,
        beta_delta_w,
        se_hac_w,
        t_hac_w,
        p_hac_w,
        r2_w,
        n_cook_gt_4n,
        max_cook
      ),
    by = "proxy"
  ) |>
  dplyr::transmute(
    `Proxy` = safe_label_proxy(proxy),
    `N` = n_obs,
    `β nivel` = fmt4(beta_icvlf),
    `p nivel` = fmt_p(p_niv),
    `β Δ` = fmt4(beta_delta),
    `EE HAC Δ` = fmt4(se_dif),
    `t HAC Δ` = fmt4(t_dif),
    `p HAC Δ` = fmt_p(p_dif),
    `R² Δ` = fmt4(r2_dif),
    `β winsor` = fmt4(beta_delta_w),
    `p winsor` = fmt_p(p_hac_w),
    `Cook > 4/n` = n_cook_gt_4n,
    `Cook máximo` = fmt4(max_cook)
  )

guardar_tabla(
  A9,
  "Anexo_A9_HAC_y_Sensibilidad",
  "Anexo A9. Consistencia HAC, primeras diferencias y sensibilidad a extremos",
  paste0(
    "Newey–West con 12 rezagos. Las estimaciones expresan asociación y no causalidad. ",
    "La winsorización 1–99% es solo sensibilidad. ",
    fuente_general
  ),
  "anexo"
)

# A10. Leave-one-out.
A10 <- dplyr::bind_rows(
  loo_indicador |>
    dplyr::transmute(
      `Nivel` = "Indicador",
      `Elemento omitido` = indicador_omitido,
      `Dimensión` = unname(etiquetas_bloque[bloque]),
      `N` = n_comparables,
      `Pearson` = round(pearson_vs_base, 4),
      `Spearman` = round(spearman_vs_base, 4),
      `DAM analítica` = round(dam_z, 4)
    ),
  loo_dimension |>
    dplyr::transmute(
      `Nivel` = "Dimensión",
      `Elemento omitido` = as.character(dimension),
      `Dimensión` = as.character(dimension),
      `N` = n_comparables,
      `Pearson` = round(pearson_vs_base, 4),
      `Spearman` = round(spearman_vs_base, 4),
      `DAM analítica` = round(dam_z, 4)
    )
)

guardar_tabla(
  A10,
  "Anexo_A10_Leave_One_Out",
  "Anexo A10. Robustez leave-one-indicator y leave-one-dimension",
  paste0(
    "DAM = diferencia absoluta media respecto del índice principal en escala analítica. ",
    fuente_general
  ),
  "anexo"
)

# A11. Benchmark de alcance.
A11 <- benchmark_alcance |>
  dplyr::transmute(
    `Benchmark` = benchmark,
    `Pearson vs. ICVLF integral` = round(
      pearson_vs_icvlf_integral,
      4
    ),
    `Spearman vs. ICVLF integral` = round(
      spearman_vs_icvlf_integral,
      4
    ),
    `Lectura` = lectura
  )

guardar_tabla(
  A11,
  "Anexo_A11_Benchmark_Alcance",
  "Anexo A11. Benchmark de alcance estrecho Liquidez + Fondeo",
  paste0(
    "Este benchmark cambia el constructo al excluir Activos, Cartera y Solvencia; ",
    "por ello no se interpreta como fallo de robustez metodológica. ",
    fuente_general
  ),
  "anexo"
)

# A12. BIC completo.
A12 <- bic_rupturas |>
  dplyr::mutate(
    seleccion = ifelse(
      BIC == min(BIC, na.rm = TRUE),
      "BIC mínimo",
      ""
    )
  ) |>
  dplyr::transmute(
    `Número de rupturas` = n_rupturas,
    `BIC` = round(BIC, 4),
    `Selección` = seleccion
  )

guardar_tabla(
  A12,
  "Anexo_A12_BIC_Rupturas",
  "Anexo A12. Selección del número de rupturas Bai–Perron mediante BIC",
  paste0(
    "Se selecciona la partición que minimiza BIC. ",
    fuente_general
  ),
  "anexo"
)

# A13. Estrés completo.
A13a <- calibracion_estres |>
  dplyr::transmute(
    `Dimensión` = as.character(dimension_label),
    `Estado actual` = round(estado_actual_z, 4),
    `Q95 histórico` = round(q95_historico_z, 4),
    `Brecha a Q95` = round(brecha_hasta_q95, 4),
    `Aporte Δ ICVLF` = round(aporte_delta_icvlf_z, 4)
  )

guardar_tabla(
  A13a,
  "Anexo_A13a_Calibracion_Estres",
  "Anexo A13a. Calibración del estrés Q95 por dimensión",
  paste0(
    "La brecha a Q95 mide distancia desde el estado de diciembre de 2025 y no ",
    "importancia estructural de la dimensión. ",
    fuente_general
  ),
  "anexo"
)

A13b <- estres_q95 |>
  dplyr::transmute(
    `Código` = codigo,
    `Escenario` = escenario,
    `Dimensión(es)` = dimensiones_afectadas,
    `Calibración` = calibracion,
    `ICVLF analítico` = round(ICVLF_simulado_z, 4),
    `Δ analítico` = round(delta_z, 4),
    `ICVLF 0–100 ref.` = round(ICVLF_simulado_100_ref, 2),
    `Tipo` = tipo
  )

guardar_tabla(
  A13b,
  "Anexo_A13b_Estres_Q95",
  "Anexo A13b. Escenarios contrafactuales de estrés Q95",
  paste0(
    "Los escenarios son determinísticos. Un valor superior a 100 es posible porque ",
    "la escala 0–100 es una referencia histórica retrospectiva no truncada. ",
    fuente_general
  ),
  "anexo"
)

A13c <- estres_de |>
  dplyr::transmute(
    `Código` = codigo,
    `Escenario` = escenario,
    `Dimensión(es)` = dimensiones_afectadas,
    `Calibración` = calibracion,
    `ICVLF analítico` = round(ICVLF_simulado_z, 4),
    `Δ analítico` = round(delta_z, 4),
    `ICVLF 0–100 ref.` = round(ICVLF_simulado_100_ref, 2)
  )

guardar_tabla(
  A13c,
  "Anexo_A13c_Estres_1_5DE",
  "Anexo A13c. Sensibilidad mecánica de estrés +1.5 desviaciones estándar",
  paste0(
    "Es una sensibilidad simétrica bajo pesos dimensionales iguales; no ordena ",
    "importancia económica de las dimensiones. ",
    fuente_general
  ),
  "anexo"
)

# A14. Rolling.
A14a <- dplyr::bind_rows(
  rolling_60_resumen |>
    dplyr::mutate(ventana = "60 meses"),
  rolling_24_resumen |>
    dplyr::mutate(ventana = "24 meses")
) |>
  dplyr::transmute(
    `Ventana` = ventana,
    `Serie` = serie,
    `N ventanas válidas` = n,
    `Media` = round(media, 4),
    `Mínimo` = round(minimo, 4),
    `Fecha mínimo` = format(fecha_min, "%Y-%m"),
    `Máximo` = round(maximo, 4),
    `Fecha máximo` = format(fecha_max, "%Y-%m"),
    `Último` = round(ultimo, 4),
    `Fecha último` = format(fecha_ultimo, "%Y-%m")
  )

guardar_tabla(
  A14a,
  "Anexo_A14a_Rolling_Correlaciones",
  "Anexo A14a. Resumen de asociaciones móviles de 60 y 24 meses",
  paste0(
    "La ventana de 60 meses es principal; 24 meses es sensibilidad. ",
    fuente_general
  ),
  "anexo"
)

A14b <- rolling_ar1_resumen |>
  dplyr::transmute(
    `Serie` = serie,
    `N ventanas válidas` = n,
    `Media` = round(media, 4),
    `Mínimo` = round(minimo, 4),
    `Fecha mínimo` = format(fecha_min, "%Y-%m"),
    `Máximo` = round(maximo, 4),
    `Fecha máximo` = format(fecha_max, "%Y-%m"),
    `Último` = round(ultimo, 4),
    `Fecha último` = format(fecha_ultimo, "%Y-%m")
  )

guardar_tabla(
  A14b,
  "Anexo_A14b_Persistencia_Rolling_AR1",
  "Anexo A14b. Persistencia descriptiva rolling AR(1) del ICVLF",
  paste0(
    "La beta AR(1) móvil se interpreta como persistencia descriptiva y no como ",
    "prueba de estacionariedad. ",
    fuente_general
  ),
  "anexo"
)

# A15. Extremos históricos.
A15 <- extremos_icvlf |>
  dplyr::transmute(
    `Tipo` = tipo,
    `Fecha` = format(Fecha, "%Y-%m"),
    `ICVLF analítico` = round(ICVLF_equal_z, 4),
    `ICVLF 0–100` = round(ICVLF_equal_100, 2),
    `Nivel histórico` = as.character(nivel_vulnerabilidad)
  )

guardar_tabla(
  A15,
  "Anexo_A15_Extremos_Historicos",
  "Anexo A15. Diez máximos y diez mínimos históricos del ICVLF",
  paste0(
    nota_escala_100, " ", fuente_general
  ),
  "anexo"
)

# =============================================================================
# 17. ANEXOS — FIGURAS
# =============================================================================

log_line("── 17. Anexos gráficos ──")

# AF1: robustez metodológica visual.
series_rob <- c(
  ICVLF_equal_100 = "Principal",
  ICVLF_alt_pesos_100 = "Pesos alternativos",
  ICVLF_pca_global_100 = "PCA global",
  ICVLF_pca_interno_100 = "PCA interno",
  ICVLF_robust_z_100 = "Normalización robusta"
)

rob_plot <- indices |>
  dplyr::select(
    Fecha,
    dplyr::all_of(names(series_rob))
  ) |>
  tidyr::pivot_longer(
    cols = -Fecha,
    names_to = "serie_id",
    values_to = "valor"
  ) |>
  dplyr::mutate(
    especificacion = unname(series_rob[serie_id])
  )

gA1 <- ggplot2::ggplot(
  rob_plot,
  ggplot2::aes(
    x = Fecha,
    y = valor,
    group = especificacion,
    color = especificacion
  )
) +
  ggplot2::geom_line(
    linewidth = 0.62,
    alpha = 0.88
  ) +
  ggplot2::scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  ggplot2::labs(
    title = "Anexo gráfico A1. Trayectoria del ICVLF bajo especificaciones alternativas",
    subtitle = "Sensibilidades metodológicas que preservan el constructo integral",
    x = NULL,
    y = "ICVLF (escala histórica 0–100)",
    caption = paste0(
      fuente_general, "\n",
      "Nota: el benchmark Liquidez + Fondeo se excluye porque modifica el constructo."
    )
  ) +
  theme_paper()

guardar_figura(
  gA1,
  "Anexo_Figura_A1_Robustez_Especificaciones",
  7.2, 4.8, "anexo"
)

# AF2: trayectoria BIC.
gA2 <- ggplot2::ggplot(
  bic_rupturas,
  ggplot2::aes(
    x = n_rupturas,
    y = BIC
  )
) +
  ggplot2::geom_line(
    color = col_principal,
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    color = col_principal,
    size = 2.2
  ) +
  ggplot2::geom_point(
    data = bic_min,
    color = col_acento,
    size = 3.1
  ) +
  ggplot2::scale_x_continuous(
    breaks = sort(unique(bic_rupturas$n_rupturas))
  ) +
  ggplot2::labs(
    title = "Anexo gráfico A2. Selección de rupturas estructurales mediante BIC",
    subtitle = "Bai–Perron sobre el nivel medio del ICVLF analítico",
    x = "Número de rupturas",
    y = "BIC",
    caption = paste0(
      fuente_general, "\n",
      "Nota: la especificación seleccionada corresponde al BIC mínimo."
    )
  ) +
  theme_paper()

guardar_figura(
  gA2,
  "Anexo_Figura_A2_BIC_Rupturas",
  6.5, 4.3, "anexo"
)

# =============================================================================
# 18. EXCEL DE RESPALDO
# =============================================================================

log_line("── 18. Workbooks Excel ──")

tablas_cuerpo <- list(
  "T4_1_Hechos" = T41,
  "T4_2_Subperiodos" = T42,
  "T4_3_Validacion" = T43,
  "T4_4_Robustez" = T44,
  "T4_5_Rupturas" = T45
)

tablas_anexo <- list(
  "A1_Marco" = A1,
  "A2_Indicadores" = A2,
  "A3_Descriptivos" = A3,
  "A4_ADF" = A4,
  "A5_Correlaciones" = A5,
  "A6a_PCA" = A6a,
  "A6b_Pesos_PCA" = A6b,
  "A7a_Estacion" = A7a,
  "A7b_Integracion" = A7b,
  "A8a_Val_Global" = A8a,
  "A8b_Val_Periodos" = A8b,
  "A8c_Adelantadas" = A8c,
  "A9_HAC" = A9,
  "A10_LOO" = A10,
  "A11_Benchmark" = A11,
  "A12_BIC" = A12,
  "A13a_CalibStress" = A13a,
  "A13b_StressQ95" = A13b,
  "A13c_StressDE" = A13c,
  "A14a_Rolling" = A14a,
  "A14b_AR1" = A14b,
  "A15_Extremos" = A15
)

exportar_excel <- function(lista, ruta) {
  
  wb <- openxlsx::createWorkbook()
  
  style_header <- openxlsx::createStyle(
    fontName = "Arial",
    fontSize = 10,
    textDecoration = "bold",
    fgFill = "#DDE6F1",
    fontColour = "#111827",
    halign = "center",
    valign = "center",
    border = "Bottom",
    borderColour = "#64748B",
    wrapText = TRUE
  )
  
  style_body <- openxlsx::createStyle(
    fontName = "Arial",
    fontSize = 9,
    valign = "top",
    wrapText = TRUE
  )
  
  for (nm in names(lista)) {
    
    df <- as.data.frame(
      lista[[nm]],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    openxlsx::addWorksheet(
      wb,
      sheetName = substr(nm, 1, 31)
    )
    
    sh <- substr(nm, 1, 31)
    
    openxlsx::writeData(
      wb,
      sh,
      df,
      headerStyle = style_header,
      borders = "none"
    )
    
    if (nrow(df) > 0 && ncol(df) > 0) {
      openxlsx::addStyle(
        wb,
        sh,
        style = style_body,
        rows = 2:(nrow(df) + 1),
        cols = seq_len(ncol(df)),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
    
    openxlsx::freezePane(
      wb,
      sh,
      firstActiveRow = 2
    )
    
    if (ncol(df) > 0) {
      for (j in seq_len(ncol(df))) {
        
        vals <- c(
          names(df)[j],
          as.character(df[[j]])
        )
        
        lens <- nchar(vals)
        lens <- lens[is.finite(lens)]
        
        mx <- if (length(lens) == 0) {
          12
        } else {
          max(lens, na.rm = TRUE)
        }
        
        ancho <- min(
          max(11, mx + 2),
          45
        )
        
        openxlsx::setColWidths(
          wb,
          sh,
          cols = j,
          widths = ancho
        )
      }
    }
  }
  
  openxlsx::saveWorkbook(
    wb,
    ruta,
    overwrite = TRUE
  )
  
  log_line("✓ Excel: ", basename(ruta))
}

ruta_excel_cuerpo <- file.path(
  dir_excel,
  "ICVLF_v6_3_Tablas_Cuerpo_Tesis.xlsx"
)

ruta_excel_anexos <- file.path(
  dir_excel,
  "ICVLF_v6_3_Tablas_Anexos.xlsx"
)

exportar_excel(
  tablas_cuerpo,
  ruta_excel_cuerpo
)

exportar_excel(
  tablas_anexo,
  ruta_excel_anexos
)

# =============================================================================
# 19. CATÁLOGO DE SALIDAS
# =============================================================================

catalogo <- tibble::tribble(
  ~tipo, ~codigo, ~ubicacion, ~titulo,
  "Tabla", "Tabla 4.1", "Cuerpo", "Hechos estilizados y diagnóstico de las series de entrada",
  "Figura", "Figura 4.1", "Cuerpo", "Perfil distributivo estandarizado de los 25 indicadores",
  "Tabla", "Tabla 4.2", "Cuerpo", "Evolución del ICVLF por subperiodo",
  "Figura", "Figura 4.2", "Cuerpo", "Trayectoria del ICVLF, cuartiles y rupturas",
  "Figura", "Figura 4.3", "Cuerpo", "Evolución de las cinco dimensiones",
  "Figura", "Figura 4.4", "Cuerpo", "Contribuciones por dimensión y subperiodo",
  "Tabla", "Tabla 4.3", "Cuerpo", "Validación convergente P1/P2",
  "Figura", "Figura 4.5", "Cuerpo", "Validación rolling P1/P2 a 60 meses",
  "Tabla", "Tabla 4.4", "Cuerpo", "Robustez ejecutiva",
  "Tabla", "Tabla 4.5", "Cuerpo", "Rupturas Bai–Perron con IC 95%",
  "Figura", "Figura 4.6", "Cuerpo", "Estrés contrafactual Q95",
  "Tabla", "A1", "Anexo", "Marco financiero de dimensiones",
  "Tabla", "A2", "Anexo", "Arquitectura de 25 indicadores",
  "Tabla", "A3", "Anexo", "Descriptivos completos",
  "Tabla", "A4", "Anexo", "ADF individual",
  "Tabla", "A5", "Anexo", "Correlaciones intradimensionales",
  "Tabla", "A6a", "Anexo", "Diagnóstico PCA",
  "Tabla", "A6b", "Anexo", "Pesos PCA de sensibilidad",
  "Tabla", "A7a", "Anexo", "ADF/PP/KPSS ICVLF y proxies",
  "Tabla", "A7b", "Anexo", "Resumen de integración",
  "Tabla", "A8a", "Anexo", "Validación global todos los proxies",
  "Tabla", "A8b", "Anexo", "Validación por subperiodo",
  "Tabla", "A8c", "Anexo", "Asociaciones adelantadas",
  "Tabla", "A9", "Anexo", "HAC y sensibilidad a extremos",
  "Tabla", "A10", "Anexo", "Leave-one-out completo",
  "Tabla", "A11", "Anexo", "Benchmark de alcance",
  "Tabla", "A12", "Anexo", "Trayectoria BIC",
  "Tabla", "A13a", "Anexo", "Calibración de estrés",
  "Tabla", "A13b", "Anexo", "Estrés Q95 completo",
  "Tabla", "A13c", "Anexo", "Estrés +1.5 d.e.",
  "Tabla", "A14a", "Anexo", "Rolling 60/24 meses",
  "Tabla", "A14b", "Anexo", "Persistencia rolling AR(1)",
  "Tabla", "A15", "Anexo", "Extremos históricos",
  "Figura", "A1", "Anexo", "Robustez visual de especificaciones",
  "Figura", "A2", "Anexo", "Selección BIC de rupturas"
)

utils::write.csv(
  catalogo,
  file.path(
    carpeta_b2,
    "CATALOGO_SALIDAS_BLOQUE2.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# =============================================================================
# 20. SALIDA MAESTRA IMPRESA / TXT
# =============================================================================

ruta_maestra <- file.path(
  dir_logs,
  "SALIDA_MAESTRA_ICVLF_v6_3_BLOQUE2.txt"
)

salida_txt <- capture.output({
  
  cat(
    paste0(
      "\n",
      strrep("=", 110),
      "\nICVLF v6.3 — BLOQUE 2 FINAL\n",
      strrep("=", 110),
      "\n"
    )
  )
  
  cat("RDS fuente: ", ruta_rds, "\n", sep = "")
  cat("Muestra: ", muestra_str, "\n", sep = "")
  cat("Carpeta de salida: ", normalizePath(carpeta_b2, winslash = "/", mustWork = TRUE), "\n\n", sep = "")
  
  cat("TABLA 4.1 — HECHOS ESTILIZADOS\n")
  print(T41, row.names = FALSE)
  
  cat("\nTABLA 4.2 — EVOLUCIÓN POR SUBPERIODO\n")
  print(T42, row.names = FALSE)
  
  cat("\nTABLA 4.3 — VALIDACIÓN P1/P2\n")
  print(T43, row.names = FALSE)
  
  cat("\nTABLA 4.4 — ROBUSTEZ EJECUTIVA\n")
  print(T44, row.names = FALSE)
  
  cat("\nTABLA 4.5 — RUPTURAS BAI–PERRON\n")
  print(T45, row.names = FALSE)
  
  cat("\nBENCHMARK DE ALCANCE\n")
  print(A11, row.names = FALSE)
  
  cat("\nCALIBRACIÓN DE ESTRÉS\n")
  print(A13a, row.names = FALSE)
  
  cat("\nESCENARIOS DE ESTRÉS Q95\n")
  print(A13b, row.names = FALSE)
  
  cat("\nRESUMEN ROLLING 60/24 MESES\n")
  print(A14a, row.names = FALSE)
  
  cat("\nCATÁLOGO DE SALIDAS\n")
  print(catalogo, row.names = FALSE)
  
  cat("\nNOTAS INTERPRETATIVAS\n")
  cat("- ", nota_analitica, "\n", sep = "")
  cat("- ", nota_escala_100, "\n", sep = "")
  cat("- P1/P2 no son LCR regulatorio.\n")
  cat("- Bai–Perron identifica cambios en nivel medio; no causalidad.\n")
  cat("- Los escenarios de estrés son contrafactuales determinísticos.\n")
  
})

writeLines(
  salida_txt,
  ruta_maestra,
  useBytes = TRUE
)

cat(paste(salida_txt, collapse = "\n"), "\n")

# =============================================================================
# 21. CONTROL FINAL
# =============================================================================

n_tab_cuerpo <- length(
  list.files(
    dir_tab_cuerpo,
    pattern = "\\.html$"
  )
)

n_fig_cuerpo_png <- length(
  list.files(
    dir_fig_cuerpo,
    pattern = "\\.png$"
  )
)

n_tab_anexo <- length(
  list.files(
    dir_tab_anexo,
    pattern = "\\.html$"
  )
)

n_fig_anexo_png <- length(
  list.files(
    dir_fig_anexo,
    pattern = "\\.png$"
  )
)

esperadas_tab_cuerpo <- 5L
esperadas_fig_cuerpo <- if (length(cols_roll) == 2) 6L else 5L
esperadas_tab_anexo <- 22L
esperadas_fig_anexo <- 2L

if (n_tab_cuerpo != esperadas_tab_cuerpo) {
  warning(
    "Control final: se esperaban ",
    esperadas_tab_cuerpo,
    " tablas del cuerpo y se encontraron ",
    n_tab_cuerpo,
    "."
  )
}

if (n_fig_cuerpo_png != esperadas_fig_cuerpo) {
  warning(
    "Control final: se esperaban ",
    esperadas_fig_cuerpo,
    " figuras PNG del cuerpo y se encontraron ",
    n_fig_cuerpo_png,
    "."
  )
}

if (n_tab_anexo != esperadas_tab_anexo) {
  warning(
    "Control final: se esperaban ",
    esperadas_tab_anexo,
    " tablas de anexo y se encontraron ",
    n_tab_anexo,
    "."
  )
}

if (n_fig_anexo_png != esperadas_fig_anexo) {
  warning(
    "Control final: se esperaban ",
    esperadas_fig_anexo,
    " figuras PNG de anexo y se encontraron ",
    n_fig_anexo_png,
    "."
  )
}

log_line("============================================================")
log_line("BLOQUE 2 FINAL COMPLETADO")
log_line("Tablas cuerpo: ", n_tab_cuerpo)
log_line("Figuras cuerpo PNG: ", n_fig_cuerpo_png)
log_line("Tablas anexos: ", n_tab_anexo)
log_line("Figuras anexos PNG: ", n_fig_anexo_png)
log_line("Excel cuerpo: ", ruta_excel_cuerpo)
log_line("Excel anexos: ", ruta_excel_anexos)
log_line("Salida maestra: ", ruta_maestra)
log_line("Carpeta general: ", normalizePath(carpeta_b2, winslash = "/", mustWork = TRUE))
log_line("============================================================")

cat("\n✓ BLOQUE 2 FINALIZADO SIN RECALCULAR EL ICVLF.\n")
