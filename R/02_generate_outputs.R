
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
    "output",
    "bloque1",
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
  stop(
    "No se encontró ", nombre_rds, ".\n",
    "Ejecute primero Rscript run_all.R o el Bloque 1."
  )
}

ruta_rds <- normalizePath(
  ruta_rds_existente[1],
  winslash = "/",
  mustWork = TRUE
)

B1 <- readRDS(ruta_rds)

if (!is.list(B1)) {
  stop(
    "El archivo RDS no contiene una lista válida del Bloque 1."
  )
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
  "output",
  "bloque2"
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