# =============================================================================
# report_helpers.R
# Helpers mínimos para el informe Quarto ICVLF
# =============================================================================

load_icvlf_objects <- function() {

  path <- file.path(
    "output",
    "bloque1",
    "diagnosticos",
    "OBJETOS_ICVLF_v6_3_BLOQUE1.rds"
  )

  if (!file.exists(path)) {
    stop(
      "No existe el RDS del Bloque 1 en: ",
      path,
      "\nEjecute primero: Rscript run_all.R"
    )
  }

  out <- readRDS(path)

  if (!is.list(out)) {
    stop("El RDS del Bloque 1 no contiene una lista válida.")
  }

  out
}


include_publication_table <- function(path) {

  if (!file.exists(path)) {
    stop("No se encontró la tabla publicable: ", path)
  }

  html <- paste(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )

  body <- sub(
    "(?is).*<body[^>]*>(.*)</body>.*",
    "\\1",
    html,
    perl = TRUE
  )

  if (identical(body, html)) {
    stop(
      "No se pudo extraer <body> de la tabla HTML: ",
      path
    )
  }

  cat(body)
}


body_table <- function(filename) {
  file.path(
    "output",
    "bloque2",
    "01_CUERPO",
    "TABLAS_HTML",
    filename
  )
}


annex_table <- function(filename) {
  file.path(
    "output",
    "bloque2",
    "02_ANEXOS",
    "TABLAS_HTML",
    filename
  )
}


body_figure <- function(filename) {

  path <- file.path(
    "output",
    "bloque2",
    "01_CUERPO",
    "FIGURAS",
    filename
  )

  if (!file.exists(path)) {
    stop("No se encontró la figura publicable: ", path)
  }

  path
}


annex_figure <- function(filename) {

  path <- file.path(
    "output",
    "bloque2",
    "02_ANEXOS",
    "FIGURAS",
    filename
  )

  if (!file.exists(path)) {
    stop("No se encontró la figura de anexo: ", path)
  }

  path
}


fmt_num <- function(x, digits = 2) {

  if (
    length(x) == 0 ||
    is.na(x[1]) ||
    !is.finite(x[1])
  ) {
    return("S/D")
  }

  sprintf(
    paste0("%.", digits, "f"),
    as.numeric(x[1])
  )
}


fmt_signed <- function(x, digits = 3) {

  if (
    length(x) == 0 ||
    is.na(x[1]) ||
    !is.finite(x[1])
  ) {
    return("S/D")
  }

  sprintf(
    paste0("%+.", digits, "f"),
    as.numeric(x[1])
  )
}


month_year_es <- function(x) {

  if (
    length(x) == 0 ||
    is.na(x[1])
  ) {
    return("S/D")
  }

  meses <- c(
    "enero", "febrero", "marzo", "abril",
    "mayo", "junio", "julio", "agosto",
    "septiembre", "octubre", "noviembre", "diciembre"
  )

  d <- as.Date(x[1])

  paste(
    meses[as.integer(format(d, "%m"))],
    format(d, "%Y")
  )
}


year_month <- function(x) {

  if (
    length(x) == 0 ||
    is.na(x[1])
  ) {
    return("S/D")
  }

  format(as.Date(x[1]), "%Y-%m")
}


safe_min <- function(x) {

  x <- x[
    is.finite(x) &
      !is.na(x)
  ]

  if (length(x) == 0) {
    return(NA_real_)
  }

  min(x)
}


safe_max <- function(x) {

  x <- x[
    is.finite(x) &
      !is.na(x)
  ]

  if (length(x) == 0) {
    return(NA_real_)
  }

  max(x)
}