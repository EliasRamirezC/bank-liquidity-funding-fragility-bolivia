# =============================================================================
# run_all.R
# Reproducción completa del ICVLF
# =============================================================================

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

cat("\n")
cat("============================================================\n")
cat("ICVLF — PIPELINE REPRODUCIBLE COMPLETO\n")
cat("============================================================\n\n")

required_files <- c(
  file.path("R", "01_build_icvlf.R"),
  file.path("R", "02_generate_outputs.R"),
  file.path("data", "raw", "Datos RL SB.xlsm")
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Faltan archivos requeridos:\n- ",
    paste(missing_files, collapse = "\n- ")
  )
}

cat("1/2 — Construyendo y auditando el ICVLF...\n\n")

source(
  file.path("R", "01_build_icvlf.R"),
  encoding = "UTF-8"
)

rds_expected <- file.path(
  "output",
  "bloque1",
  "diagnosticos",
  "OBJETOS_ICVLF_v6_3_BLOQUE1.rds"
)

if (!file.exists(rds_expected)) {
  stop(
    "El Bloque 1 terminó sin producir el RDS esperado: ",
    rds_expected
  )
}

cat("\n2/2 — Generando tablas, figuras y anexos publicables...\n\n")

source(
  file.path("R", "02_generate_outputs.R"),
  encoding = "UTF-8"
)

expected_outputs <- c(
  file.path(
    "output", "bloque2", "01_CUERPO", "TABLAS_HTML",
    "Tabla_4_1_Hechos_Estilizados.html"
  ),
  file.path(
    "output", "bloque2", "01_CUERPO", "TABLAS_HTML",
    "Tabla_4_2_ICVLF_Subperiodos.html"
  ),
  file.path(
    "output", "bloque2", "01_CUERPO", "TABLAS_HTML",
    "Tabla_4_3_Validacion_P1_P2.html"
  ),
  file.path(
    "output", "bloque2", "01_CUERPO", "TABLAS_HTML",
    "Tabla_4_4_Robustez_Ejecutiva.html"
  ),
  file.path(
    "output", "bloque2", "01_CUERPO", "TABLAS_HTML",
    "Tabla_4_5_Rupturas_Bai_Perron.html"
  ),
  file.path(
    "output", "bloque2", "01_CUERPO", "FIGURAS",
    "Figura_4_1_Perfil_Series_Entrada.png"
  ),
  file.path(
    "output", "bloque2", "01_CUERPO", "FIGURAS",
    "Figura_4_2_Trayectoria_ICVLF_Rupturas.png"
  ),
  file.path(
    "output", "bloque2", "01_CUERPO", "FIGURAS",
    "Figura_4_3_Subindices_Dimensiones.png"
  ),
  file.path(
    "output", "bloque2", "01_CUERPO", "FIGURAS",
    "Figura_4_4_Contribuciones_Subperiodo.png"
  ),
  file.path(
    "output", "bloque2", "01_CUERPO", "FIGURAS",
    "Figura_4_5_Rolling_P1_P2_60m.png"
  ),
  file.path(
    "output", "bloque2", "01_CUERPO", "FIGURAS",
    "Figura_4_6_Estres_Q95.png"
  )
)

missing_outputs <- expected_outputs[
  !file.exists(expected_outputs)
]

if (length(missing_outputs) > 0) {
  stop(
    "El Bloque 2 no produjo todas las salidas principales esperadas:\n- ",
    paste(missing_outputs, collapse = "\n- ")
  )
}

cat("\n")
cat("============================================================\n")
cat("PIPELINE COMPLETADO CORRECTAMENTE\n")
cat("============================================================\n")
cat("RDS:     ", rds_expected, "\n", sep = "")
cat("Salidas: output/bloque2/\n")
cat("Datos:   data/processed/\n")
cat("\nSiguiente paso: quarto render\n")
