# ============================================================
# QUARTO WEB HELPERS
# ============================================================

load_b1 <- function() {

  path <- file.path(
    "output",
    "bloque1",
    "diagnosticos",
    "OBJETOS_ICVLF_v6_3_BLOQUE1.rds"
  )

  if (!file.exists(path)) {

    stop(
      paste0(
        "\nNo existe el RDS metodológico.\n",
        "Ejecute primero:\n\n",
        "Rscript run_all.R\n"
      )
    )

  }

  readRDS(path)

}


fmt_num <- function(
  x,
  digits = 2
) {

  ifelse(
    is.na(x),
    "S/D",
    formatC(
      x,
      format = "f",
      digits = digits,
      big.mark = ","
    )
  )

}


fmt_pct <- function(
  x,
  digits = 1
) {

  ifelse(
    is.na(x),
    "S/D",
    paste0(
      formatC(
        x,
        format = "f",
        digits = digits
      ),
      "%"
    )
  )

}


plot_icvlf <- function(indices) {

  if (
    requireNamespace(
      "plotly",
      quietly = TRUE
    )
  ) {

    plotly::plot_ly(

      data = indices,

      x = ~Fecha,

      y = ~ICVLF_equal_100,

      type = "scatter",

      mode = "lines",

      text = ~paste0(
        "<b>Date:</b> ",
        format(Fecha, "%Y-%m"),
        "<br>",
        "<b>ICVLF:</b> ",
        round(
          ICVLF_equal_100,
          2
        ),
        "<br>",
        "<b>Regime:</b> ",
        nivel_vulnerabilidad
      ),

      hoverinfo = "text",

      line = list(
        width = 2.5
      )

    ) |>

      plotly::layout(

        xaxis = list(
          title = ""
        ),

        yaxis = list(
          title = "ICVLF (0–100)"
        ),

        hovermode = "x unified",

        margin = list(
          l = 60,
          r = 20,
          t = 30,
          b = 50
        )

      )

  } else {

    plot(
      indices$Fecha,
      indices$ICVLF_equal_100,
      type = "l",
      xlab = "",
      ylab = "ICVLF (0–100)"
    )

  }

}


plot_dimensions <- function(indices) {

  bloques <- c(
    "liquidez",
    "fondeo",
    "activos",
    "cartera",
    "solvencia"
  )

  long <-
    tidyr::pivot_longer(
      indices[
        ,
        c(
          "Fecha",
          bloques
        )
      ],
      cols = -Fecha,
      names_to = "Dimension",
      values_to = "Value"
    )

  plotly::plot_ly(

    data = long,

    x = ~Fecha,

    y = ~Value,

    color = ~Dimension,

    type = "scatter",

    mode = "lines"

  ) |>

    plotly::layout(

      xaxis = list(
        title = ""
      ),

      yaxis = list(
        title = "Standardized dimension score"
      ),

      hovermode = "x unified"

    )

}