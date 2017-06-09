#' @title Pintar mapas
#' @description Dado un objeto tatatatatata
#' @param cartografia objeto de la clase Spatial
#' @param variable vector num
#' @param n_grupos valor numérico
#' @param title título del gráfico
#' @export
pinta_mapas <- function(cartografia, variable, n_grupos,title=NULL) {
  paleta <- RColorBrewer::brewer.pal(n_grupos, "BrBG")[1:n_grupos]
  grupos <- quantile(variable, probs = seq(0, 1, 1 / n_grupos))
  pcorte <- c(grupos[1] - 0.5, grupos[2:n_grupos], grupos[n_grupos + 1] + 0.5)
  colores <- paleta[
    findInterval(variable[match(cartografia@data$CUSEC, privacion_valencia[, 1])], pcorte)
    ]
  leyenda <- c()
  for (j in 2:length(pcorte)) {
    leyenda[j] <- paste0(round(pcorte[j - 1], 2), " - ", round(pcorte[j], 2))
  }
  sp::plot(cartografia, col = colores,title = title)
  legend("bottomright", leyenda[-1],border = NULL, fill = paleta, bty = "n")
}
