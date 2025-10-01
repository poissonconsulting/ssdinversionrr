#' autoplot_fun
#'
#' @param x A fitdists object as returned by ssdtools.
#' @param y labels to plot on each grob.
#' @param plot.tag.position Tag position for labels.
#'
#' @importFrom ggpubr rremove
#'
#' @returns A ggplot2 grob of the fitdists object.
#' @export
autoplot_fun <- function(x, y, plot.tag.position = c(0, 0.95)) {
  ggplot2::autoplot(x, delta = Inf) +
    rremove("xlab") + rremove("ylab") +
    ggplot2::labs(tag = as.character(y)) +
    ggplot2::theme(
      title = ggplot2::element_blank(),
      plot.tag = ggplot2::element_text(size = 12),
      plot.tag.position = plot.tag.position
    )
}
