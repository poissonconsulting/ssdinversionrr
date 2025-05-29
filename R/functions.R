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
  autoplot(x, delta = Inf) +
    rremove("xlab") + rremove("ylab") +
    labs(tag = as.character(y)) +
    theme(
      title = element_blank(),
      plot.tag = element_text(size = 12),
      plot.tag.position = plot.tag.position
    )
}
