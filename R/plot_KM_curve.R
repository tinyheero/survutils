#' Create a Kaplan-Meier plot using ggplot2
#'
#' This a modified version of the original ggkm code by Ahijit Dasgupta. It
#' generates a Kaplan-Meier plot using ggplot2
#'
#' @param sfit a \code{\link[survival]{survfit}} object
#' @param returns logical: if \code{TRUE}, return an ggplot object
#' @param xlabs x-axis label
#' @param ylabs y-axis label
#' @param ystratalabs The strata labels. \code{Default = levels(summary(sfit)$strata)}
#' @param ystrataname The legend name. Default = “Strata”
#' @param timeby numeric: control the granularity along the time-axis
#' @param main plot title
#' @param pval logical: add the pvalue to the plot?
#' @return a ggplot is made. if returns=TRUE, then an ggplot object
#'   is returned
#' @author Abhijit Dasgupta \url{https://gist.github.com/araastat/9927677}
#' @examples
#' fit <- survival::survfit(survival::Surv(time,status) ~ rx, data = survival::colon)
#' plot_KM_curve(fit, timeby = 500)
#' @export
plot_KM_curve <- function(sfit, returns = FALSE, xlabs = "Time", 
                 ylabs = "survival probability", ystratalabs = NULL, 
                 ystrataname = NULL, timeby = 100, main = "Kaplan-Meier Plot", 
                 pval = TRUE, ...) {

  if (is.null(ystratalabs)) {
    ystratalabs <- as.character(levels(summary(sfit)$strata))
  }

  m <- max(nchar(ystratalabs))
  if (is.null(ystrataname)) {
    ystrataname <- "Strata"
  }
  times <- seq(0, max(sfit$time), by = timeby)

  .df <- data.frame(time = sfit$time, n.risk = sfit$n.risk,
                    n.event = sfit$n.event, surv = sfit$surv, 
                    strata = summary(sfit, censored = T)$strata,
                    upper = sfit$upper, lower = sfit$lower)

  levels(.df$strata) <- ystratalabs

  zeros <- data.frame(time = 0, surv = 1, 
                      strata = factor(ystratalabs, levels=levels(.df$strata)), 
                      upper = 1, lower = 1)

  .df <- plyr::rbind.fill(zeros, .df)
  d <- length(levels(.df$strata))

  p <- ggplot2::ggplot(.df, ggplot2::aes(time, surv, group = strata)) +
    ggplot2::geom_step(ggplot2::aes(color = strata), size = 0.7) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(vjust = 0.5)) +
    ggplot2::scale_x_continuous(xlabs, breaks = times, limits = c(0, max(sfit$time))) +
    ggplot2::scale_y_continuous(ylabs, limits = c(0, 1)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = c(ifelse(m < 10, .28, .35), 
                                       ifelse(d < 4, .25, .35))) +
    ggplot2::theme(legend.key = ggplot2::element_rect(colour = NA)) +
    ggplot2::theme(plot.margin = grid::unit(c(0, 1, .5, ifelse(m < 10, 1.5, 2.5)), "lines")) +
    ggplot2::ggtitle(main)
  
  if (pval) {
      sdiff <- survival::survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
      pval <- pchisq(sdiff$chisq, length(sdiff$n)-1, lower.tail = FALSE)
      pvaltxt <- ifelse(pval < 0.0001, "p < 0.0001", paste("p =", signif(pval, 3)))
      p <- p + ggplot2::annotate("text", x = 0.6 * max(sfit$time), y = 0.1, label = pvaltxt)
  }
  p
}
