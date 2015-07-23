#' Returns a Number At Risk Table from a survfit Object
#'
#' This function generates a number at risk table that typically seen in
#' publications.
#' 
#' @param sfit A survival::survfit object.
#' @param timeby The "step" in which to calculate the risk.
#' @return A data.frame with the number of risks at each timeby step.
#' @author Abhijit Dasgupta \url{https://gist.github.com/araastat/9927677}.
#' @export
#' @examples
#' fit <- survival::survfit(survival::Surv(time,status) ~ rx, data = survival::colon)
#' get_nrisk_tbl(fit, timeby = 500)
get_nrisk_tbl <- function(sfit, timeby){
  times <- seq(0, max(sfit$time), by = timeby)
  risk.data <- 
    data.frame(strata = summary(sfit, times = times, extend = TRUE)$strata,
               time = summary(sfit, times = times, extend = TRUE)$time,
               n.risk = summary(sfit, times = times, extend = TRUE)$n.risk)
  risk.data
} 
