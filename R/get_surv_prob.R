#' Get Survival Probability at Specified Times
#'
#' \code{get_surv_prob} retrieves the survival probability at a specific time
#' from a survival curve object from the survival::survfit function. The
#' survival curve object can only have one group.
#'
#' @param fit survival::survfit object.
#' @param times Vector of times to lookup survival probabilities.
#' @export
#' @return Vector of survival probabilities based on the input times.
#' @examples
#' library("survival")
#'
#' # Get Survival Probabilities Based on Whole Cohort
#' fit <- survfit(Surv(time, status) ~ 1, data = colon)
#' times <- c(100, 200, 300)
#' get_surv_prob(fit, times)
#'
#' # Get Survival Probabilities for Each rx Group
#' library("purrr")
#' library("dplyr")
#' library("reshape2")
#'
#' surv.prob.res <- 
#'   colon %>%
#'   split(.$rx) %>%
#'   map(~ survfit(Surv(time, status) ~ 1, data = .)) %>%
#'   map(get_surv_prob, times)
#'
#' surv.prob.res.df <- as_data_frame(surv.prob.res)
#' colnames(surv.prob.res.df) <- names(surv.prob.res)
#' surv.prob.res.df <-
#'   surv.prob.res.df %>%
#'   mutate(surv_prob_time = times)
#' 
#' surv.prob.res.df %>%
#'   melt(id.vars = "surv_prob_time", value.name = "surv_prob",
#'        variable.name = "group")
get_surv_prob <- function(fit, times) {
  stepfun(fit$time, c(1, fit$surv))(times)
}
