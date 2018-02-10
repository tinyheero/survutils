#' Summarizes the Cox Regression Analysis
#'
#' This function summarizes the results of a cox regression returning the
#' results in a data.frame. It will calculate the confidence interval.
#' 
#' @param cox.res The result of a coxph fit.
#' @param ci The confidence interval to calculate.
#' @return A data.frame that summarizes the results.
#' @export
#' @examples
#' cox.res <- survival::coxph(survival::Surv(time,status) ~ rx, survival::colon)
#' get_cox_summary(cox.res)
# Description: This function is used to get the cox-regression results
get_cox_summary <- function(cox.res, ci = 95){
  beta <- coef(cox.res)
  se <- sqrt(diag(vcov(cox.res)))
  HR <- exp(beta)
  HRse <- HR * se

  # Calculate the tail to use for the confidence interval
  ci.tail <- 1 - ((100 - ci)/2)/100

  outDf <- data.frame(coef = beta, 
                      se = se, 
                      z = beta/se, 
                      p = 1 - pchisq((beta/se)^2, 1),
                      HR = HR,
                      HRse = HRse,
                      HRz = (HR - 1) / HRse, 
                      HRp = 1 - pchisq(((HR - 1)/HRse)^2, 1),
                      HRCILL = exp(beta - qnorm(ci.tail, 0, 1) * se),
                      HRCIUL = exp(beta + qnorm(ci.tail, 0, 1) * se))
  outDf <- round(outDf, 3)
  outDf
} 
