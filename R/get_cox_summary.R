#' Summarizes the Cox Regression Analsysis
#'
#' This function summarizes the results of a cox regression returning the
#' results in a data.frame. It will calculate the confidence interval.
#' 
#' @param cox.res A cox regression analysis results object.
#' @return A data.frame that summarizes the results.
#' @export
#' @examples
#' cox.res <- survival::coxph(survival::Surv(time,status) ~ rx, survival::colon)
#' get_cox_summary(cox.res)
# Description: This function is used to get the cox-regression results
get_cox_summary <- function(cox.res){
	beta <- coef(cox.res)
	se <- sqrt(diag(vcov(cox.res)))
	HR <- exp(beta)
	HRse <- HR * se

	outDf <- data.frame(coef = beta, 
                      se = se, 
                      z = beta/se, 
                      p = 1 - pchisq((beta/se)^2, 1),
                      HR = HR,
                      HRse = HRse,
                      HRz = (HR - 1) / HRse, 
                      HRp = 1 - pchisq(((HR - 1)/HRse)^2, 1),
                      HRCILL = exp(beta - qnorm(.975, 0, 1) * se),
                      HRCIUL = exp(beta + qnorm(.975, 0, 1) * se))
  outDf <- round(outDf, 3)
  outDf
} 
