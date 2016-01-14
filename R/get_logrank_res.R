#' Run Log-Rank Test
#'
#' \code{get_logrank_res} is a wrapper over the survival::survdiff() function
#' return the direct results or the log rank p-value only if specified.
#'
#' @param in.formula Survival model formula. Can extract from an existing 
#'   survfit object with formula(survfit).
#' @param in.df data.frame Corresponding data for the survival model.
#' @param return.p If set to TRUE, return only the log rank p-value.
#' @return Results of survdiff or a log rank p-value if return.p is set to 
#'   TRUE.
#' @export
#' @examples
#'  
#' # Get survdiff results
#' fit <- survfit(Surv(time, status) ~ rx, data = colon)
#' get_logrank_res(formula(fit), colon)
#' 
#' # Get only log-rank p-value
#' get_logrank_res(formula(fit), colon, return.p = TRUE)
get_logrank_res <- function(in.formula, in.df, return.p = FALSE) {

	survdiff.res <- survival::survdiff(in.formula, 
																		 data = in.df)

	if (return.p) {
		1 - pchisq(survdiff.res$chisq, length(survdiff.res$n) - 1)
	} else {
		survdiff.res
	}
}
