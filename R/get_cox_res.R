#' Run Cox Regression on a Single or Multiple Groups of Data
#'
#' \code{get_cox_res} is a wrapper around coxph. It can run
#' univariate or multivariate cox regression. If the group parameter is used,
#' then cox regression is run for each group separately. 
#'
#' The data is returned in a broom::tidy data.frame format.
#'
#' @param in.df Input data.frame.
#' @param endpoint Column name of the endpoint.
#' @param endpoint.code Column name of the endpoint status code.
#' @param features Vector containing the features to run cox regression on.
#' @param group Column name containing the groups to run cox regression on. If,
#'   specified, cox regression is run separately for each group.
#' @return Cox regression results returned in a tidy data.frame format.
#' @export
#' @examples
#' library("survival")
#'
#' # Run Univariate Cox Regression on Entire data.frame
#' endpoint <- "time"
#' endpoint.code <- "status"
#' features <- "age"
#' get_cox_res(colon, endpoint, endpoint.code, features)
#'
#' # Run Multivariate Cox Regression on Entire data.frame
#' endpoint <- "time"
#' endpoint.code <- "status"
#' features <- c("age", "obstruct")
#' get_cox_res(colon, endpoint, endpoint.code, features)
#
#' # Run Multivariate Cox Regression For Each rx Group
#' endpoint <- "time"
#' endpoint.code <- "status"
#' features <- c("age", "obstruct")
#' group <- "rx"
#' get_cox_res(colon, endpoint, endpoint.code, features, group)
get_cox_res <- function(in.df, endpoint, endpoint.code, features, group) {

  # Checking if All Columns are Present
  input.col.names <- c(endpoint, endpoint.code, features)
  if (!all(input.col.names %in% colnames(in.df))) {
    missing.col.names <- 
      input.col.names[which(!input.col.names %in% colnames(in.df))]
    stop(paste0("Missing columns: ", 
                paste(missing.col.names, collapse = ", ")))
  }

  # Setup Cox Formula
  features.formula <- paste(features, collapse = "+")
  resp.var <- paste0("Surv(", endpoint, ", ", endpoint.code, ")")
  cox.formula <- as.formula(paste0(resp.var, "~", features.formula))

  # Running Cox Regression
  if (missing(group)) {
    # Run Cox Regression on Whole data.frame
    survival::coxph(formula = cox.formula, data = in.df) %>%
    broom::tidy(exponentiate = TRUE)
  } else {
    # Run Cox Regression on "groups" in the data.frame
    in.df %>%
      dplyr::group_by_(.dots = group) %>%
      dplyr::do(cox.res = survival::coxph(formula = cox.formula, data = .)) %>%
      dplyr::rowwise() %>%
      broom::tidy(cox.res, exponentiate = TRUE)
  }
}
