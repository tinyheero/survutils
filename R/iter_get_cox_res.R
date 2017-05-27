#' Runs get_cox_res Over a Range of Features
#'
#' This is a modified version of \code{get_cox_res} allowing for multiple runs
#' of get_cox_res.
#'
#' @inheritParams get_cox_res
#' @param features This must be a list of features.
#' @return List of data frames with each data frame being the output of 
#'   \code{get_cox_res}.
#' @export
#' @examples
#' library("survival")
#' endpoint <- "time"
#' endpoint.code <- "status"
#' 
#' # Run Cox Regression on List of Features
#' features <- list(c("age", "obstruct"),
#'                  c("nodes"))
#' 
#' iter_get_cox_res(colon, endpoint, endpoint.code, features,
#'                   group = "rx")
iter_get_cox_res <- function(in.df, endpoint, endpoint.code, features, 
                             broom.fun = c("tidy", "glance"), group = NULL) {

  if (!is.list(features)) {
    stop("features must be a list")
  }

  # Iterate over list of features running get_cox_res
  purrr::map(features, 
    function(x) {
      get_cox_res(in.df = in.df,
                  endpoint = endpoint,
                  endpoint.code = endpoint.code,
                  features = x,
                  broom.fun = broom.fun,
                  group = group)
    }
  ) %>%
  dplyr::bind_rows(.id = "iter_num")
}
