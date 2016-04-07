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
#' endpoint <- "time"
#' endpoint.code <- "status"
#'
#' # Run Univariate Cox Regression on Entire data.frame
#' features <- "age"
#' get_cox_res(colon, endpoint, endpoint.code, features)
#'
#' # Run Multivariate Cox Regression on Entire data.frame
#' features <- c("age", "obstruct")
#' get_cox_res(colon, endpoint, endpoint.code, features)
#'
#' # Run Multivariate Cox Regression For Each rx Group
#' group <- "rx"
#' get_cox_res(colon, endpoint, endpoint.code, features, group)
get_cox_res <- function(in.df, endpoint, endpoint.code, features, group,
                        test.type = c("multicox", "unicox")) {

	test.type <- match.arg(test.type)

  # Checking if All Columns are Present
  input.col.names <- c(endpoint, endpoint.code, features)
  if (!all(input.col.names %in% colnames(in.df))) {
    missing.col.names <- 
      input.col.names[which(!input.col.names %in% colnames(in.df))]
    stop(paste0("Missing columns: ", 
                paste(missing.col.names, collapse = ", ")))
  }

  # Running Cox Regression
  # exponentiate = TRUE in broom is used to convert into hazard ratios
  resp.var <- paste0("survival::Surv(", endpoint, ", ", endpoint.code, ")")

  # If running multivariate cox regression
  if (test.type == "multicox") {
    features.formula <- paste(features, collapse = "+")
    cox.formula <- as.formula(paste0(resp.var, "~", features.formula))

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

  # If running univariate cox regression
  } else if (test.type == "unicox") {
    in.melt.df <- tidyr::gather_(in.df, "variable", "value", features)
    cox.formula <- as.formula(paste0(resp.var, "~ value"))

    if (missing(group)) {
      cox.res.df <- 
        in.melt.df %>%
        split(.$variable) %>%
        purrr::map(~ 
          survival::coxph(formula = cox.formula, data = .) %>%
          broom::tidy(exponentiate = TRUE) %>%
          dplyr::select_("-term")
        ) %>%
        dplyr::bind_rows(.id = "term")
    } 
  }

	# Add test.type as column to output data.frame
	mutate.call <- lazyeval::interp(~ a, a = as.name(test.type))

	cox.res.df <- 
		dplyr::mutate_(cox.res.df, .dots = setNames(list(mutate.call), "test_type"))
	cox.res.df
}

#' Plot Cox Regression Results
#' 
#' \code{plot_cox_res} takes the output from \code{get_cox_res} and generates
#' a forest plot showing the hazard ratio and confidence interval of the cox
#' cox regression. 
#' 
#' @param data.frame output from \code{get_cox_res}.
#' @param group Vector containing the column name(s) of the grouping variables.
#'   This is only applicable if multivariate cox regression was run. 
#'   The data will be faceted for each group. This currently only supports at
#'   most two column names (i.e. two-dimensions).
#' @param x.lab x-axis label.
#' @param y.lab y-axis label.
#' @param y.col Column name that contains the values for the y-values. 
#' @param color.col Column name that contains color groups.
#' @param color.legend.name Title for the color legend.
#' @param coord.flip By default hazard ratio and its confidence interval is 
#'   plotted on the y-axis using ggplot2::geom_errorbarh(). If this is set to
#'   TRUE, then this information is plotted along the x-axis using 
#'   ggplot2::geom_errorbar(). This means that the x.lab and y.lab will be 
#'   flipped to. 
#' @return Forest plot of cox regression results in the ggplot framework.
#' @export
#' @examples
#' library("survival")
#' library("magrittr")
#' library("dplyr")
#' 
#' in.df <- colon
#' endpoint <- "time"
#' endpoint.code <- "status"
#' 
#' # Run and Plot Multivariate Cox Regression on Entire data.frame
#' features <- c("age", "obstruct")
#' cox.res.df <- get_cox_res(colon, endpoint, endpoint.code, features)
#' plot_cox_res(cox.res.df)
#' 
#' # Run and Plot Multivariate Cox Regression For Each rx Group
#' group <- "rx"
#' cox.res.df <- get_cox_res(colon, endpoint, endpoint.code, features, group)
#' plot_cox_res(cox.res.df, group)
#' 
#' # Change x and y labels
#' plot_cox_res(cox.res.df, group, x.lab = "Hazard Ratio", y.lab = "Feature")
#' 
#' # Adding colors
#' cox.res.df %>%
#'   mutate(sig_flag = p.value < 0.05) %>%
#'   plot_cox_res(group = group, x.lab = "Hazard Ratio", y.lab = "Feature", 
#'                color.col = "sig_flag", 
#'                color.legend.name = "Significant Flag")
#'
#' # Flipping Plot
#' cox.res.df %>%
#'   mutate(sig_flag = p.value < 0.05) %>%
#'   plot_cox_res(group = group, x.lab = "Hazard Ratio", y.lab = "Feature", 
#'                color.col = "sig_flag", 
#'                color.legend.name = "Significant Flag", coord.flip = TRUE)
plot_cox_res <- function(cox.res.df, group, x.lab, y.lab, y.col = "term",
                         color.col, color.legend.name, coord.flip = FALSE) {

  if (!coord.flip) {
    p <- cox.res.df %>%
      ggplot2::ggplot(
        ggplot2::aes_string(y = y.col, x = "estimate", xmin = "conf.high", 
                            xmax = "conf.low")) +
      ggplot2::geom_errorbarh(height = 0.1) +
      ggplot2::geom_point()
  } else {
    message("Flipping Axis")
    p <- cox.res.df %>%
      ggplot2::ggplot(
        ggplot2::aes_string(x = y.col, y = "estimate", ymin = "conf.high", 
                            ymax = "conf.low")) +
      ggplot2::geom_errorbar(width = 0.1) +
      ggplot2::geom_point()
  }

  if (!missing(color.col)) {
    p <- p + ggplot2::aes_string(color = color.col)

    if (!missing(color.legend.name)) {
      message("Setting Color Legend Name")
      p <- p + ggplot2::scale_color_discrete(name = color.legend.name)
    }
  }
    
  if (!missing(group)) {
    p <- p + ggplot2::facet_grid(reformulate(group))
  }

  if (!missing(x.lab)) {
    message("Setting x-axis Title")
    p <- p + ggplot2::xlab(x.lab)
  }

  if (!missing(y.lab)) {
    message("Setting y-axis Title")
    p <- p + ggplot2::ylab(y.lab)
  }

    p
}
