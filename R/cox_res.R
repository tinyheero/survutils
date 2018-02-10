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
#' @param broom.fun Which broom function to run on the cox regression results.
#' @param group Column name containing the groups to run cox regression on. If,
#'   specified, cox regression is run separately for each group.
#' @return Cox regression results returned in a tidy data.frame format.
#' @export
#' @examples
#' library("survival")
#' endpoint <- "time"
#' endpoint.code <- "status"
#'
#' # Run Univariate Cox Regression on Single Feature
#' features <- "age"
#' test.df <- get_cox_res(colon, endpoint, endpoint.code, features)
#'
#' # Run Univariate Cox Regression on Multiple Features
#' multi.features <- c("age", "obstruct")
#' get_cox_res(colon, endpoint, endpoint.code, multi.features)
#'
#' # Run Univariate Cox Regression on Multiple Features For Each rx group
#' group <- "rx"
#' get_cox_res(colon, endpoint, endpoint.code, multi.features, group)
#'
#' # Run Multivariate Cox Regression 
#' get_cox_res(colon, endpoint, endpoint.code, multi.features)
#'
#' # Run Multivariate Cox Regression For Each rx Group
#' get_cox_res(colon, endpoint, endpoint.code, multi.features, group)
get_cox_res <- function(
  in.df, 
  endpoint, endpoint.code, 
  features, 
  group = NULL,
  broom.fun = c("tidy", "glance")
) {

  # Input Checking
  if (is.null(features)) {
    stop("No features specified")
  } 

  if (length(features) == 1) {
    message("Detected only one feature. Running univariate cox regression")
    test.type <- "unicox"
  } else {
    message("Detected multiple features. Running multivariate cox regression")
    test.type <- "multicox"
  }

  broom.fun <- match.arg(broom.fun)

  # Checking if All Columns are Present
  input.col.names <- c(endpoint, endpoint.code, features)
  if (!all(input.col.names %in% colnames(in.df))) {
    missing.col.names <- 
      input.col.names[which(!input.col.names %in% colnames(in.df))]
    stop(paste0("Missing columns: ", 
                paste(missing.col.names, collapse = ", ")))
  }

  # Check that endpoint and endpoint.code were not accidentally swapped
  endpoint_code_factors <- levels(factor(in.df[[endpoint.code]]))
  if (length(endpoint_code_factors) > 2) {
    stop(
      glue::glue(
        "Error: More than two factor levels in your endpoint.code. 
        Did you swap your endpoint and endpoint.code columns?"
      )
    )
  }

  # Running Cox Regression
  # exponentiate = TRUE in broom is used to convert into hazard ratios
  resp.var <- paste0("survival::Surv(", endpoint, ", ", endpoint.code, ")")

  features.formula <- paste(features, collapse = "+")
  cox.formula <- as.formula(paste0(resp.var, "~", features.formula))

  if (is.null(group)) {
    # Run Cox Regression 
    if (broom.fun == "tidy") {
      cox.res.df <- 
        survival::coxph(formula = cox.formula, data = in.df) %>%
        broom::tidy(exponentiate = TRUE)
    } else if (broom.fun == "glance") {
      cox.res.df <- 
        survival::coxph(formula = cox.formula, data = in.df) %>%
        broom::glance()
    }

  } else {
    # Split into group and then run Cox regression
    split.call <- lazyeval::interp(quote(list(.$a)),
                                   a = group)

    cox.res.df <- in.df %>%
      split(f = eval(split.call)) 

    if (broom.fun == "tidy") {
      cox.res.df <- 
        cox.res.df %>%
        purrr::map(~
          survival::coxph(formula = cox.formula, data = .) %>%
          broom::tidy(exponentiate = TRUE)
        ) 
    } else if (broom.fun == "glance") {
      cox.res.df <- 
        cox.res.df %>%
        purrr::map(~
          survival::coxph(formula = cox.formula, data = .) %>%
          broom::glance()
        ) 
    }
    cox.res.df <- dplyr::bind_rows(cox.res.df, .id = "group")
  }

  # Add test.type as column to output data.frame
  mutate.call <- lazyeval::interp(~ a, a = test.type)
  
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
#' @param cox.res.df data.frame output from \code{get_cox_res}.
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
#' @param facet.formula Facet formula for faceting the plot. This should be
#'   used plotting results from \code{iter_get_cox_res} or when the parameter 
#'   group is used in \code{get_cox_res} and \code{iter_get_cox_res}.
#' @param facet.scales Parameter passed to the scales parameter in 
#'   \code{ggplot2::facet_grid}.
#' @param add_sig_line Boolean to indicate if a red, dotted, vertical line 
#'   should be added to allow users to see if a Cox regression confidence 
#'   interval overlaps with 1.
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
#' plot_cox_res(cox.res.df, facet.formula = ". ~ group")
#' 
#' # Change x and y labels
#' plot_cox_res(cox.res.df, facet.formula = ". ~ group", 
#'              x.lab = "Hazard Ratio", y.lab = "Feature")
#' 
#' # Adding colors
#' cox.res.df %>%
#'   mutate(sig_flag = p.value < 0.05) %>%
#'   plot_cox_res(facet.formula = ". ~ group", x.lab = "Hazard Ratio", 
#'                y.lab = "Feature", 
#'                color.col = "sig_flag", 
#'                color.legend.name = "Significant Flag")
#'
#' # Flipping Plot
#' cox.res.df %>%
#'   mutate(sig_flag = p.value < 0.05) %>%
#'   plot_cox_res(facet.formula = ". ~ group", x.lab = "Hazard Ratio", 
#'                y.lab = "Feature", 
#'                color.col = "sig_flag", 
#'                color.legend.name = "Significant Flag", 
#'                coord.flip = TRUE)
plot_cox_res <- function(
  cox.res.df, 
  x.lab, y.lab, y.col = "term", 
  color.col, color.legend.name, 
  coord.flip = FALSE, 
  facet.formula = NULL, facet.scales = "fixed", 
  add_sig_line = TRUE
) {

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

  p <- add_sig_line_to_plot(p, coord.flip, add_sig_line)

  if (!missing(color.col)) {
    p <- p + ggplot2::aes_string(color = color.col)

    if (!missing(color.legend.name)) {
      message("Setting Color Legend Name")
      p <- p + ggplot2::scale_color_discrete(name = color.legend.name)
    }
  }

  if (!is.null(facet.formula)) {
    p <- p + ggplot2::facet_grid(facets = facet.formula,
                                 scales = facet.scales)
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

#' Add a Cox regression statistical significance line 
#'
#' This function will add a Cox regression statistical significance line to the
#' input ggplot2.
#' 
#' @param cur_plot ggplot2 object to add on to.
#' @param coord_flip Boolean to indicate if the ggplot2 has been flipped or not.
#' @param add_sig_line Boolean to indicate whether the significance line should
#'   be added or not.
add_sig_line_to_plot <- function(cur_plot, coord_flip, add_sig_line) {

  if (!add_sig_line) {
    return(cur_plot)
  }

  message("Adding significance line")

  if (!coord_flip) {
    cur_plot <- 
      cur_plot + 
      ggplot2::geom_vline(
        xintercept = 1, linetype = "dotted", color = "red"
      )
  } else {
    cur_plot <- 
      cur_plot + 
      ggplot2::geom_hline(
        yintercept = 1, linetype = "dotted", color = "red"
      )
  }

  cur_plot
}
