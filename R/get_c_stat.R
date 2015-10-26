#' Calculate C-statistics
#' 
#' Wrapper around the Inf.Cval function from the survC1 R package to calculate
#' C-statistics.
#'
#' @param in.df data.frame containing all the input data
#' @param endpoint Column name of endpoint
#' @param endpoint.code Column name of endpoint code
#' @param prog.factor Column name of the prognostic factor to test
#' @param tau Tau to be used for C-statistics inference
#' @return data.frame containing the c-statistic, 95% CI, and standard error
#' @examples 
#' # Example taken from survC1
#' library("survival")
#' in.df <- survC1::CompCase(pbc[1:200, c(2:4,10:14)])
#' in.df[, 2] <- as.numeric(in.df[,2]==2)
#' tau <- 365.25*8
#' prog.factor <- c("trt", "edema", "bili", "chol", "albumin", "copper")
#' get_c_stat(in.df, "time", "status", prog.factor, tau)
get_c_stat <- function(in.df, endpoint, endpoint.code, prog.factor, tau.val) {

  message(paste0("Endpoint: ", endpoint))
  message(paste0("Endpoint code: ", endpoint.cod
  message(paste0("Prognostic Factor: ", paste(prog.factor, collapse = ", ")))

  # Prepare input data.frame to be used with survC1::Inf.Cval
  col.select <- c(endpoint, endpoint.code, prog.factor)
  in.sub.df <- dplyr::select_(in.df, .dots = col.select)
  in.sub.df <- survC1::CompCase(in.sub.df)
  in.sub.df <- data.frame(in.sub.df)

  c.stat.res <- survC1::Inf.Cval(in.sub.df, tau.val, itr = nrow(in.sub.df))
  data.frame(c_stat = c.stat.res$Dhat,
             low95 = c.stat.res$low95,
             upp95 = c.stat.res$upp95,
             se = c.stat.res$se)
}
