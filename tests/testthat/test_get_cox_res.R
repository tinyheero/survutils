context("Cox regression")

test_that(
  "get_cox_res correctly runs univariate Cox regression on a single feature", {

  library("survival")
  endpoint <- "time"
  endpoint.code <- "status"
  features <- "age"

  expected_out_df <- 
    structure(
      list(
        term = "age", 
        estimate = 0.997558880035054, 
        std.error = 0.00279486462785936, 
        statistic = -0.874498296537251, 
        p.value = 0.381846947490051,
        conf.low = 0.992109357489558, 
        conf.high = 1.003038336071,
        test_type = "unicox"
      ), 
    class = "data.frame", 
    .Names = 
      c("term", "estimate", "std.error", "statistic", "p.value", 
        "conf.low", "conf.high", "test_type"), 
      row.names = c(NA, -1L)
    )

  out_df <- get_cox_res(colon, endpoint, endpoint.code, features)
  expect_equal(object = out_df, expected = expected_out_df)
})
