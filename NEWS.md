# survutils 1.0.2

Patch to fix test failures

Latest version of broom (0.5.0) returns tibbles now. As the `get_cox_res` 
function uses broom, the results were now being returned as tibbles. The 
`test_get_cox_res.R` uses `expect_equal` to test whether two data frames are 
equivalent, but this does not work for tibbles. This patch changes it such that 
the test first converts the tibbles to data frames to allow `expect_equal` to 
work.  

# survutils 1.0.1

* Adding a unit testing framework (#9)
* Add check for whether endpoint and endpoint.code input values were swapped (#7)
* Fix "match.arg(broom.fun) : 'arg' should be one of “tidy”, “glance”" error when running get_cox_res() (#11)
* Add default significance line for plot_cox_res (#11)

# survutils 1.0.0

* First major release of survutils
