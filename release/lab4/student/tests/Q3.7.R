test = list(
  name = "Q3.7",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "The result should be assigned to an object called analytical_mle",
      code = {
        testthat::expect_true(exists("analytical_mle"))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "Wrong analytical MLE result.",
      code = {
        testthat::expect_equal(digest::digest(round(analytical_mle, 2)), "a3564ee4b8babf66fab2345315ad5823")
      }
    )
  )
)