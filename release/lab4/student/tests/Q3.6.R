test = list(
  name = "Q3.6",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "The result should be assigned to an object called empirical_mle",
      code = {
        testthat::expect_true(exists("empirical_mle"))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 3,
      failure_message = "Wrong empirical MLE result.",
      code = {
        testthat::expect_equal(digest::digest(round(empirical_mle, 2)), "b3b87cbcc9623440b2422fc7d1f3e38a")
      }
    )
  )
)