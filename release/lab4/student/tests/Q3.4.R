test = list(
  name = "Q3.4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "The results should be assigned to an object called lambda_sequence_values",
      code = {
        testthat::expect_true(exists("lambda_sequence_values"))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "lambda_sequence_values should be a data frame.",
      code = {
        testthat::expect_true("data.frame" %in% class(lambda_sequence_values))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 2,
      failure_message = "Wrong lambda sequence.",
      code = {
        testthat::expect_equal(digest::digest(round(sum(lambda_sequence_values$possible_lambdas), 2)), "0dfbee83ad543f2a1eca2455ccb693b3")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 2,
      failure_message = "Wrong log-likelihood sequence.",
      code = {
        testthat::expect_equal(digest::digest(round(sum(lambda_sequence_values$log_likelihood), 2)), "02678265526ba6d0fe8eb230626c91ff")
      }
    )
  )
)