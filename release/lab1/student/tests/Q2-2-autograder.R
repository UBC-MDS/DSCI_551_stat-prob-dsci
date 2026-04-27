test = list(
  name = "Q2-2-autograder",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.2.1 is correct, good job!",
      failure_message = "Answer 2.2.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_2_1, 1)), "ade3a57d5b8bfcb2b8aac4e85a93c941")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.2.2 is correct, good job!",
      failure_message = "Answer 2.2.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_2_2, 1)), "dffc43e7028ff2d3605dc21a31104a1c")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.2.3 is correct, good job!",
      failure_message = "Answer 2.2.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_2_3, 1)), "0a2b2c3b572db73b98dbd97701863742")
      }
    )
  )
)