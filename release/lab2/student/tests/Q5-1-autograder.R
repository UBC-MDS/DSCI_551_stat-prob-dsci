test = list(
  name = "Q5-1-autograder",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 5.1.1 is correct, good job!",
      failure_message = "Answer 5.1.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer5_1_1, 1)), "0aee9b78301d7ec8998971363be87c03")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 5.1.2 is correct, good job!",
      failure_message = "Answer 5.1.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer5_1_2, 1)), "db8e490a925a60e62212cefc7674ca02")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 5.1.3 is correct, good job!",
      failure_message = "Answer 5.1.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer5_1_3, 2)), "17e2b03db8be9f746a1ba78d4bb279b3")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 5.1.4 is correct, good job!",
      failure_message = "Answer 5.1.4 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer5_1_4, 1)), "6a8e65e0821e8011c0f04d886dce9323")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 5.1.5 is correct, good job!",
      failure_message = "Answer 5.1.5 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer5_1_5, 1)), "908d1fd10b357ed0ceaaec823abf81bc")
      }
    )
  )
)