test = list(
  name = "Q2-autograder",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.1 is correct, good job!",
      failure_message = "Answer 2.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_1, 5)), "6a8e65e0821e8011c0f04d886dce9323")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.2 is correct, good job!",
      failure_message = "Answer 2.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_2, 5)), "908d1fd10b357ed0ceaaec823abf81bc")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.3 is correct, good job!",
      failure_message = "Answer 2.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_3, 5)), "908d1fd10b357ed0ceaaec823abf81bc")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.4 is correct, good job!",
      failure_message = "Answer 2.4 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_4, 3)), "72544651fd4af02e85544a197c5a199d")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.5 is correct, good job!",
      failure_message = "Answer 2.5 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_5, 3)), "db8e490a925a60e62212cefc7674ca02")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.6 is correct, good job!",
      failure_message = "Answer 2.6 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_6, 3)), "db8e490a925a60e62212cefc7674ca02")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.7 is correct, good job!",
      failure_message = "Answer 2.7 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_7, 3)), "03b3b7e9967823ca8ae75c138a3aa39c")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.8 is correct, good job!",
      failure_message = "Answer 2.8 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_8, 3)), "3c3b9d75cc0e8cfcc29f40abd17afe8a")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.9 is correct, good job!",
      failure_message = "Answer 2.9 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_9, 2)), "75dc8b7b8724a54d1fba4cc109438cfb")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.10 is correct, good job!",
      failure_message = "Answer 2.10 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_10, 3)), "db8e490a925a60e62212cefc7674ca02")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.11 is correct, good job!",
      failure_message = "Answer 2.11 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_11, 2)), "84ffee348056dd833765fe8efc0d26b4")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.12 is correct, good job!",
      failure_message = "Answer 2.12 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_12, 5)), "908d1fd10b357ed0ceaaec823abf81bc")
      }
    )
  )
)