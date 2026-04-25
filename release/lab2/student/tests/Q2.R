test = list(
  name = "Q2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.1 is correct, good job!",
      failure_message = "Answer 2.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_1, 1)), "3c3b9d75cc0e8cfcc29f40abd17afe8a")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.2 is correct, good job!",
      failure_message = "Answer 2.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_2, 1)), "f23662d0838c244acb308b71749ac22e")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.3 is correct, good job!",
      failure_message = "Answer 2.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_3, 1)), "af04a6f39588915a4dcac626c46434de")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.4 is correct, good job!",
      failure_message = "Answer 2.4 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_4, 2)), "cb516c1275960e4813c60d8d51d7c8e4")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.5 is correct, good job!",
      failure_message = "Answer 2.5 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_5, 2)), "37cd4e5174c65a7196eae5fed7c0a61e")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.6 is correct, good job!",
      failure_message = "Answer 2.6 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_6, 2)), "9f7307e7bf5007922e3b0bdef6c3142e")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.7 is correct, good job!",
      failure_message = "Answer 2.7 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_7, 2)), "aac62a0390a4ebd30bc133c8e3b21ef4")
      }
    )
  )
)