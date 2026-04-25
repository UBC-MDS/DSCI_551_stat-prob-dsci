test = list(
  name = "Q7-autograder",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 7.1 is correct, good job!",
      failure_message = "Answer 7.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer7_1, 2)), "78c860cf185d5c870193d8654a5010cc")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 7.2 is correct, good job!",
      failure_message = "Answer 7.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer7_2, 2)), "aac62a0390a4ebd30bc133c8e3b21ef4")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 7.3 is correct, good job!",
      failure_message = "Answer 7.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer7_3, 2)), "e521ed5596735d8b4b29f83c57f471b5")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 7.4 is correct, good job!",
      failure_message = "Answer 7.4 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer7_4, 2)), "17510f1c35f73387c74b7373df4322dd")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 7.5 is correct, good job!",
      failure_message = "Answer 7.5 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer7_5, 2)), "6b114738e10a6ed3d070b2ea0e2c89ca")
      }
    )
  )
)