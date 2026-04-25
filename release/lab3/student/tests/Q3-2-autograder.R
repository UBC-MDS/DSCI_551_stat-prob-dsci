test = list(
  name = "Q3-2-autograder",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 3.2.1 is correct, good job!",
      failure_message = "Answer 3.2.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer3_2_1, 3)), "26f886db3148d0fbf83f8f0bf492cf51")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 3.2.2 is correct, good job!",
      failure_message = "Answer 3.2.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer3_2_2, 3)), "00f3fa27c01aee5e7633e06a130c827e")
      }
    )
  )
)