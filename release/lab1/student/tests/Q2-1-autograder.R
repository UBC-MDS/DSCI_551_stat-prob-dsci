test = list(
  name = "Q2-1-autograder",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.1.1 is correct, good job!",
      failure_message = "Answer 2.1.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_1_1, 1)), "d66748df863de710fb11504ba3e75d38")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.1.2 is correct, good job!",
      failure_message = "Answer 2.1.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_1_2, 1)), "908d1fd10b357ed0ceaaec823abf81bc")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.1.3 is correct, good job!",
      failure_message = "Answer 2.1.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_1_3, 1)), "44a0f6df00182c825086d3d8225b2394")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.1.4 is correct, good job!",
      failure_message = "Answer 2.1.4 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_1_4, 1)), "24bc7e137af61b689875e4abfdbac9ad")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.1.5 is correct, good job!",
      failure_message = "Answer 2.1.5 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_1_5, 2)), "13a7bedd185d0cfcd7163cb371eee88a")
      }
    )
  )
)