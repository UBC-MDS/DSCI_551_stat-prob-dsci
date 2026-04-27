test = list(
  name = "Q8-autograder",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.2,
      success_message = "Answer 8.1 is correct, good job!",
      failure_message = "Answer 8.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer8_1, 2)), "f5ae58aeed4bb9e2fc149bb4f6931b74")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.2,
      success_message = "Answer 8.2 is correct, good job!",
      failure_message = "Answer 8.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer8_2, 2)), "6717f2823d3202449301145073ab8719")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.2,
      success_message = "Answer 8.3 is correct, good job!",
      failure_message = "Answer 8.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer8_3, 2)), "76b5039c1aeabb0df142998a1e272111")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.2,
      success_message = "Answer 8.4 is correct, good job!",
      failure_message = "Answer 8.4 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer8_4, 2)), "ee48059132b8cdd8f1a1d9abbdaead78")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.2,
      success_message = "Answer 8.5 is correct, good job!",
      failure_message = "Answer 8.5 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer8_5, 2)), "9ec94824ffb63a1eae60605aef4dda02")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.2,
      success_message = "Answer 8.6 is correct, good job!",
      failure_message = "Answer 8.6 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer8_6, 2)), "2f38571c4dd8c539cb02b11ed6e7f8fd")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.2,
      success_message = "Answer 8.7 is correct, good job!",
      failure_message = "Answer 8.7 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer8_7, 2)), "9c2d69e299cb7adb52b0bfec05156e32")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.2,
      success_message = "Answer 8.8 is correct, good job!",
      failure_message = "Answer 8.8 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer8_8, 2)), "ebe557aaf54dcd977eb624e5fad200f2")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.2,
      success_message = "Answer 8.9 is correct, good job!",
      failure_message = "Answer 8.9 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer8_9, 2)), "908d1fd10b357ed0ceaaec823abf81bc")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.2,
      success_message = "Answer 8.10 is correct, good job!",
      failure_message = "Answer 8.10 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer8_10, 2)), "5d4ca65e8b2fb8f85cec1a0eb5dd463f")
      }
    )
  )
)