test = list(
  name = "Q4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.1 is correct, good job!",
      failure_message = "Answer 4.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_1, 3)), "f25b9881115381f5b0c19f8989a28a7e")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.2 is correct, good job!",
      failure_message = "Answer 4.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_2, 2)), "f4aa569dea6fe5d5ccf5d00ba35029a4")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.3 is correct, good job!",
      failure_message = "Answer 4.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_3, 1)), "c4fecd2200bfc28e365a80b3cd4790d9")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.4 is correct, good job!",
      failure_message = "Answer 4.4 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_4, 2)), "a7a1f44dc49f8d2ae309ba947e1252d5")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.5 is correct, good job!",
      failure_message = "Answer 4.5 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_5, 0)), "6717f2823d3202449301145073ab8719")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.6 is correct, good job!",
      failure_message = "Answer 4.6 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_6, 0)), "5d4ca65e8b2fb8f85cec1a0eb5dd463f")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.7 is correct, good job!",
      failure_message = "Answer 4.7 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_7, 2)), "facdb5bd41c6d89eee20067187a98f08")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.8 is correct, good job!",
      failure_message = "Answer 4.8 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_8, 2)), "78c860cf185d5c870193d8654a5010cc")
      }
    )
  )
)