test = list(
  name = "Q4-autograder",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.1 is correct, good job!",
      failure_message = "Answer 4.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_1, 2)), "a7a1f44dc49f8d2ae309ba947e1252d5")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.2 is correct, good job!",
      failure_message = "Answer 4.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_2, 0)), "e5b57f323c7b3719bbaaf9f96b260d39")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.3 is correct, good job!",
      failure_message = "Answer 4.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_3, 2)), "2a1ea47875e195a421d56ae3f6621d32")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 4.4 is correct, good job!",
      failure_message = "Answer 4.4 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_4, 2)), "015206a6c4770b8eedf2710f45cae1f1")
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
        testthat::expect_equal(digest::digest(round(answer4_6, 2)), "74ff560dd5944e16be3179156880b66e")
      }
    )
  )
)