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
        testthat::expect_equal(digest::digest(round(answer5_1_1, 3)), "03b3b7e9967823ca8ae75c138a3aa39c")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 5.1.2 is correct, good job!",
      failure_message = "Answer 5.1.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer5_1_2, 5)), "908d1fd10b357ed0ceaaec823abf81bc")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 5.1.3 is correct, good job!",
      failure_message = "Answer 5.1.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer5_1_3, 3)), "08d81b4d231a011dc547069387c944d9")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 5.1.4 is correct, good job!",
      failure_message = "Answer 5.1.4 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer5_1_4, 2)), "cac038f2d49684bc5e4fc485a40a53f5")
      }
    )
  )
)