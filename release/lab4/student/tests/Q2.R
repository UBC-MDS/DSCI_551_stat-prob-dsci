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
        testthat::expect_equal(digest::digest(round(answer2_1, 3)), "2cd40573001fb3aa2dde9e73db834f65")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.2 is correct, good job!",
      failure_message = "Answer 2.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_2, 3)), "afdb697d80000f2e1c8897ff3ca66c3d")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 2.3 is correct, good job!",
      failure_message = "Answer 2.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer2_3, 3)), "dde97989cd9f0ae8764512849a70d26b")
      }
    )
  )
)