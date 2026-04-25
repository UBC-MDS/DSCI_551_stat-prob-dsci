test = list(
  name = "Q3-1-autograder",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 3.1 is correct, good job!",
      failure_message = "Answer 3.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer3_1, 4)), "9d539e287bb866fe8daf7e703f05d488")
      }
    )
  )
)