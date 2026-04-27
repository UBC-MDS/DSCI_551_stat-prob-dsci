test = list(
  name = "Q3-2-autograder",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 3.2 is correct, good job!",
      failure_message = "Answer 3.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(round(answer3_2, 4)), "c85c2d03263b2d30604893d6266ea787")
      }
    )
  )
)