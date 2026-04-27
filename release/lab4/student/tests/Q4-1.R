test = list(
  name = "Q4-1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "The result should be assigned to an object called answer4_1",
      code = {
        testthat::expect_true(exists("answer4_1"))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "Wrong theoretical probability.",
      code = {
        testthat::expect_equal(digest::digest(round(answer4_1, 2)), "6a1559ccb503c66a3a9e68c012eb9baa")
      }
    )
  )
)