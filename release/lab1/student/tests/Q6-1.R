test = list(
  name = "Q6-1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 6.1.1 is correct, good job!",
      failure_message = "Answer 6.1.1 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(answer6_1_1), "9ad0d92384b698bc64167bdc58c987cb")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 6.1.2 is correct, good job!",
      failure_message = "Answer 6.1.2 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(answer6_1_2), "be418dc448434b789db399e8e95434ac")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 6.1.3 is correct, good job!",
      failure_message = "Answer 6.1.3 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(answer6_1_3), "32573f776c45e2670f0b5cd39753d24b")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 6.1.4 is correct, good job!",
      failure_message = "Answer 6.1.4 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(answer6_1_4), "32573f776c45e2670f0b5cd39753d24b")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      success_message = "Answer 6.1.5 is correct, good job!",
      failure_message = "Answer 6.1.5 is wrong.",
      code = {
        testthat::expect_equal(digest::digest(answer6_1_5), "d3d95d8d997f0044317d41e1a75ce9ec")
      }
    )
  )
)