test_that("invalid data types rejected", {
  x <- matrix(1:2, nrow = 2)
  expect_error(load_long_data(x), ".data must be a tibble or data.frame object")
})

test_that("parameter checks work", {
  f <- tibble::tibble(id = 1:3, foo = 2:4)
  expect_error(load_long_data(f), "id_col and time_col must be present in .data")
  expect_visible(load_long_data(f, time_col = "foo"))
  expect_visible(load_long_data(f, id, foo))
})
