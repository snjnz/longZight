test_that("refactor_wave provides correct responses", {
  f <- tibble::tibble(id = rep(1:3, each = 3), wave = c(2,3,1,2,3,1,1,2,3))
  f2 <- tibble::tibble(id = rep(1:3, each = 3), wave = letters[1:9])
  data1 <- load_long_data(f, time_col = wave)
  data2 <- load_long_data(f2, time_col = wave)

  expect_visible((res1 <- refactor_wave(data1)))
  expect_equal(nrow(res1), 9)

  expect_visible((res2 <- refactor_wave(data1, subset = c(1,2))))
  expect_equal(nrow(res2), 6)

  expect_visible((res1 <- refactor_wave(data2)))
  expect_equal(nrow(res1), 9)

  expect_visible((res2 <- refactor_wave(data2, subset = c(1,2))))
  expect_equal(nrow(res2), 0)
})
