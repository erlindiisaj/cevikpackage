test_that("Global Workspace’de check_spotify_keys adlı bir değişken var.", {
  expect_true(exists("check_spotify_keys"))
})

test_that("check_spotify_keys adlı değişkenin tipi “function” olmalı.", {
  expect_true(is.function(check_spotify_keys))
})
