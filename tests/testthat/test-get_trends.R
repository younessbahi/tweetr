test_that("trend status:200", {
  expect_length(tweetr::get_trends(), 7)
})