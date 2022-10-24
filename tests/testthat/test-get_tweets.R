test_that("tweets status:200", {
  expect_length(get_tweets(query = '#rstats', .count = 40), 4)
})