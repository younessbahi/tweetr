test_that("trend status:200", {
  expect_length(tweetr::get_score('#rstats'), 4)
})

test_that("trend status:200", {
  expect_length(tweetr::get_score('russia'), 4)
})