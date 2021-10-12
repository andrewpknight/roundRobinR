test_that("Data parsing yields correct output without self or original", {

  d = createDummies(group.id="groupId", act.id="actId", part.id="partId", d=sampleDyadData, include.self=FALSE, merge.original=FALSE)
  expect_equal(nrow(d), 792)
  expect_equal(length(d), 16)

})

test_that("Data parsing yields correct output with self but not original", {

  d = createDummies(group.id="groupId", act.id="actId", part.id="partId", d=sampleDyadData, include.self=TRUE, merge.original=FALSE)
  expect_equal(nrow(d), 1056)
  expect_equal(length(d), 16)

})

test_that("Data parsing yields correct output without self but with original", {

  d = createDummies(group.id="groupId", act.id="actId", part.id="partId", d=sampleDyadData, include.self=FALSE, merge.original=TRUE)
  expect_equal(nrow(d), 1584)
  expect_equal(length(d), 29)

})

test_that("Data parsing yields correct output with self and with original", {

  d = createDummies(group.id="groupId", act.id="actId", part.id="partId", d=sampleDyadData, include.self=TRUE, merge.original=TRUE)
  expect_equal(nrow(d), 1848)
  expect_equal(length(d), 29)

})
