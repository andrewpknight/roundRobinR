test_that("createDummies returns a data.frame", {
  out <- createDummies(
    group.id = "groupId",
    act.id   = "actId",
    part.id  = "partId",
    d        = sampleDyadData
  )
  expect_s3_class(out, "data.frame")
})

test_that("createDummies excludes self-ratings by default", {
  out <- createDummies(
    group.id = "groupId",
    act.id   = "actId",
    part.id  = "partId",
    d        = sampleDyadData
  )
  expect_true(all(out$actId != out$partId))
})

test_that("createDummies includes self-ratings when requested", {
  out <- createDummies(
    group.id     = "groupId",
    act.id       = "actId",
    part.id      = "partId",
    d            = sampleDyadData,
    include.self = TRUE
  )
  expect_true(any(out$actId == out$partId))
})

test_that("createDummies actor dummy rows sum to 1", {
  out <- createDummies(
    group.id = "groupId",
    act.id   = "actId",
    part.id  = "partId",
    d        = sampleDyadData
  )
  a_cols   <- grep("^a[0-9]+$", names(out), value = TRUE)
  row_sums <- rowSums(out[, a_cols])
  expect_true(all(row_sums == 1))
})

test_that("createDummies partner dummy rows sum to 1", {
  out <- createDummies(
    group.id = "groupId",
    act.id   = "actId",
    part.id  = "partId",
    d        = sampleDyadData
  )
  p_cols   <- grep("^p[0-9]+$", names(out), value = TRUE)
  row_sums <- rowSums(out[, p_cols])
  expect_true(all(row_sums == 1))
})

test_that("createDummies dyad IDs are symmetric", {
  out <- createDummies(
    group.id = "groupId",
    act.id   = "actId",
    part.id  = "partId",
    d        = sampleDyadData
  )
  # i->j and j->i should share the same pdSRM_dyad_id
  merged <- merge(
    out[, c("groupId", "actId", "partId", "pdSRM_dyad_id")],
    out[, c("groupId", "actId", "partId", "pdSRM_dyad_id")],
    by.x = c("groupId", "actId", "partId"),
    by.y = c("groupId", "partId", "actId")
  )
  expect_true(all(merged$pdSRM_dyad_id.x == merged$pdSRM_dyad_id.y))
})

test_that("createDummies merge.original returns all original columns", {
  out <- createDummies(
    group.id       = "groupId",
    act.id         = "actId",
    part.id        = "partId",
    d              = sampleDyadData,
    merge.original = TRUE
  )
  original_cols <- names(sampleDyadData)
  expect_true(all(original_cols %in% names(out)))
})

test_that("createDummies produces correct number of rows (no self)", {
  # For a group of size n, expect n*(n-1) directed dyads
  d1 <- sampleDyadData[sampleDyadData$groupId == 1 &
                         sampleDyadData$timeId  == 1, ]
  n  <- length(unique(c(d1$actId, d1$partId)))
  out <- createDummies(
    group.id = "groupId",
    act.id   = "actId",
    part.id  = "partId",
    d        = d1
  )
  expect_equal(nrow(out), n * (n - 1))
})
