test_that("srmRun returns a list with lme.output and srm.output", {
  out <- srmRun(
    dv      = "liking",
    groupId = "groupId",
    actId   = "actId",
    partId  = "partId",
    data    = sampleDyadData[sampleDyadData$timeId == 1, ]
  )
  expect_type(out, "list")
  expect_named(out, c("lme.output", "srm.output"))
})

test_that("srmRun lme.output is an lme object", {
  out <- srmRun(
    dv      = "liking",
    groupId = "groupId",
    actId   = "actId",
    partId  = "partId",
    data    = sampleDyadData[sampleDyadData$timeId == 1, ]
  )
  expect_s3_class(out$lme.output, "lme")
})

test_that("srmRun srm.output has correct row names", {
  out <- srmRun(
    dv      = "liking",
    groupId = "groupId",
    actId   = "actId",
    partId  = "partId",
    data    = sampleDyadData[sampleDyadData$timeId == 1, ]
  )
  expected_rows <- c(
    "Group", "Actor", "Partner", "Dyad",
    "Generalized Reciprocity", "Dyadic Reciprocity"
  )
  expect_equal(rownames(out$srm.output), expected_rows)
})

test_that("srmRun variance percentages sum to 100", {
  out <- srmRun(
    dv      = "liking",
    groupId = "groupId",
    actId   = "actId",
    partId  = "partId",
    data    = sampleDyadData[sampleDyadData$timeId == 1, ]
  )
  pcts <- out$srm.output[
    c("Group", "Actor", "Partner", "Dyad"),
    "percents.and.correlations"
  ]
  expect_equal(sum(pcts), 100, tolerance = 0.1)
})

test_that("srmRun variance components are non-negative", {
  out <- srmRun(
    dv      = "liking",
    groupId = "groupId",
    actId   = "actId",
    partId  = "partId",
    data    = sampleDyadData[sampleDyadData$timeId == 1, ]
  )
  vcs <- out$srm.output[
    c("Group", "Actor", "Partner", "Dyad"),
    "variances.and.covariances"
  ]
  expect_true(all(vcs >= 0))
})

test_that("srmRun with fixed effects returns correct structure", {
  out <- srmRun(
    dv      = "liking",
    groupId = "groupId",
    actId   = "actId",
    partId  = "partId",
    feVars  = c("actEx", "partEx"),
    data    = sampleDyadData[sampleDyadData$timeId == 1, ]
  )
  expect_s3_class(out$lme.output, "lme")
  fe <- nlme::fixef(out$lme.output)
  expect_true("actEx"  %in% names(fe))
  expect_true("partEx" %in% names(fe))
})
