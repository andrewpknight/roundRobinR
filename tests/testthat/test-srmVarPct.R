test_that("srmVarPct returns a data.frame with two columns", {
  d <- createDummies(
    group.id       = "groupId",
    act.id         = "actId",
    part.id        = "partId",
    d              = sampleDyadData[sampleDyadData$timeId == 1, ],
    merge.original = TRUE
  )
  o <- nlme::lme(
    liking ~ 1,
    random = list(groupId = nlme::pdBlocked(list(
      nlme::pdIdent(~1),
      pdSRM(~ -1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)
    ))),
    correlation = nlme::corCompSymm(form = ~1 | groupId / pdSRM_dyad_id),
    data        = d,
    na.action   = stats::na.omit
  )
  result <- srmVarPct(o)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 6)
})
