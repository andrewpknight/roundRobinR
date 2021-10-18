test_that("SRM variance decomposition is as expected", {
  d = createDummies(group.id="groupId", act.id="actId", part.id="partId", d=sampleDyadData, include.self=FALSE, merge.original=TRUE)
  d = d[d$timeId == 1, ]

  o = nlme::lme(liking ~ 1, random=list(groupId=nlme::pdBlocked(list(nlme::pdIdent(~1),
                                                                 pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
            correlation=nlme::corCompSymm(form=~1 | groupId/pdSRM_dyad_id),
            data=
              createDummies(group.id="groupId", act.id="actId", part.id="partId",
                            d=sampleDyadData[sampleDyadData$timeId==1, ],
                            merge.original=TRUE), na.action=na.omit)
  o.pct = srmVarPct(o)
  expect_equal(o.pct[1,1], 0.122)
  expect_equal(o.pct[2,1], 0.808)
  expect_equal(o.pct[3,1], 0.027)
  expect_equal(o.pct[4,1], 0.275)
  expect_equal(o.pct[5,1], 0.002)
  expect_equal(o.pct[6,1], 0.009)

  expect_equal(o.pct[1,2], 9.926)
  expect_equal(o.pct[2,2], 65.596)
  expect_equal(o.pct[3,2], 2.154)
  expect_equal(o.pct[4,2], 22.324)
  expect_equal(o.pct[5,2], 0.011)
  expect_equal(o.pct[6,2], 0.033)
})
