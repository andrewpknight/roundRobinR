# roundRobinR 1.0.0

## Initial CRAN release

* First submission to CRAN.
* Functions for manipulating round-robin dyadic data: `createDummies()`.
* Functions for fitting the Social Relations Model via multilevel modeling:
  `srmRun()`, `srmVarPct()`, `srmPseudoRSq()`.
* Custom `nlme` covariance class `pdSRM` implementing the SRM
  variance-covariance constraints (equal actor variances, equal partner
  variances, single actor-partner covariance).
* Sample dataset `sampleDyadData` with simulated round-robin data from
  two time points.
* Vignette: "Introduction to roundRobinR".

## Bug fixes vs. GitHub version (0.0.0.9000)

* Fixed reference to `groupId` column in `srmRun()` that used the variable
  name string rather than the column contents when creating the
  `pdSRM_group_id` grouping variable.
* Replaced `class(x) == "try-error"` with `inherits(x, "error")` in
  `pdMatrix.pdSRM()` per CRAN policy.
* Added `stats::` namespace prefixes to calls to `formula()`, `na.omit()`,
  and `coef()` throughout.
* Wrapped long-running examples in `\donttest{}` to comply with CRAN
  example time limits.
