# roundRobinR (development version)

## New: Co-Partner Social Relations Model (`cpsrmRun()`)

* Added `cpsrmRun()`, `print.cpsrm()`, and `summary.cpsrm()` for fitting the
  Co-Partner SRM (CP-SRM), in which each observation involves one actor and
  two simultaneous partners (e.g. threesomes), via REML with a Woodbury
  matrix identity formulation.
* `print.cpsrm()`/`summary.cpsrm()` now report variance-of-total percentages
  under BOTH the RAW convention (each component's own share of total
  variance; the correct basis for comparing Partner's effect size to
  Actor's) and the COMBINED convention (Partner's contribution scaled by
  the number of partners summed into each row; the correct basis for a
  full total-variance decomposition), whenever `weight.partners = FALSE`
  and the number of partners per row is constant. Previously only one
  (RAW, unlabeled) convention was shown, with an ambiguous in-code comment
  about what it represented.
* `?cpsrmRun` now documents (a) the RAW vs. COMBINED distinction and why
  conflating them can make partner effects look several times more or
  less important than actor effects than they really are, and (b) the
  boundary-corrected likelihood-ratio test procedure for testing whether a
  variance component is zero (one-sided halved chi-square for a single
  component; the three-part 1/4-1/2-1/4 mixture for a joint two-component
  test), since variance components are boundary-constrained and the naive
  two-sided/unhalved chi-square test is not valid here.

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
