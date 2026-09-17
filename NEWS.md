# roundRobinR 2.0.0

## New: Co-Partner Social Relations Model (`cpsrm()`, `cpsrm_run()`)

* Added `cpsrm()`, a friendly entry point for the Co-Partner SRM (CP-SRM)
  that takes raw long-format data plus `actor_id`/`group_id` and builds
  the required actor/partner dummy matrices internally via
  `create_cp_dummies()`, then fits the model with `cpsrm_run()`.
* Added `cpsrm_run()`, the full-control fitting function underneath
  `cpsrm()`, together with `create_cp_dummies()`, `print.cpsrm()`, and
  `summary.cpsrm()`. `cpsrm_run()` fits the CP-SRM -- in which each
  observation involves one actor and all other members of a group acting
  simultaneously as partners (e.g. three-person teams) -- via REML, using
  either a Woodbury-matrix-identity ("block") or full-covariance-loop
  ("loop") formulation, and supports variable group sizes.
* `print.cpsrm()`/`summary.cpsrm()` report variance-of-total percentages
  under BOTH the RAW convention (each component's own share of total
  variance; the correct basis for comparing Partner's effect size to
  Actor's) and the COMBINED convention (Partner's contribution scaled by
  the number of partners summed into each row; the correct basis for a
  full total-variance decomposition), whenever `weight_partners = FALSE`
  and the number of partners per row is constant.
* `?cpsrm_run` documents (a) the RAW vs. COMBINED distinction and why
  conflating them can make partner effects look several times more or
  less important than actor effects than they really are, and (b) the
  boundary-corrected likelihood-ratio test procedure for testing whether a
  variance component is zero (one-sided halved chi-square for a single
  component; the three-part 1/4-1/2-1/4 mixture for a joint two-component
  test), since variance components are boundary-constrained and the naive
  two-sided/unhalved chi-square test is not valid here.

## Naming consistency: snake_case rename

* Renamed several existing functions to snake_case for consistency with
  the new `cpsrm()`/`cpsrm_run()`/`create_cp_dummies()` naming:
  `createDummies()` -> `create_dummies()`, `srmRun()` -> `srm_run()`,
  `srmVarPct()` -> `srm_var_pct()`, `srmPseudoRSq()` -> `srm_pseudo_rsq()`.
* Every renamed function's old name still works exactly as before and
  remains exported; calling it emits a one-time `.Deprecated()` warning
  pointing to the new name and new (also renamed) arguments. No
  user-facing breaking changes.

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
