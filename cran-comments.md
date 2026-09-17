## Test environments

* local macOS (aarch64-apple-darwin23), R 4.6.0
* win-builder (devel: R Under development (unstable) (2026-09-16 r90549 ucrt))
* win-builder (release: R 4.6.1 (2026-06-24 ucrt))

## R CMD check results

0 errors | 0 warnings | 0 notes

All three environments (local macOS, win-builder devel, win-builder release)
came back clean.

## Downstream dependencies

There are currently no reverse dependencies for this package.

## This submission

This is a major release (1.1.0 -> 2.0.0) that:

* Adds the Co-Partner Social Relations Model (cpsrm): a dyadic/triadic
  variance-decomposition model for data in which each observation involves
  one actor and all other members of a group acting simultaneously as
  partners (e.g., three-person teams). This is a new model class for the
  package, alongside the existing standard Social Relations Model (SRM)
  functionality. Includes a full-control fitting function (cpsrm_run())
  and a friendly wrapper (cpsrm()) that builds the required dummy matrices
  from raw long-format data.
* Renames several existing functions to snake_case for naming consistency
  (e.g., srmRun -> srm_run, createDummies -> create_dummies). Every old
  name remains available and fully functional, emitting a deprecation
  warning that points to the new name -- no user-facing breaking changes.
* Adds documentation on RAW vs. COMBINED variance-decomposition reporting
  and boundary-corrected likelihood-ratio-test guidance for variance
  components, in cpsrm_run()'s help page.
