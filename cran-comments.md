# CRAN Submission Comments

## Test environments

* Local: macOS (Apple Silicon), R 4.4.x
* win-builder: R-devel, R-release
* R-hub: Ubuntu Linux, Fedora Linux, Windows

## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is:

```
New submission
```

This is expected for a new package submission.

## Downstream dependencies

This is a new package. There are no existing reverse dependencies.

## Notes for CRAN reviewers

* The package implements a custom `nlme` covariance class (`pdSRM`) to
  enforce the variance-covariance constraints of the Social Relations Model
  (Kenny et al., 2006; Snijders & Kenny, 1999). This requires several S3
  method exports for internal `nlme` generics (`pdConstruct`, `pdMatrix`,
  `corMatrix`, `coef`, `summary`).

* Examples for `srmPseudoRSq()` and the internal `pdSRM` class methods are
  wrapped in `\donttest{}` because they require fitting two `lme` models,
  which exceeds the 5-second CRAN example time limit on some platforms.

* The `sampleDyadData` dataset is simulated and included in `data/` as an
  `.rda` file. `LazyData: true` is set in DESCRIPTION.
