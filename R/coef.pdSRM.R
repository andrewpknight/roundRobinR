#' Extract Coefficients from a pdSRM Object
#'
#' Internal method called by \code{\link[nlme]{coef.pdMat}} to extract the
#' three underlying SRM parameters: actor standard deviation, partner
#' standard deviation, and actor-partner correlation.
#'
#' @param object an object inheriting from \code{pdSRM}
#' @param unconstrained logical; if \code{TRUE} (default) the unconstrained
#'   parameterization is returned; if \code{FALSE} the three named SRM
#'   parameters are returned
#' @param ... additional arguments (currently unused)
#'
#' @return a named numeric vector with elements \code{std. dev-a},
#'   \code{std. dev-p}, and \code{corr.}
#'
#' @import nlme
#' @export
#'
#' @examples
#' \dontrun{
#' d <- createDummies(
#'   group.id = "groupId", act.id = "actId", part.id = "partId",
#'   d = sampleDyadData[sampleDyadData$timeId == 1, ],
#'   merge.original = TRUE
#' )
#' o <- nlme::lme(
#'   liking ~ 1,
#'   random = list(groupId = nlme::pdBlocked(list(
#'     nlme::pdIdent(~1),
#'     pdSRM(~ -1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)
#'   ))),
#'   correlation = nlme::corCompSymm(form = ~1 | groupId / pdSRM_dyad_id),
#'   data = d,
#'   na.action = stats::na.omit
#' )
#' }
coef.pdSRM <- function(object, unconstrained = TRUE, ...) {
  if (unconstrained || !nlme::isInitialized(object)) {
    NextMethod()
  } else {
    if (is.null(attr(object, "ncol"))) {
      stop("cannot obtain constrained coefficients with uninitialized dimensions")
    }
    val <- as.vector(object)
    val <- c(val[1], val[2], val[3])
    names(val) <- c("std. dev-a", "std. dev-p", "corr.")
    val
  }
}
