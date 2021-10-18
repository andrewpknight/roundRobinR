#' Extract model coefficients from pdSRM object
#'
#' This is used internally as part of the construction of the pdSRM
#' object that will fit the appropriate structure for the SRM.
#'
#' @param object an object inheriting from pdSRM
#' @param unconstrained a logical value. If TRUE the coefficients are returned
#' in unconstrained form. If FALSE the upper triangular elements are returned
#' @param ... additional arguments
#' @import nlme
#' @return a vector with the coefficients corresponding to object
#' @export
#'
#' @examples
#' \dontrun{
#' o = lme(liking ~ 1, random=list(groupId=pdBlocked(list(pdIdent(~1),
#' pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
#' correlation=corCompSymm(form=~1 | groupId/pdSRM_dyad_id),
#' data=d, na.action=na.omit)
#' }
coef.pdSRM <- function (object, unconstrained = TRUE, ...)
{
  if (unconstrained || !nlme::isInitialized(object)) NextMethod()
  else {
    if (is.null(Ncol <- attr(object, "ncol"))) {
      stop("cannot obtain constrained coefficients with uninitialized dimensions")
    }
    val <- as.vector(object)
    val <- c(val[1], val[2], val[3])
    names(val) <- c("std. dev-a","std. dev-p", "corr.")
    val
  }
}
