#' Summarize a pdSRM Object
#'
#' Internal method that produces a \code{summary.pdMat} representation of
#' a \code{pdSRM} object, used by \code{\link[nlme]{lme}} when printing
#' model output.
#'
#' @param object an object inheriting from \code{pdSRM}
#' @param structName a character string describing the covariance structure;
#'   defaults to \code{"Social Relations Model"}
#' @param ... optional arguments passed to other methods
#'
#' @return an object of class \code{summary.pdMat} with additional attributes
#'   \code{structName} and \code{noCorrelation}
#'
#' @import nlme
#' @export
#'
#' @examples
#' \donttest{
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
summary.pdSRM <- function(object, structName = "Social Relations Model", ...) {
  if (nlme::isInitialized(object)) {
    value <- corMatrix(object)
    attr(value, "structName")   <- structName
    attr(value, "noCorrelation") <- FALSE
    attr(value, "formula")       <- stats::formula(object)
    class(value) <- "summary.pdMat"
    value
  } else {
    object
  }
}
