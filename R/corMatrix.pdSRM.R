#' Extract Correlation Matrix from a pdSRM Object
#'
#' Internal method called by \code{\link[nlme]{corMatrix}} to reconstruct
#' the correlation matrix from the compressed SRM parameters.
#'
#' @param object an object inheriting from \code{pdSRM}
#' @param ... additional arguments (currently unused)
#'
#' @return the correlation matrix corresponding to the positive-definite
#'   matrix represented by \code{object}, with a \code{"stdDev"} attribute
#'   giving the standard deviations
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
corMatrix.pdSRM <- function(object, ...) {
  if (!nlme::isInitialized(object)) {
    stop("cannot extract the matrix from an uninitialized \"pdSRM\" object")
  }
  if (is.null(Ncol <- attr(object, "ncol"))) {
    stop("cannot extract the matrix with uninitialized dimensions")
  }

  obj <- as.vector(object)
  aux <- c(obj[1], obj[2], obj[3])

  value <- diag(Ncol)
  value[cbind((1:(Ncol / 2)), (Ncol / 2 + 1):Ncol)] <- rep(aux[3], Ncol / 2)
  value[cbind((Ncol / 2 + 1):Ncol, (1:(Ncol / 2)))] <- rep(aux[3], Ncol / 2)

  attr(value, "stdDev") <- c(rep(aux[1], Ncol / 2), rep(aux[2], Ncol / 2))
  attr(value, "corr")   <- aux[3]

  if (length(nm <- nlme::Names(object)) == 0) {
    nm <- paste0("V", seq_len(Ncol))
    dimnames(value) <- list(nm, nm)
  }
  names(attr(value, "stdDev")) <- nm
  value
}
