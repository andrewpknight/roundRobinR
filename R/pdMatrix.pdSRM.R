#' Extract Matrix or Square-Root Factor from a pdSRM Object
#'
#' Internal method called by \code{\link[nlme]{pdMatrix}} to reconstruct the
#' full covariance matrix from the three stored SRM parameters (actor SD,
#' partner SD, actor-partner correlation).
#'
#' @param object an object inheriting from \code{pdSRM}
#' @param factor logical; if \code{TRUE} the upper Cholesky factor is
#'   returned, otherwise the full positive-definite matrix
#'
#' @return if \code{factor} is \code{FALSE}, the positive-definite matrix
#'   represented by \code{object}; if \code{TRUE}, an upper triangular
#'   Cholesky factor with a \code{logDet} attribute
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
pdMatrix.pdSRM <- function(object, factor = FALSE) {
  if (!nlme::isInitialized(object)) {
    stop("cannot extract the matrix from an uninitialized \"pdSRM\" object")
  }
  if (is.null(Ncol <- attr(object, "ncol"))) {
    stop("cannot extract the matrix with uninitialized dimensions")
  }

  parms <- as.vector(object)
  a.sd  <- parms[1]
  a.var <- a.sd^2
  p.sd  <- parms[2]
  p.var <- p.sd^2
  ap.cor <- parms[3]
  ap.cov <- ap.cor * a.sd * p.sd

  mat.cov <- diag(c(rep(a.var, (Ncol / 2)), rep(p.var, (Ncol / 2))))
  mat.cov[cbind((1:(Ncol / 2)), (Ncol / 2 + 1):Ncol)] <- rep(ap.cov, (Ncol / 2))
  mat.cov[cbind((Ncol / 2 + 1):Ncol, (1:(Ncol / 2)))] <- rep(ap.cov, (Ncol / 2))

  if (factor) {
    # Use inherits() rather than class() == "..." per CRAN policy
    chol_result <- tryCatch(chol(mat.cov), error = function(e) e)
    if (inherits(chol_result, "error")) {
      message("matrix is not positive definite: using upper triangular workaround")
      value <- upper.tri(mat.cov, diag = TRUE)
    } else {
      value <- chol_result
    }
    ld <- determinant(mat.cov, logarithm = TRUE)
    attr(value, "logDet") <- ld$modulus
  } else {
    value <- mat.cov
  }

  dimnames(value) <- attr(object, "Dimnames")
  value
}
