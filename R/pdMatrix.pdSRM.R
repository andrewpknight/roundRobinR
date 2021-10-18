#' Extract Matrix or Square-Root Factor from a pdSRM object
#'
#' This function is used internally as part of the construction of the pdSRM
#' object that will fit the appropriate structure for the SRM.
#'
#' @param object an object inheriting from pdSRM
#' @param factor an optional logical value
#' @import nlme
#' @return if factor is FALSE the positive-definite matrix represented by object;
#' else a square-root of the positive-definite matrix is returned
#' @export
#'
#' @examples
#' \dontrun{
#' o = lme(liking ~ 1, random=list(groupId=pdBlocked(list(pdIdent(~1),
#' pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
#' correlation=corCompSymm(form=~1 | groupId/pdSRM_dyad_id),
#' data=d, na.action=na.omit)
#' }
pdMatrix.pdSRM <- function (object, factor = FALSE)
{
    if (!nlme::isInitialized(object)) {
        stop("cannot extract the matrix from an uninitialized \"pdSRM\" object")
    }
    if (is.null(Ncol <- attr(object, "ncol"))) {
        stop("cannot extract the matrix with uninitialized dimensions")
    }

  parms <- as.vector(object)

  # Recreate all the components
  a.sd <- parms[1]
  a.var <- a.sd^2
  p.sd <- parms[2]
  p.var <- p.sd^2
  ap.cor <- parms[3]
  ap.cov <- ap.cor*a.sd*p.sd

  # Create the variance/covariance matrix
  mat.cov <- diag(c(rep(a.var, (Ncol/2)), rep(p.var, (Ncol/2))))
  mat.cov[cbind((1:(Ncol/2)),(Ncol/2+1):Ncol)] <- rep(ap.cov,(Ncol/2))
  mat.cov[cbind((Ncol/2+1):Ncol,(1:(Ncol/2)))] <- rep(ap.cov,(Ncol/2))

  # Create a correlation matrix
  aux <- 1/sqrt(diag(mat.cov))
  mat.cor <- aux * t(mat.cov * aux)

  if(factor) {
    # Test for positive definite here
    cholStatus <- try(u <- chol(mat.cov), silent = TRUE)
    cholError <- ifelse(class(cholStatus)[1] == "try-error", TRUE, FALSE)
    if(cholError) {
      cat("matrix is not positive definite: executing work around...you should really check your results my friend!\n")
      value <- upper.tri(mat.cov, diag=TRUE)
    } else {
      value <- chol(mat.cov)
    }

    ld <- determinant(mat.cov, logarithm=TRUE)[1]
    attr(value, "logDet") <- ld$modulus
  } else {
    value <- mat.cov
  }
  dimnames(value) <- attr(object, "Dimnames")
  value
}
