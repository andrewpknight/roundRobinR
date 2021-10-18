#' Extract Correlation Matrix from a pdSRM Object
#'
#' This function is used internally as part of the construction of the pdSRM
#' object that will fit the appropriate structure for the SRM. The correlation matrix
#' corresponding to the positive-definite matrix represented by object is
#' obtained.
#'
#' @param object an object inheriting from pdSRM
#' @param ... some methods for this require additional arguments
#' @import nlme
#' @return the correlation matrix corresponding to the positive-definite
#' matrix represented by object
#' @export
#'
#' @examples
#' \dontrun{
#' o = lme(liking ~ 1, random=list(groupId=pdBlocked(list(pdIdent(~1),
#' pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
#' correlation=corCompSymm(form=~1 | groupId/pdSRM_dyad_id),
#' data=d, na.action=na.omit)
#' }
corMatrix.pdSRM <- function (object, ...)
{
    if (!nlme::isInitialized(object)) {
        stop("cannot extract the matrix from an uninitialized \"pdSRM\" object")
    }
    if (is.null(Ncol <- attr(object, "ncol"))) {
        stop("cannot extract the matrix with uninitialized dimensions")
    }
    obj <- as.vector(object)
    aux <- c(obj[1], obj[2], obj[3])

    # This builds the correlation matrix
    value <- diag(Ncol)
  value[cbind((1:(Ncol/2)),(Ncol/2+1):Ncol)] <- rep(aux[3],(Ncol/2))
  value[cbind((Ncol/2+1):Ncol,(1:(Ncol/2)))] <- rep(aux[3],(Ncol/2))

  # This builds the vector of SD values
  attr(value, "stdDev") <- c(rep(aux[1], (Ncol/2)), rep(aux[2], (Ncol/2)))
  attr(value, "corr") <- aux[3]
    if (length(nm <- nlme::Names(object)) == 0) {
        nm <- paste("V", 1:Ncol, sep = "")
        dimnames(value) <- list(nm, nm)
    }
    names(attr(value, "stdDev")) <- nm
    value
}
