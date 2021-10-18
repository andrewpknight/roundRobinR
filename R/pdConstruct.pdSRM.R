#' Construct pdSRM Object
#'
#' This function is used internally as part of the construction of the pdSRM
#' object that will fit the appropriate structure for the SRM.
#'
#' @param object an object inheriting from pdSRM
#' @param value an optional initialization value
#' @param form an optional one-sided linear formula
#' @param nam an optional vector of character strings
#' @param data an optional data frame in which to evaluate the variables
#' @param ... optional arguments for some methods
#' @import nlme
#' @return a pdSRM object representing a positive-definite matrix
#' @export
#'
#' @examples
#' \dontrun{
#' o = lme(liking ~ 1, random=list(groupId=pdBlocked(list(pdIdent(~1),
#' pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
#' correlation=corCompSymm(form=~1 | groupId/pdSRM_dyad_id),
#' data=d, na.action=na.omit)
#' }
pdConstruct.pdSRM <- function (object, value = numeric(0), form = stats::formula(object),
                               nam = nlme::Names(object), data = sys.frame(sys.parent()), ...)
{
  val <- NextMethod()
  if (length(val) == 0) {
    if ((nc <- length(nlme::Names(val))) > 0) {
     attr(val, "ncol") <- nc
    }
    class(val) <- c("pdSRM", "pdMat")
    return(val)
  }

  if (is.matrix(val)) {

    # Read in a Cholesky factorization of the variance-covariance matrix
    # Then, transform it to the original variance-covariance matrix
    mat.cov <- crossprod(val)

    # Check to see if this is positive-definite

    # Build the original correlation matrix based on the variance-covariance matrix
    aux <- 1/sqrt(diag(mat.cov))
    mat.cor <- aux * t(mat.cov * aux)
    nc <- dim(mat.cov)[2]

    # Extract the variances from the original matrix
    variances <- diag(mat.cov)

    # calculate the actor and partner intercepts as the mean of the constituent parts
    a.var <- mean(variances[1:(nc/2)])
    a.sd <- sqrt(a.var)
    p.var <- mean(variances[(nc/2+1):nc])
    p.sd <- sqrt(p.var)

    # calculate the actor-partner covariance and correlation
    ap.cor <- mean(mat.cor[cbind((1:(nc/2)),(nc/2+1):nc)])
    ap.cov <- mean(mat.cov[cbind((1:(nc/2)),(nc/2+1):nc)])

    # Create a new correlation matrix using these parameters
    new.mat.cor <- diag(nc)
    new.mat.cor[cbind((1:(nc/2)),(nc/2+1):nc)] <- rep(ap.cor,(nc/2))
    new.mat.cor[cbind((nc/2+1):nc,(1:(nc/2)))] <- rep(ap.cor,(nc/2))

    # Create the vector of parameters to send to other functions
    parms <- c(a.sd, p.sd, ap.cor)
    attributes(parms) <- attributes(val)[names(attributes(val)) != "dim"]
    attr(parms, "ncol") <- nc
    class(parms) <- c("pdSRM", "pdMat")
    return(parms)
  }
}
