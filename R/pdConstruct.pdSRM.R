#' Construct pdSRM Object
#'
#' Internal method called by \code{\link[nlme]{pdConstruct}} to initialize
#' a \code{pdSRM} object. Enforces the SRM constraint of equal actor
#' variances, equal partner variances, and a single actor-partner covariance.
#'
#' @param object an object inheriting from \code{pdSRM}
#' @param value an optional initialization value
#' @param form an optional one-sided linear formula
#' @param nam an optional vector of character strings
#' @param data an optional data frame in which to evaluate the variables
#' @param ... optional arguments passed to other methods
#'
#' @return a \code{pdSRM} object representing the SRM positive-definite matrix
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
pdConstruct.pdSRM <- function(object, value = numeric(0),
                               form = stats::formula(object),
                               nam = nlme::Names(object),
                               data = sys.frame(sys.parent()), ...) {
  val <- NextMethod()
  if (length(val) == 0) {
    if ((nc <- length(nlme::Names(val))) > 0) {
      attr(val, "ncol") <- nc
    }
    class(val) <- c("pdSRM", "pdMat")
    return(val)
  }

  if (is.matrix(val)) {
    mat.cov <- crossprod(val)
    aux <- 1 / sqrt(diag(mat.cov))
    mat.cor <- aux * t(mat.cov * aux)
    nc <- dim(mat.cov)[2]
    variances <- diag(mat.cov)

    a.var <- mean(variances[1:(nc / 2)])
    a.sd <- sqrt(a.var)
    p.var <- mean(variances[(nc / 2 + 1):nc])
    p.sd <- sqrt(p.var)

    ap.cor <- mean(mat.cor[cbind((1:(nc / 2)), (nc / 2 + 1):nc)])

    parms <- c(a.sd, p.sd, ap.cor)
    attributes(parms) <- attributes(val)[names(attributes(val)) != "dim"]
    attr(parms, "ncol") <- nc
    class(parms) <- c("pdSRM", "pdMat")
    return(parms)
  }
}
