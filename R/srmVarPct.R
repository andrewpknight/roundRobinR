#' Extract Variance Decomposition from a Fitted SRM
#'
#' Extracts and formats the variance components and reciprocity correlations
#' from an \code{\link[nlme]{lme}} object fitted with the
#' \code{\link{pdSRM}} covariance structure. Returns group, actor, partner,
#' and dyadic (relationship) variances as both raw values and percentages of
#' total variance, along with generalized reciprocity (actor-partner
#' correlation) and dyadic reciprocity.
#'
#' @param object an \code{lme} model object fitted with \code{\link{pdSRM}}
#'
#' @return a \code{data.frame} with two columns and six rows:
#'   \describe{
#'     \item{\code{variances.and.covariances}}{Group, Actor, Partner, and
#'       Dyad variances; Generalized Reciprocity covariance; Dyadic
#'       Reciprocity covariance}
#'     \item{\code{percents.and.correlations}}{variance percentages for the
#'       four components; Generalized Reciprocity correlation; Dyadic
#'       Reciprocity correlation}
#'   }
#'
#' @references
#' Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006).
#' \emph{Dyadic Data Analysis}. Guilford Press.
#'
#' @import nlme
#' @importFrom stats coef
#' @export
#'
#' @examples
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
#' srmVarPct(o)
srmVarPct <- function(object) {

  variances <- as.numeric(nlme::VarCorr(object)[, 1])
  num.mem   <- (length(variances) - 2) / 2
  grp.var   <- variances[1]
  act.var   <- variances[2]
  part.var  <- variances[num.mem + 2]
  dyd.var   <- variances[length(variances)]

  o.sum <- summary(object)
  o     <- as.matrix(o.sum$modelStruct$reStruct[[1]])
  ap.cor <- o[(num.mem + 2), 2] / sqrt(o[2, 2] * o[(num.mem + 2), (num.mem + 2)])
  ap.cov <- ap.cor * sqrt(act.var * part.var)
  dyd.cor <- stats::coef(object$modelStruct$corStruct, unconstrained = FALSE)
  dyd.cov <- dyd.cor * dyd.var

  variance.parms <- as.numeric(
    c(grp.var, act.var, part.var, dyd.var, ap.cov, dyd.cov)
  )
  names(variance.parms) <- c(
    "Group", "Actor", "Partner", "Dyad",
    "Generalized Reciprocity", "Dyadic Reciprocity"
  )

  total.var     <- grp.var + act.var + part.var + dyd.var
  variance.pcts <- c(
    100 * grp.var  / total.var,
    100 * act.var  / total.var,
    100 * part.var / total.var,
    100 * dyd.var  / total.var,
    ap.cor,
    dyd.cor
  )
  names(variance.pcts) <- names(variance.parms)

  output <- round(
    as.data.frame(list(
      variances.and.covariances  = variance.parms,
      percents.and.correlations  = variance.pcts
    )),
    3
  )
  return(output)
}
