#' Extract Variance Decomposition Results from pdSRM Output
#'
#' Returns the breakdown of variance components and reciprocity correlations
#' from the output of a multilevel model fitting the SRM according to the
#' dummy variable specification.
#'
#' @param object an lme output object with the SRM structure
#' @import nlme
#' @return data.frame with the variance-covariance parameters, percentages, and
#' the reciprocity correlations
#' @export
#'
#' @examples
#' o = srmVarPct(nlme::lme(liking ~ 1, random=list(groupId=nlme::pdBlocked(list(nlme::pdIdent(~1),
#' pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
#' correlation=nlme::corCompSymm(form=~1 | groupId/pdSRM_dyad_id),
#' data=
#' createDummies(group.id="groupId", act.id="actId", part.id="partId",
#' d=sampleDyadData[sampleDyadData$timeId==1, ],
#' merge.original=TRUE), na.action=na.omit))
srmVarPct <- function(object) {

  # Get the variances using VarCorr
  variances <- as.numeric(nlme::VarCorr(object)[,1])
  num.mem <- (length(variances)-2)/2
  grp.var <- variances[1]
  act.var <- variances[2]
  part.var <- variances[num.mem+2]
  dyd.var <- variances[length(variances)]

  # Get the correlations directly from the summary object
  # Haven't written the method for SRM to pull these directly using VarCorr
  # Hack!!
  o.sum <- summary(object)

  #	ap.cor <- attr(o.sum$apVar, "Pars")[4]
  o <- as.matrix(o.sum$modelStruct$reStruct[[1]])
  ap.cor <- o[(num.mem+2), 2]/sqrt(o[2,2]*o[(num.mem+2),(num.mem+2)])
  ap.cov <- ap.cor*sqrt(act.var*part.var)
  dyd.cor <- stats::coef(object$modelStruct$corStruct,unconstrained=FALSE)

  #	dyd.cor <- as.vector(o.sum$modelStruct$corStruct[[1]])/2
  dyd.cov <- dyd.cor*dyd.var
  variance.parms <- as.numeric(c(grp.var, act.var, part.var, dyd.var, ap.cov, dyd.cov))
  names(variance.parms) <- c("Group", "Actor", "Partner", "Dyad", "Generalized Reciprocity", "Dyadic Reciprocity")

  # Compute the percentages and return the correlations
  total.var <- grp.var + act.var + part.var + dyd.var
  act.pct <- 100*act.var/total.var
  part.pct <- 100*part.var/total.var
  grp.pct <- 100*grp.var/total.var
  dyd.pct <- 100*dyd.var/total.var
  variance.pcts <- c(grp.pct, act.pct, part.pct, dyd.pct, ap.cor, dyd.cor)
  names(variance.pcts) <- c("Group", "Actor", "Partner", "Dyad", "Generalized Reciprocity", "Dyadic Reciprocity")
  output <- round(as.data.frame(list(variances.and.covariances=variance.parms, percents.and.correlations=variance.pcts)), 3)
  return(output)
}
