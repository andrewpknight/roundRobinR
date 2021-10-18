#' Calculate a Pseudo R-Squared for the Social Relations Model
#'
#' Provides an estimated pseudo r-squared value based on the comparison
#' between a null model and a model with fixed effects
#'
#' @param null.model an lme output object fit using pdSRM, without covariates
#' @param predict.model an lme output object fit using pdSRM, with covariates
#' @import nlme
#' @return data.frame with variances from a null and predict model and the
#' estimated pseudo r-squared values
#' @export
#'
#' @examples
#' o = srmPseudoRSq(null.model=nlme::lme(liking ~ 1,
#' random=list(groupId=nlme::pdBlocked(list(nlme::pdIdent(~1),
#' pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
#' correlation=nlme::corCompSymm(form=~1 | groupId/pdSRM_dyad_id),
#' data=
#' createDummies(group.id="groupId", act.id="actId", part.id="partId",
#'              d=sampleDyadData[sampleDyadData$timeId==1, ],
#'                merge.original=TRUE), na.action=na.omit),
#'                predict.model= nlme::lme(liking ~ groupCohesion +
#'                actEx + partEx + contact,
#'                random=list(groupId=nlme::pdBlocked(list(nlme::pdIdent(~1),
#'                pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
#'                correlation=nlme::corCompSymm(form=~1 |
#'                groupId/pdSRM_dyad_id),
#'              createDummies(group.id="groupId",
#'              act.id="actId", part.id="partId",
#'              d=sampleDyadData[sampleDyadData$timeId==1, ],
#'              merge.original=TRUE), na.action=na.omit))
srmPseudoRSq <- function(null.model, predict.model) {

  # Get the variances for null using VarCorr
  variances.null <- as.numeric(VarCorr(null.model)[,1])
  num.mem <- (length(variances.null)-2)/2
  grp.var.null <- variances.null[1]
  act.var.null <- variances.null[2]
  part.var.null <- variances.null[num.mem+2]
  dyd.var.null <- variances.null[length(variances.null)]
  null.vals <- c(grp.var.null, act.var.null, part.var.null, dyd.var.null)

  # Get the variances for predict using VarCorr
  variances.predict <- as.numeric(VarCorr(predict.model)[,1])
  num.mem <- (length(variances.predict)-2)/2
  grp.var.predict <- variances.predict[1]
  act.var.predict <- variances.predict[2]
  part.var.predict <- variances.predict[num.mem+2]
  dyd.var.predict <- variances.predict[length(variances.predict)]
  predict.vals <- c(grp.var.predict, act.var.predict, part.var.predict, dyd.var.predict)

  # put it together
  tab <- data.frame(null.vals, predict.vals)
  colnames(tab) <- c("null", "predict")

  # Calculate pseudo R sq
  tab$pseudoR2 <- (tab$null-tab$predict)/tab$null

  # Return this
  return(tab)
}
