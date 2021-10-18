#' Run the Social Relations Model using Multilevel Modeling
#'
#' This is a wrapper function to efficiently run the social relations model
#' with a basic directed dyad-level dataset. The function will create the
#' necessary dummy variables and build out the correct formula for lme.
#'
#' @param dv string giving the name of the directed dyadic criterion variable
#' @param groupId string giving the name of the group identifier variable
#' @param actId string giving the name of the actor identifier variable
#' @param partId string giving the name of the partner identifier variable
#' @param feVars vector containing the names of fixed effect variables
#' @param data data.frame at the directed dyad level
#' @import nlme
#'
#' @return a list with two items:
#' \itemize{
#' \item lme.output is the full lme output object
#' \item srm.output is the variance decomposition with reciprocity correlations
#' }
#' @export
#'
#' @examples
#' o = srmRun(dv="liking", groupId="groupId", actId="actId", partId="partId",
#' feVars=c("actEx", "partEx", "contact"),
#' data=sampleDyadData[sampleDyadData$timeId==1, ])
srmRun = function(dv, groupId, actId, partId, feVars=NULL, data) {

  # first, create the dummies and get the maximum group size
  d = createDummies(group.id=groupId, act.id=actId, part.id=partId, d=data, include.self=FALSE, merge.original=TRUE)
  maxGroupSize = max(d$pdSRM_act_num)
  d$pdSRM_group_id = d$groupId

  # Now create the formula
  if(!is.null(feVars)) {

    fixClause=stats::formula(paste(dv, "~", paste0(feVars,collapse="+"), sep=""))

  } else {

    fixClause=stats::formula(paste(dv,"~1", sep=""))

  }

  srmClause=stats::formula(paste("~-1 + ",paste0(paste("a", 1:maxGroupSize, sep=""),collapse="+"), "+", paste0(paste("p", 1:maxGroupSize, sep=""),collapse="+"), sep=""))

  o = lme(fixed=fixClause, random=list(pdSRM_group_id=nlme::pdBlocked(list(nlme::pdIdent(~1), pdSRM(srmClause)))), correlation=nlme::corCompSymm(form=~1 | pdSRM_group_id/pdSRM_dyad_id), data=d, na.action=stats::na.omit)

  o.pct = srmVarPct(o)
  outputList = list("lme.output"=o, "srm.output"=o.pct)
  return(outputList)
}
