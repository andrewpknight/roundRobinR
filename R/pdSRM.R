#' Construct the pdSRM object
#'
#' This function is used to create the correct structure for the variance/covariance
#' matrix for a full social relations model.
#' @param value an optional initialization value
#' @param form an optional one-sided linear formula specifying the row/column names for the matrix
#' @param nam an optional vector of character strings specifying the row/column names for the matrix
#' @param data inherited from the surrounding nlme call
#' @import nlme
#' @return a pdMat object representing a positive-definite matrix conforming to the SRM structure
#' @export
#'
#' @examples
#' o = nlme::lme(liking ~ 1, random=list(groupId=nlme::pdBlocked(list(nlme::pdIdent(~1),
#' pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
#' correlation=nlme::corCompSymm(form=~1 | groupId/pdSRM_dyad_id),
#' data=
#' createDummies(group.id="groupId", act.id="actId", part.id="partId",
#'              d=sampleDyadData[sampleDyadData$timeId==1, ],
#'                merge.original=TRUE), na.action=na.omit)
pdSRM <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent()))
{
  object <- numeric(0)
  class(object) <- c("pdSRM", "pdMat")
  pdConstruct(object, value, form, nam, data)
}
