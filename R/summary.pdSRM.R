#' Summarize a pdSRM Object
#'
#' This function is used internally as part of the construction of the pdSRM
#' object that will fit the appropriate structure for the SRM.
#' Attributes structNsame and noCorrelation, with the values of the
#' corresponding arguments to the method function, are appended
#' to object and its class is changed to summary.pdSRM
#'
#' @param object an object inheriting from pdSRM
#' @param structName an optional character string with a description of the pdSRM class
#' @param ... optional arguments for some methods
#' @import nlme
#' @return an object similar to object, with additional attributes structName
#' @export
#'
#' @examples
#' \dontrun{
#' o = lme(liking ~ 1, random=list(groupId=pdBlocked(list(pdIdent(~1),
#' pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
#' correlation=corCompSymm(form=~1 | groupId/pdSRM_dyad_id),
#' data=d, na.action=na.omit)
#' }
summary.pdSRM <- function (object, structName = "Social Relations Model", ...)
{
    if (nlme::isInitialized(object)) {
      # Build the correlation matrix
        value <- corMatrix(object)
        attr(value, "structName") <- structName
        attr(value, "noCorrelation") <- FALSE
        attr(value, "formula") <- stats::formula(object)
        class(value) <- "summary.pdMat"
        value
    }
    else {
        object
    }
}
