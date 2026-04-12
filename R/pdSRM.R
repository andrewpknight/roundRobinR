#' Construct the pdSRM object
#'
#' Creates the positive-definite matrix class used to specify the
#' actor-partner covariance structure of the Social Relations Model within
#' \code{\link[nlme]{lme}}. This class enforces the SRM constraint that all
#' actors share a single variance, all partners share a single variance, and
#' a single actor-partner covariance (generalized reciprocity) is estimated.
#'
#' @param value an optional initialization value, inherited from
#'   \code{\link[nlme]{pdMat}}
#' @param form an optional one-sided linear formula specifying the
#'   row/column names for the matrix
#' @param nam an optional vector of character strings specifying the
#'   row/column names for the matrix
#' @param data inherited from the surrounding \code{nlme} call
#'
#' @return a \code{pdMat} object representing a positive-definite matrix
#'   conforming to the SRM covariance structure
#'
#' @references
#' Knight, A. P., & Humphrey, S. E. (2019). Dyadic data analysis. In
#' S. E. Humphrey & J. M. LeBreton (Eds.), \emph{The Handbook for
#' Multilevel Theory, Measurement, and Analysis} (pp. 423--447).
#' American Psychological Association.
#' \doi{10.1037/0000115-019}
#'
#' Snijders, T. A. B., & Kenny, D. A. (1999). The social relations model
#' for family data: A multilevel approach. \emph{Personal Relationships},
#' \emph{6}, 471--486. \doi{10.1111/j.1475-6811.1999.tb00204.x}
#'
#' @import nlme
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
pdSRM <- function(value = numeric(0), form = NULL, nam = NULL,
                  data = sys.frame(sys.parent())) {
  object <- numeric(0)
  class(object) <- c("pdSRM", "pdMat")
  pdConstruct(object, value, form, nam, data)
}
