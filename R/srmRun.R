#' Run the Social Relations Model Using Multilevel Modeling
#'
#' A wrapper function that fits the Social Relations Model (SRM) on a
#' directed dyadic dataset using restricted maximum likelihood via
#' \code{\link[nlme]{lme}}. The function creates the necessary actor and
#' partner dummy variables, constructs the SRM covariance structure using
#' \code{\link{pdSRM}}, and returns both the raw \code{lme} output and a
#' formatted variance decomposition table.
#'
#' @param dv string; name of the directed dyadic criterion (outcome) variable
#' @param groupId string; name of the group identifier variable
#' @param actId string; name of the actor identifier variable
#' @param partId string; name of the partner identifier variable
#' @param feVars character vector of fixed-effect predictor variable names,
#'   or \code{NULL} (default) for an intercept-only null model
#' @param data a \code{data.frame} at the directed dyad level
#'
#' @return a named list with two elements:
#' \describe{
#'   \item{\code{lme.output}}{the full \code{lme} model object}
#'   \item{\code{srm.output}}{a \code{data.frame} from \code{\link{srmVarPct}}
#'     giving variances, percentages, and reciprocity correlations}
#' }
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
#' o <- srmRun(
#'   dv      = "liking",
#'   groupId = "groupId",
#'   actId   = "actId",
#'   partId  = "partId",
#'   feVars  = c("actEx", "partEx", "contact"),
#'   data    = sampleDyadData[sampleDyadData$timeId == 1, ]
#' )
#' o$srm.output
srmRun <- function(dv, groupId, actId, partId, feVars = NULL, data) {

  d <- createDummies(
    group.id       = groupId,
    act.id         = actId,
    part.id        = partId,
    d              = data,
    include.self   = FALSE,
    merge.original = TRUE
  )

  maxGroupSize <- max(d$pdSRM_act_num)

  # Use a temporary column name to avoid the groupId-string-vs-column bug
  d$pdSRM_group_id <- d[[groupId]]

  if (!is.null(feVars)) {
    fixClause <- stats::formula(
      paste(dv, "~", paste(feVars, collapse = "+"))
    )
  } else {
    fixClause <- stats::formula(paste(dv, "~ 1"))
  }

  srmClause <- stats::formula(
    paste(
      "~-1 +",
      paste(paste0("a", seq_len(maxGroupSize)), collapse = "+"),
      "+",
      paste(paste0("p", seq_len(maxGroupSize)), collapse = "+")
    )
  )

  o <- lme(
    fixed       = fixClause,
    random      = list(
      pdSRM_group_id = nlme::pdBlocked(list(
        nlme::pdIdent(~1),
        pdSRM(srmClause)
      ))
    ),
    correlation = nlme::corCompSymm(form = ~1 | pdSRM_group_id / pdSRM_dyad_id),
    data        = d,
    na.action   = stats::na.omit
  )

  o.pct      <- srmVarPct(o)
  outputList <- list("lme.output" = o, "srm.output" = o.pct)
  return(outputList)
}
