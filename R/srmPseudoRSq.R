#' Calculate Pseudo R-Squared Values for the Social Relations Model
#'
#' Computes pseudo R-squared values for each SRM variance component by
#' comparing a null model (intercept only) with a predictor model (with
#' fixed effects). The pseudo R-squared for each component is
#' \code{(null - predicted) / null}, reflecting the proportion of each
#' variance component explained by the fixed effects.
#'
#' @param null.model an \code{lme} object fitted with \code{\link{pdSRM}}
#'   and no fixed-effect predictors (intercept only)
#' @param predict.model an \code{lme} object fitted with \code{\link{pdSRM}}
#'   and one or more fixed-effect predictors; must use the same dataset and
#'   random effects structure as \code{null.model}
#'
#' @return a \code{data.frame} with three columns and four rows (Group,
#'   Actor, Partner, Dyad):
#'   \describe{
#'     \item{\code{null}}{variance component from the null model}
#'     \item{\code{predict}}{variance component from the predictor model}
#'     \item{\code{pseudoR2}}{pseudo R-squared: \code{(null - predict) / null}}
#'   }
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
#' null_mod <- nlme::lme(
#'   liking ~ 1,
#'   random = list(groupId = nlme::pdBlocked(list(
#'     nlme::pdIdent(~1),
#'     pdSRM(~ -1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)
#'   ))),
#'   correlation = nlme::corCompSymm(form = ~1 | groupId / pdSRM_dyad_id),
#'   data = d,
#'   na.action = stats::na.omit
#' )
#' pred_mod <- nlme::lme(
#'   liking ~ actEx + partEx + contact,
#'   random = list(groupId = nlme::pdBlocked(list(
#'     nlme::pdIdent(~1),
#'     pdSRM(~ -1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)
#'   ))),
#'   correlation = nlme::corCompSymm(form = ~1 | groupId / pdSRM_dyad_id),
#'   data = d,
#'   na.action = stats::na.omit
#' )
#' srmPseudoRSq(null.model = null_mod, predict.model = pred_mod)
#' }
srmPseudoRSq <- function(null.model, predict.model) {

  extract_vcs <- function(mod) {
    variances <- as.numeric(nlme::VarCorr(mod)[, 1])
    num.mem   <- (length(variances) - 2) / 2
    c(
      variances[1],
      variances[2],
      variances[num.mem + 2],
      variances[length(variances)]
    )
  }

  null.vals    <- extract_vcs(null.model)
  predict.vals <- extract_vcs(predict.model)

  tab <- data.frame(
    null    = null.vals,
    predict = predict.vals,
    row.names = c("Group", "Actor", "Partner", "Dyad"),
    stringsAsFactors = FALSE
  )
  tab$pseudoR2 <- (tab$null - tab$predict) / tab$null
  return(tab)
}
