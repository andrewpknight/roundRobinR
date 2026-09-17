# =============================================================================
# cpsrm.R  —  Friendly top-level entry point for the Co-Partner SRM
# =============================================================================
#
# Thin convenience wrapper: takes raw long-format data plus a few column
# names, builds the actor/partner dummy matrices internally via
# create_cp_dummies(), then fits the model via cpsrm_run(). Everything
# cpsrm_run() can do is still reachable — either call cpsrm_run() directly
# with hand-built dummies, or pass extra arguments through cpsrm()'s `...`.
# =============================================================================

#' Fit the Co-Partner Social Relations Model from Raw Long-Format Data
#'
#' \code{cpsrm()} is a friendly, tidyverse-style entry point for the
#' Co-Partner SRM. Give it raw long-format data and the names of the
#' outcome, actor, and group columns; it builds the actor/partner dummy
#' matrices with \code{\link{create_cp_dummies}} and fits the model with
#' \code{\link{cpsrm_run}}. Use \code{\link{cpsrm_run}} directly when you
#' need to supply your own dummy matrices (e.g. dummies built once and
#' reused across several model calls).
#'
#' @param data a \code{data.frame} in long format, one row per
#'   person-group observation
#' @param dv character; column name of the outcome variable
#' @param actor_id character; column name of the actor/person identifier
#' @param group_id character; column name of the group identifier
#' @param location_id character or \code{NULL}; column name of a
#'   higher-level clustering variable (e.g. course, site), if any
#' @param weight_partners logical; passed to both
#'   \code{\link{create_cp_dummies}} (how the dummies are built) and
#'   \code{\link{cpsrm_run}} (how \eqn{\sigma_P^2} is interpreted) — see
#'   \code{\link{create_cp_dummies}} for details. Default \code{TRUE}
#' @param ... additional arguments passed through to
#'   \code{\link{cpsrm_run}} (e.g. \code{zero_rho}, \code{fixed_effects},
#'   \code{method}, \code{optimizer}, \code{se_method}, \code{start},
#'   \code{verbose})
#'
#' @return an object of class \code{"cpsrm"}; see \code{\link{cpsrm_run}}
#'   for the full return value. The dummy-construction output from
#'   \code{\link{create_cp_dummies}} is attached as \code{$dummies} for
#'   reference (e.g. to inspect \code{$dummies$group_size_table}).
#'
#' @seealso \code{\link{cpsrm_run}} for the full-control interface,
#'   \code{\link{create_cp_dummies}} for the dummy-matrix construction this
#'   wrapper calls internally
#'
#' @export
#'
#' @examples
#' \donttest{
#' fit <- cpsrm(
#'   data     = sampleDyadData[sampleDyadData$timeId == 1, ],
#'   dv       = "score",
#'   actor_id = "actId",
#'   group_id = "groupId"
#' )
#' print(fit)
#' }
cpsrm <- function(data,
                   dv,
                   actor_id,
                   group_id,
                   location_id     = NULL,
                   weight_partners = TRUE,
                   ...) {

  stopifnot(is.data.frame(data))
  if (!dv %in% names(data))
    stop("Column '", dv, "' (dv) not found in data.")
  if (!is.null(location_id) && !location_id %in% names(data))
    stop("Column '", location_id, "' (location_id) not found in data.")

  dummies <- create_cp_dummies(
    data            = data,
    actor_id        = actor_id,
    group_id        = group_id,
    weight_partners = weight_partners
  )

  # cpsrm_run() expects actor_dummies/partner_dummies as column NAMES
  # already present in 'data' (it does data[, actor_dummies], etc.), so
  # bind the matrices create_cp_dummies() built onto a working copy of
  # data before calling it. Guard against name collisions with existing
  # columns (e.g. if the data already has columns literally named "A1").
  clash <- intersect(c(dummies$actor_names, dummies$partner_names), names(data))
  if (length(clash) > 0)
    stop("create_cp_dummies() dummy column names collide with existing ",
         "columns in 'data': ", paste(clash, collapse = ", "),
         ". Rename those columns, or call create_cp_dummies()/cpsrm_run() ",
         "directly with custom prefix_actor/prefix_partner.")

  data_aug <- cbind(data, dummies$actor_mat, dummies$partner_mat)

  fit <- cpsrm_run(
    dv              = dv,
    actor_id        = actor_id,
    group_id        = group_id,
    data            = data_aug,
    actor_dummies   = dummies$actor_names,
    partner_dummies = dummies$partner_names,
    group_sizes     = dummies$group_sizes,
    location_id     = location_id,
    weight_partners = weight_partners,
    ...
  )

  fit$dummies <- dummies
  fit
}
