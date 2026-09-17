# =============================================================================
# create_cp_dummies.R  —  Create actor/partner dummy matrices for Co-Partner SRM
# =============================================================================
#
# Supports variable group sizes (2, 3, 4, ..., or mixed within the same dataset).
# Uses data.table for scalability.
#
# Note: this is a separate function from create_dummies(), which creates dummy
# variables for the standard Social Relations Model (SRM). This function is
# purpose-built for the Co-Partner SRM (cpsrm_run()).
# =============================================================================

#' Create Actor and Partner Dummy Matrices for the Co-Partner SRM
#'
#' Generates the actor and partner dummy matrices required by
#' \code{\link{cpsrm_run}}. Unlike \code{\link{create_dummies}}, which
#' produces dummies for the standard Social Relations Model, this function
#' is designed for the Co-Partner SRM where each observation involves one
#' actor and all other members of the group acting simultaneously as
#' partners. Variable group sizes (2, 3, 4, or mixed) are fully supported.
#'
#' @param data a \code{data.frame} in long format, with one row per
#'   person-group observation
#' @param actor_id character; column name of the actor/person identifier
#' @param group_id character; column name of the group identifier
#' @param weight_partners logical; if \code{TRUE} (default), partner dummies
#'   are weighted by \eqn{1/(group\_size - 1)}, making \eqn{\sigma_P^2} the
#'   variance of the average partner contribution. If \code{FALSE}, dummies
#'   are 0/1 and \eqn{\sigma_P^2} is the variance of the sum of partner
#'   contributions. Weighting is recommended when group sizes vary, as it
#'   ensures comparability of \eqn{\sigma_P^2} across group sizes
#' @param prefix_actor character; prefix for actor dummy columns. Default
#'   \code{"A"}
#' @param prefix_partner character; prefix for partner dummy columns. Default
#'   \code{"P"}
#'
#' @return a list with elements:
#' \describe{
#'   \item{\code{actor_mat}}{matrix of actor dummies (n x n_persons)}
#'   \item{\code{partner_mat}}{matrix of partner dummies, optionally weighted
#'     (n x n_persons)}
#'   \item{\code{actor_names}}{character vector of actor dummy column names}
#'   \item{\code{partner_names}}{character vector of partner dummy column names}
#'   \item{\code{group_sizes}}{integer vector of length n giving the group size
#'     for each observation; pass this to \code{cpsrm_run()} via the
#'     \code{group_sizes} argument}
#'   \item{\code{person_levels}}{character vector of person IDs (column order)}
#'   \item{\code{n_persons}}{integer; number of unique persons}
#'   \item{\code{n_groups}}{integer; number of unique groups}
#'   \item{\code{group_size_table}}{table of group size frequencies}
#' }
#'
#' @seealso \code{\link{cpsrm_run}}, \code{\link{create_dummies}}
#'
#' @import data.table
#' @export
#'
#' @examples
#' \donttest{
#' # create_cp_dummies() expects one row per PERSON per GROUP, with every
#' # other group member treated as a simultaneous co-partner (e.g. one row
#' # per player per team, as in three-person golf teams). sampleDyadData is
#' # a round-robin/directed-dyad dataset (multiple rows per actor per
#' # group, one per rated partner) and is NOT the right shape for this
#' # function -- see create_dummies()/srm_run() for that design instead.
#' # This example simulates a small, correctly-shaped dataset: 30
#' # three-person teams drawn from a pool of 90 players.
#' set.seed(1)
#' team_dat <- do.call(rbind, lapply(1:30, function(g) {
#'   data.frame(player = sample(1:90, 3), team = g)
#' }))
#'
#' dummies <- create_cp_dummies(
#'   data        = team_dat,
#'   actor_id    = "player",
#'   group_id    = "team"
#' )
#' cat("Persons:", dummies$n_persons, "\n")
#' cat("Groups:", dummies$n_groups, "\n")
#' }
create_cp_dummies <- function(data,
                               actor_id,
                               group_id,
                               weight_partners = TRUE,
                               prefix_actor    = "A",
                               prefix_partner  = "P") {

  if (!requireNamespace("data.table", quietly = TRUE))
    stop("The 'data.table' package is required. ",
         "Install with install.packages('data.table').")

  stopifnot(is.data.frame(data))
  if (!actor_id %in% names(data))
    stop("Column '", actor_id, "' not found in data.")
  if (!group_id %in% names(data))
    stop("Column '", group_id, "' not found in data.")

  n <- nrow(data)

  # ── 1. Factor levels ────────────────────────────────────────────────────────
  person_levels <- sort(unique(as.character(data[[actor_id]])))
  group_levels  <- sort(unique(as.character(data[[group_id]])))
  n_persons     <- length(person_levels)
  n_groups      <- length(group_levels)

  person_idx <- match(as.character(data[[actor_id]]), person_levels)
  group_idx  <- match(as.character(data[[group_id]]),  group_levels)

  # ── 2. Group sizes ──────────────────────────────────────────────────────────
  group_size_vec <- tabulate(group_idx, nbins = n_groups)
  if (any(group_size_vec < 2))
    stop("Some groups have fewer than 2 members. Each group must have at least ",
         "2 members to estimate partner effects. Problem groups: ",
         paste(group_levels[group_size_vec < 2], collapse = ", "))

  row_group_size   <- group_size_vec[group_idx]
  group_size_table <- table(group_size_vec)
  names(dimnames(group_size_table)) <- "group_size"

  # ── 3. Build actor dummy matrix ─────────────────────────────────────────────
  actor_mat <- matrix(0.0, nrow = n, ncol = n_persons)
  actor_mat[cbind(seq_len(n), person_idx)] <- 1.0
  colnames(actor_mat) <- paste0(prefix_actor, seq_len(n_persons))

  # ── 4. Build partner dummy matrix ───────────────────────────────────────────
  # For each observation, partner dummies are 1 for every OTHER person
  # in the same group.

  # Silence R CMD check NOTEs for data.table variables
  row_id <- i.person_idx <- i.row_id <- partner_col <- NULL

  dt <- data.table::data.table(
    row_id     = seq_len(n),
    person_idx = person_idx,
    group_idx  = group_idx,
    group_size = row_group_size
  )

  dt_self <- dt[dt, on = "group_idx", allow.cartesian = TRUE, nomatch = 0L]
  dt_self <- dt_self[person_idx != i.person_idx]

  partner_counts <- dt_self[, list(count = .N),
                             by = list(row_id, partner_col = i.person_idx)]

  partner_mat <- matrix(0.0, nrow = n, ncol = n_persons)
  partner_mat[cbind(partner_counts$row_id, partner_counts$partner_col)] <-
    partner_counts$count

  # ── 5. Apply weighting ───────────────────────────────────────────────────────
  if (weight_partners) {
    w <- 1.0 / (row_group_size - 1L)
    partner_mat <- sweep(partner_mat, 1L, w, `*`)
  }

  # ── 6. Validation ───────────────────────────────────────────────────────────
  actor_rowsums <- rowSums(actor_mat)
  if (any(abs(actor_rowsums - 1) > 1e-9))
    warning("Some actor dummy rows do not sum to 1. Check actor_id.")

  partner_rowsums <- rowSums(partner_mat)
  if (weight_partners) {
    if (any(abs(partner_rowsums - 1) > 1e-9))
      warning("After weighting, some partner dummy rows do not sum to 1.")
  } else {
    expected_sums <- row_group_size - 1L
    if (any(abs(partner_rowsums - expected_sums) > 1e-9))
      warning("Unweighted partner dummy row sums do not equal group_size - 1.")
  }

  colnames(partner_mat) <- paste0(prefix_partner, seq_len(n_persons))

  # ── 7. Return ────────────────────────────────────────────────────────────────
  list(
    actor_mat        = actor_mat,
    partner_mat      = partner_mat,
    actor_names      = colnames(actor_mat),
    partner_names    = colnames(partner_mat),
    group_sizes      = row_group_size,
    person_levels    = person_levels,
    n_persons        = n_persons,
    n_groups         = n_groups,
    group_size_table = group_size_table
  )
}
