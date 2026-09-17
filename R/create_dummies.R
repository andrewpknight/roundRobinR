#' Create Dummy Variables for the Social Relations Model
#'
#' Generates the actor and partner dummy variables required to fit the
#' Social Relations Model (SRM) using multilevel modeling, following the
#' approach of Snijders and Kenny (1999). The function produces \emph{N}
#' actor dummies and \emph{N} partner dummies, where \emph{N} is the
#' maximum group size in the dataset.
#'
#' @param group_id string; name of the group identifier variable
#' @param act_id string; name of the actor identifier variable
#' @param part_id string; name of the partner identifier variable
#' @param data a \code{data.frame} structured in directed dyadic long-form,
#'   with one row per ordered (actor, partner) pair
#' @param include_self logical; if \code{TRUE} self-ratings (actor ==
#'   partner) are retained. Default is \code{FALSE}
#' @param merge_original logical; if \code{TRUE} the generated identifiers
#'   and dummy variables are merged back onto the original dataset and
#'   returned together. Default is \code{FALSE}
#'
#' @return a \code{data.frame} containing:
#'   \describe{
#'     \item{\code{pdSRM_act_id}}{integer internal actor identifier}
#'     \item{\code{pdSRM_part_id}}{integer internal partner identifier}
#'     \item{\code{pdSRM_act_num}}{integer actor slot number within group}
#'     \item{\code{pdSRM_part_num}}{integer partner slot number within group}
#'     \item{\code{pdSRM_dyad_id}}{integer undirected dyad identifier}
#'     \item{\code{a1}, \code{a2}, \ldots}{actor dummy variables}
#'     \item{\code{p1}, \code{p2}, \ldots}{partner dummy variables}
#'   }
#'   If \code{merge_original = TRUE}, all original variables are appended.
#'
#' @references
#' Snijders, T. A. B., & Kenny, D. A. (1999). The social relations model
#' for family data: A multilevel approach. \emph{Personal Relationships},
#' \emph{6}, 471--486. \doi{10.1111/j.1475-6811.1999.tb00204.x}
#'
#' @import data.table
#' @export
#'
#' @examples
#' d_out <- create_dummies(
#'   group_id = "groupId",
#'   act_id   = "actId",
#'   part_id  = "partId",
#'   data     = sampleDyadData
#' )
#' head(d_out)
create_dummies <- function(group_id, act_id, part_id, data,
                            include_self   = FALSE,
                            merge_original = FALSE) {

  # Silence R CMD check NOTEs for data.table variables
  .N <- orig_group_id <- NULL

  d <- data[with(data, order(data[, group_id], data[, act_id], data[, part_id])), ]

  d.sub <- d[, c(group_id, act_id, part_id)]

  d.sub$act_indiv_id  <- paste(d.sub[, group_id], d.sub[, act_id],  sep = "_-")
  d.sub$part_indiv_id <- paste(d.sub[, group_id], d.sub[, part_id], sep = "_-")

  acts   <- unique(d.sub$act_indiv_id)
  parts  <- unique(d.sub$part_indiv_id)
  all.indivs <- data.frame(
    string_indiv_id = unique(c(acts, parts)),
    stringsAsFactors = FALSE
  )
  all.indivs$unique_indiv_id <- seq_len(nrow(all.indivs))

  split_ids <- t(matrix(
    unlist(strsplit(all.indivs$string_indiv_id, split = "_-")),
    nrow = 2
  ))
  all.indivs$orig_group_id <- as.numeric(split_ids[, 1])
  all.indivs$orig_indiv_id <- as.numeric(split_ids[, 2])

  d.dt <- data.table::data.table(all.indivs)
  agg  <- as.data.frame(d.dt[, list(group_size = .N), by = list(orig_group_id)])
  max_group_size <- max(agg[, 2])

  grps  <- unique(all.indivs$orig_group_id)
  count <- 1L
  res   <- NULL

  for (grp in grps) {
    members  <- unique(all.indivs[all.indivs$orig_group_id == grp, "unique_indiv_id"])
    act_num  <- 1L
    for (act in members) {
      part_num <- 1L
      for (part in members) {
        res.line <- c(grp, act, act_num, part, part_num)
        if (is.null(res)) {
          res <- res.line
        } else {
          res <- rbind(res, res.line)
        }
        part_num <- part_num + 1L
        count    <- count + 1L
      }
      act_num <- act_num + 1L
    }
  }

  res <- data.frame(res, stringsAsFactors = FALSE)
  colnames(res) <- c("orig_group_id", "unique_act_id", "act_num",
                     "unique_part_id", "part_num")

  dummy_cols <- c(paste0("a", seq_len(max_group_size)),
                  paste0("p", seq_len(max_group_size)))
  res[, dummy_cols] <- NA_integer_

  for (i in seq_len(max_group_size)) {
    res[, paste0("a", i)] <- as.integer(res$act_num  == i)
    res[, paste0("p", i)] <- as.integer(res$part_num == i)
  }

  if (!include_self) {
    res <- res[res$unique_act_id != res$unique_part_id, ]
  }

  res$dyad_id <- NA_integer_
  count <- 1L
  for (grp in grps) {
    actors   <- unique(res[res$orig_group_id == grp, "unique_act_id"])
    partners <- unique(res[res$orig_group_id == grp, "unique_part_id"])
    indivs   <- sort(unique(c(actors, partners)))
    for (a in seq_along(indivs)) {
      for (p in seq_along(indivs)) {
        if (a > p) {
          res$dyad_id <- ifelse(
            (res$unique_act_id == indivs[a] & res$unique_part_id == indivs[p]) |
              (res$unique_act_id == indivs[p] & res$unique_part_id == indivs[a]),
            count, res$dyad_id
          )
          count <- count + 1L
        }
      }
    }
  }

  res1 <- merge(res,
                all.indivs[, c("orig_group_id", "unique_indiv_id", "orig_indiv_id")],
                by.x = c("orig_group_id", "unique_act_id"),
                by.y = c("orig_group_id", "unique_indiv_id"),
                all.x = TRUE)
  colnames(res1)[ncol(res1)] <- "orig_act_id"

  res2 <- merge(res1,
                all.indivs[, c("orig_group_id", "unique_indiv_id", "orig_indiv_id")],
                by.x = c("orig_group_id", "unique_part_id"),
                by.y = c("orig_group_id", "unique_indiv_id"),
                all.x = TRUE)
  colnames(res2)[ncol(res2)] <- "orig_part_id"

  colnames(res2) <- c(
    group_id,
    "pdSRM_part_id", "pdSRM_act_id", "pdSRM_act_num", "pdSRM_part_num",
    paste0("a", seq_len(max_group_size)),
    paste0("p", seq_len(max_group_size)),
    "pdSRM_dyad_id", act_id, part_id
  )

  col_order <- c(
    group_id, act_id, part_id,
    "pdSRM_act_id", "pdSRM_part_id", "pdSRM_dyad_id",
    "pdSRM_act_num", "pdSRM_part_num",
    paste0("a", seq_len(max_group_size)),
    paste0("p", seq_len(max_group_size))
  )

  res3 <- res2[
    with(res2, order(res2[, group_id], res2[, "pdSRM_act_id"], res2[, "pdSRM_part_id"])),
    col_order
  ]

  # Merge with the original dataset, first removing any pre-existing
  # dummy columns to avoid .x/.y name conflicts
  if (merge_original) {
    dummy_pattern <- paste0("^(",
      paste(c(paste0("a", seq_len(max_group_size)),
              paste0("p", seq_len(max_group_size))), collapse = "|"),
      ")$")
    d_clean <- d[, !grepl(dummy_pattern, names(d)), drop = FALSE]
    res4 <- merge(res3, d_clean, by = c(group_id, act_id, part_id), all.x = TRUE)
    return(res4)
  } else {
    return(res3)
  }
}
