# =============================================================================
# deprecated.R
#
# Backward-compatibility wrappers for functions renamed to snake_case in
# roundRobinR 1.1.0. These functions will continue to work but emit a
# deprecation warning directing users to the new names.
# =============================================================================

#' @title Deprecated: use \code{\link{create_dummies}} instead
#' @description \code{createDummies} has been renamed to \code{create_dummies}.
#'   The argument names have also changed: \code{group.id} -> \code{group_id},
#'   \code{act.id} -> \code{act_id}, \code{part.id} -> \code{part_id},
#'   \code{d} -> \code{data}, \code{include.self} -> \code{include_self},
#'   \code{merge.original} -> \code{merge_original}.
#' @param group.id string; name of the group identifier variable
#' @param act.id string; name of the actor identifier variable
#' @param part.id string; name of the partner identifier variable
#' @param d a \code{data.frame} in directed dyadic long-form
#' @param include.self logical; retain self-ratings? Default \code{FALSE}
#' @param merge.original logical; merge back onto original data? Default
#'   \code{FALSE}
#' @return see \code{\link{create_dummies}}
#' @export
createDummies <- function(group.id, act.id, part.id, d,
                           include.self = FALSE, merge.original = FALSE) {
  .Deprecated(
    new = "create_dummies",
    msg = paste0(
      "'createDummies' is deprecated and will be removed in a future version.\n",
      "Please use 'create_dummies()' instead.\n",
      "Argument names have changed: group.id -> group_id, act.id -> act_id,\n",
      "part.id -> part_id, d -> data, include.self -> include_self,\n",
      "merge.original -> merge_original."
    )
  )
  create_dummies(
    group_id       = group.id,
    act_id         = act.id,
    part_id        = part.id,
    data           = d,
    include_self   = include.self,
    merge_original = merge.original
  )
}


#' @title Deprecated: use \code{\link{srm_run}} instead
#' @description \code{srmRun} has been renamed to \code{srm_run}.
#'   The argument names have also changed: \code{groupId} -> \code{group_id},
#'   \code{actId} -> \code{act_id}, \code{partId} -> \code{part_id},
#'   \code{feVars} -> \code{fe_vars}.
#' @param dv string; name of the outcome variable
#' @param groupId string; name of the group identifier variable
#' @param actId string; name of the actor identifier variable
#' @param partId string; name of the partner identifier variable
#' @param feVars character vector of fixed-effect predictor names, or
#'   \code{NULL}
#' @param data a \code{data.frame} at the directed dyad level
#' @return see \code{\link{srm_run}}
#' @export
srmRun <- function(dv, groupId, actId, partId, feVars = NULL, data) {
  .Deprecated(
    new = "srm_run",
    msg = paste0(
      "'srmRun' is deprecated and will be removed in a future version.\n",
      "Please use 'srm_run()' instead.\n",
      "Argument names have changed: groupId -> group_id, actId -> act_id,\n",
      "partId -> part_id, feVars -> fe_vars."
    )
  )
  srm_run(
    dv       = dv,
    group_id = groupId,
    act_id   = actId,
    part_id  = partId,
    fe_vars  = feVars,
    data     = data
  )
}


#' @title Deprecated: use \code{\link{srm_var_pct}} instead
#' @description \code{srmVarPct} has been renamed to \code{srm_var_pct}.
#' @param object an \code{lme} model object fitted with \code{\link{pdSRM}}
#' @return see \code{\link{srm_var_pct}}
#' @export
srmVarPct <- function(object) {
  .Deprecated(
    new = "srm_var_pct",
    msg = paste0(
      "'srmVarPct' is deprecated and will be removed in a future version.\n",
      "Please use 'srm_var_pct()' instead."
    )
  )
  srm_var_pct(object)
}


#' @title Deprecated: use \code{\link{srm_pseudo_rsq}} instead
#' @description \code{srmPseudoRSq} has been renamed to \code{srm_pseudo_rsq}.
#'   The argument names have also changed: \code{null.model} ->
#'   \code{null_model}, \code{predict.model} -> \code{predict_model}.
#' @param null.model an \code{lme} null model fitted with \code{\link{pdSRM}}
#' @param predict.model an \code{lme} predictor model fitted with
#'   \code{\link{pdSRM}}
#' @return see \code{\link{srm_pseudo_rsq}}
#' @export
srmPseudoRSq <- function(null.model, predict.model) {
  .Deprecated(
    new = "srm_pseudo_rsq",
    msg = paste0(
      "'srmPseudoRSq' is deprecated and will be removed in a future version.\n",
      "Please use 'srm_pseudo_rsq()' instead.\n",
      "Argument names have changed: null.model -> null_model,\n",
      "predict.model -> predict_model."
    )
  )
  srm_pseudo_rsq(
    null_model    = null.model,
    predict_model = predict.model
  )
}


#' @title Deprecated: use \code{\link{cpsrm_run}} instead
#' @description \code{cpsrmRun} has been renamed to \code{cpsrm_run}.
#'   Key argument changes: \code{course.id} -> \code{location_id};
#'   \code{actor.dummies} -> \code{actor_dummies};
#'   \code{partner.dummies} -> \code{partner_dummies};
#'   \code{zero.rho} -> \code{zero_rho}; \code{zero.actor} -> \code{zero_actor};
#'   \code{zero.partner} -> \code{zero_partner};
#'   \code{zero.group} -> \code{zero_group};
#'   \code{zero.course} -> \code{zero_location};
#'   \code{weight.partners} -> \code{weight_partners};
#'   \code{fixed.effects} -> \code{fixed_effects};
#'   \code{stage1.maxit} -> \code{stage1_maxit};
#'   \code{stage2.maxit} -> \code{stage2_maxit};
#'   \code{se.method} -> \code{se_method};
#'   \code{rho.boundary} -> \code{rho_boundary};
#'   \code{verbose.every} -> \code{verbose_every}.
#' @param dv string; name of the outcome variable
#' @param actor.id string; name of the actor identifier variable
#' @param group.id string; name of the group identifier variable
#' @param actor.dummies character vector of actor dummy column names
#' @param partner.dummies character vector of partner dummy column names
#' @param data a \code{data.frame}
#' @param course.id string or \code{NULL}; higher-level clustering variable
#' @param zero.rho logical; fix actor-partner correlation at zero
#' @param zero.actor logical; fix actor variance at zero
#' @param zero.partner logical; fix partner variance at zero
#' @param zero.group logical; fix group variance at zero
#' @param zero.course logical; fix course/location variance at zero
#' @param weight.partners logical; weight partner dummies
#' @param fixed.effects character vector or \code{NULL}
#' @param stage1.maxit integer; stage 1 iterations
#' @param stage2.maxit integer; stage 2 iterations
#' @param se.method character; \code{"hessian"} or \code{"none"}
#' @param rho.boundary numeric; boundary threshold
#' @param verbose.every integer; verbose print frequency
#' @param ... additional arguments passed to \code{\link{cpsrm_run}}
#' @return see \code{\link{cpsrm_run}}
#' @export
cpsrmRun <- function(dv, actor.id, group.id, actor.dummies, partner.dummies,
                     data, course.id = NULL,
                     zero.rho = FALSE, zero.actor = FALSE, zero.partner = FALSE,
                     zero.group = FALSE, zero.course = FALSE,
                     weight.partners = TRUE, fixed.effects = NULL,
                     stage1.maxit = 1000L, stage2.maxit = 1000L,
                     se.method = c("hessian", "none"), rho.boundary = 0.99,
                     verbose.every = 50L, ...) {
  .Deprecated(
    new = "cpsrm_run",
    msg = paste0(
      "'cpsrmRun' is deprecated and will be removed in a future version.\n",
      "Please use 'cpsrm_run()' instead.\n",
      "All arguments have been renamed to snake_case: ",
      "course.id -> location_id, actor.dummies -> actor_dummies, etc."
    )
  )
  cpsrm_run(
    dv              = dv,
    actor_id        = actor.id,
    group_id        = group.id,
    actor_dummies   = actor.dummies,
    partner_dummies = partner.dummies,
    data            = data,
    location_id     = course.id,
    zero_rho        = zero.rho,
    zero_actor      = zero.actor,
    zero_partner    = zero.partner,
    zero_group      = zero.group,
    zero_location   = zero.course,
    weight_partners = weight.partners,
    fixed_effects   = fixed.effects,
    stage1_maxit    = stage1.maxit,
    stage2_maxit    = stage2.maxit,
    se_method       = match.arg(se.method),
    rho_boundary    = rho.boundary,
    verbose_every   = verbose.every,
    ...
  )
}
