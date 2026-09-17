# =============================================================================
# cpsrm_run.R  —  Co-Partner Social Relations Model  (v13, snake_case)
# =============================================================================
#
# Model:
#   Y_{i(jk...)gt} = mu + A_i + sum_j(w_j * P_j) + G_g + [L_l] + e
#
# Woodbury formulation (block method):
#   V = se^2 * I_n + Z * D * Z'   where Z = [Za | Zp | Zg | (Zl)]
# =============================================================================

#' Run the Co-Partner Social Relations Model
#'
#' Fits the Co-Partner Social Relations Model (CP-SRM) using restricted
#' maximum likelihood (REML). The \code{"block"} method uses the Woodbury
#' matrix identity for fast computation on large datasets; the \code{"loop"}
#' method constructs the full covariance matrix by iterating over groups.
#'
#' @details
#' # Reporting partner variance: RAW vs. COMBINED
#' Because each row sums multiple independent partner effects (its
#' \code{group_size - 1}), \eqn{\sigma_P^2} can be reported on two
#' different, equally valid bases when \code{weight_partners = FALSE}:
#' \itemize{
#'   \item \strong{RAW} (\eqn{\sigma_P^2} itself): the variance of the
#'     partner-effect parameter, on the same per-person basis as
#'     \eqn{\sigma_A^2}. Use RAW whenever comparing the magnitude of
#'     partner effects to actor effects (e.g. tests of generalized
#'     reciprocity).
#'   \item \strong{COMBINED} (partner's row multiplied by
#'     \code{group_size - 1}): partner identity's total contribution to
#'     the variance of a single observed score. Use COMBINED only when
#'     reading a full variance decomposition in which all components must
#'     sum to the outcome's total variance.
#' }
#' Mixing the two conventions in the same comparison is a common source of
#' error: because COMBINED mechanically multiplies partner's numerator
#' while RAW does not, a naive COMBINED-vs-RAW comparison can make partner
#' effects look several times more (or less) important than actor effects
#' even when the two are, per person, identical in magnitude.
#' \code{print.cpsrm}/\code{summary.cpsrm} report both whenever
#' \code{weight_partners = FALSE} and group size is constant across the
#' data; when group size varies, or \code{weight_partners = TRUE} (partner
#' dummies already rescaled to average rather than sum partner effects),
#' only RAW is shown.
#'
#' # Testing variance components: use the boundary-corrected LRT
#' \code{cpsrm_run} reports a Wald z-statistic (estimate / SE) for each
#' variance component, but variance components are bounded below by zero,
#' so the usual two-sided reference distribution is not correct for
#' testing whether a variance component is zero. The standard correction
#' (Self & Liang, 1987; Snijders & Bosker, 2012; Stram & Lee, 1994) is to
#' fit the model with and without the component of interest (e.g.
#' \code{zero_partner = TRUE} vs. \code{FALSE}), form the likelihood-ratio
#' statistic from each fit's \code{fit["reml_loglik"]}
#' (\eqn{LRT = -2(\ell_{restricted} - \ell_{full})}), and compute a
#' ONE-SIDED p-value:
#' \preformatted{
#' p <- 0.5 * pchisq(LRT, df = 1, lower.tail = FALSE)
#' }
#' Testing TWO variance components jointly at their boundary (e.g. actor
#' and partner both zero) is NOT a simple halving of the df = 2
#' chi-square, because both parameters are boundary-constrained; it
#' requires the three-part mixture (Self & Liang, 1987):
#' \preformatted{
#' p <- 0.25 * pchisq(LRT, df = 0, lower.tail = FALSE) +
#'      0.50 * pchisq(LRT, df = 1, lower.tail = FALSE) +
#'      0.25 * pchisq(LRT, df = 2, lower.tail = FALSE)
#' }
#' (the df = 0 term is always 0 for any \code{LRT > 0}). Applying the
#' plain, uncorrected \code{pchisq} p-value, or halving the df = 2 joint
#' test the same way as a single-component test, both misstate
#' significance and are not valid inferential procedures for this model.
#'
#' @param dv character; name of the dependent variable column
#' @param actor_id character; name of the actor identifier column
#' @param group_id character; name of the group identifier column
#' @param data a \code{data.frame} containing all required variables
#' @param actor_dummies character vector; actor dummy column names
#' @param partner_dummies character vector; partner dummy column names
#' @param group_sizes integer vector or \code{NULL}; per-row group sizes.
#'   Pass the \code{group_sizes} element from \code{\link{create_cp_dummies}}
#' @param location_id character or \code{NULL}; optional higher-level
#'   clustering variable. Default \code{NULL}
#' @param method character; \code{"block"} (default) or \code{"loop"}
#' @param zero_rho logical; fix actor-partner correlation at zero. Default
#'   \code{FALSE}
#' @param zero_actor logical; fix actor variance at zero. Default \code{FALSE}
#' @param zero_partner logical; fix partner variance at zero. Default
#'   \code{FALSE}
#' @param zero_group logical; fix group variance at zero. Default \code{FALSE}
#' @param zero_location logical; fix location variance at zero. Default
#'   \code{FALSE}
#' @param weight_partners logical; weight partner dummies by
#'   \eqn{1/(group\_size - 1)}. Default \code{TRUE}
#' @param fixed_effects character vector or \code{NULL}; fixed effect
#'   predictor column names. Default \code{NULL}
#' @param start named numeric vector or \code{NULL}; optional starting values
#' @param maxit integer; maximum optimizer iterations. Default \code{200000}
#' @param stage1_maxit integer; stage 1 Nelder-Mead iterations. Default
#'   \code{1000}
#' @param stage2_maxit integer; stage 2 Nelder-Mead iterations. Default
#'   \code{1000}
#' @param tol numeric; convergence tolerance. Default \code{1e-10}
#' @param se_method character; \code{"hessian"} (default) or \code{"none"}
#' @param rho_boundary numeric; boundary warning threshold. Default \code{0.99}
#' @param optimizer character; \code{"Nelder-Mead"} (default) or
#'   \code{"L-BFGS-B"}
#' @param verbose logical; print progress. Default \code{FALSE}
#' @param verbose_every integer; print every N evaluations. Default \code{50}
#'
#' @return an object of class \code{"cpsrm"}
#' @seealso \code{\link{create_cp_dummies}}
#' @export
#'
#' @examples
#' \donttest{
#' # Prepare dummies using create_cp_dummies(), bind onto data, then call
#' # cpsrm_run(). See create_cp_dummies() documentation for a full example.
#' }
cpsrm_run <- function(dv,
                      actor_id,
                      group_id,
                      data,
                      actor_dummies,
                      partner_dummies,
                      group_sizes     = NULL,
                      location_id     = NULL,
                      method          = c("block", "loop"),
                      zero_rho        = FALSE,
                      zero_actor      = FALSE,
                      zero_partner    = FALSE,
                      zero_group      = FALSE,
                      zero_location   = FALSE,
                      weight_partners = TRUE,
                      fixed_effects   = NULL,
                      start           = NULL,
                      maxit           = 200000L,
                      stage1_maxit    = 1000L,
                      stage2_maxit    = 1000L,
                      tol             = 1e-10,
                      se_method       = c("hessian", "none"),
                      rho_boundary    = 0.99,
                      optimizer       = c("Nelder-Mead", "L-BFGS-B"),
                      verbose         = FALSE,
                      verbose_every   = 50L) {

  cl          <- match.call()
  se_method   <- match.arg(se_method)
  optimizer   <- match.arg(optimizer)
  method      <- match.arg(method)
  use_loc     <- !is.null(location_id)

  if (zero_location && !use_loc) {
    warning("zero_location = TRUE has no effect when location_id is not specified.")
    zero_location <- FALSE
  }
  use_group  <- !zero_group
  use_loc_re <- use_loc && !zero_location

  # 1. Validate
  stopifnot(is.data.frame(data))
  required_cols <- c(dv, actor_id, group_id, actor_dummies, partner_dummies,
                     fixed_effects, location_id)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Columns not found in 'data': ", paste(missing_cols, collapse = ", "))
  if (length(actor_dummies) != length(partner_dummies))
    stop("'actor_dummies' and 'partner_dummies' must have the same length.")

  # 2. Extract matrices
  y  <- as.numeric(data[[dv]])
  n  <- length(y)
  p  <- length(actor_dummies)

  Za <- as.matrix(data[, actor_dummies,   drop = FALSE]); storage.mode(Za) <- "double"
  Zp <- as.matrix(data[, partner_dummies, drop = FALSE]); storage.mode(Zp) <- "double"

  if (is.null(group_sizes)) {
    raw_partner_sums <- rowSums(Zp)
    if (weight_partners && all(abs(raw_partner_sums - 1) < 1e-6)) {
      warning("group_sizes not provided and partner dummies appear already weighted. ",
              "Assuming uniform group size = 3.")
      row_group_size <- rep(3L, n)
    } else {
      row_group_size <- as.integer(round(raw_partner_sums)) + 1L
    }
  } else {
    row_group_size <- as.integer(group_sizes)
    if (length(row_group_size) != n) stop("group_sizes must have length equal to nrow(data).")
    if (any(row_group_size < 2))    stop("All group sizes must be >= 2.")
  }

  variable_group_sizes <- length(unique(row_group_size)) > 1
  if (variable_group_sizes && verbose)
    cat(sprintf("Variable group sizes detected: %s\n",
                paste(sort(unique(row_group_size)), collapse = ", ")))

  .dummies_preweighted <- !is.null(group_sizes) && weight_partners
  if (weight_partners && !.dummies_preweighted) {
    w_vec <- 1.0 / (row_group_size - 1L)
    Zp <- sweep(Zp, 1L, w_vec, `*`)
  }

  grp_factor <- factor(data[[group_id]])
  n_groups   <- nlevels(grp_factor)
  Zg         <- stats::model.matrix(~ grp_factor - 1)

  if (use_loc) {
    loc_factor <- factor(data[[location_id]])
    n_locs     <- nlevels(loc_factor)
    Zl         <- stats::model.matrix(~ loc_factor - 1)
  } else { n_locs <- 0L }

  if (is.null(fixed_effects)) {
    X <- matrix(1, nrow = n, ncol = 1); colnames(X) <- "(Intercept)"
  } else {
    X <- stats::model.matrix(~ ., data = data[, fixed_effects, drop = FALSE])
  }
  p_fixed   <- ncol(X)
  n_persons <- p

  if (any(abs(rowSums(Za) - 1) > 1e-9))
    warning("Some actor dummy rows do not sum to 1. Check actor_dummies.")
  if (weight_partners && !.dummies_preweighted && any(abs(rowSums(Zp) - 1) > 1e-9))
    warning("After weighting, some partner dummy rows do not sum to 1.")

  if (!requireNamespace("Matrix", quietly = TRUE))
    stop("The 'Matrix' package is required. Install with install.packages('Matrix').")

  # 3a. Block pre-computation
  if (method == "block") {
    col <- 0L
    if (zero_actor && zero_partner)       { a_idx <- integer(0); b_idx <- integer(0)
    } else if (zero_actor)                { a_idx <- integer(0); b_idx <- col + seq_len(p); col <- col + p
    } else if (zero_partner)              { a_idx <- col + seq_len(p); col <- col + p; b_idx <- integer(0)
    } else                                { a_idx <- col + seq_len(p); col <- col + p
                                            b_idx <- col + seq_len(p); col <- col + p }
    if (use_group)  { g_idx <- col + seq_len(n_groups); col <- col + n_groups } else g_idx <- integer(0)
    if (use_loc_re) { l_idx <- col + seq_len(n_locs);   col <- col + n_locs  } else l_idx <- integer(0)

    Z_blocks <- list()
    if (!zero_actor && !zero_partner) Z_blocks <- c(Z_blocks, list(Za, Zp))
    else if (zero_actor)              Z_blocks <- c(Z_blocks, list(Zp))
    else if (zero_partner)            Z_blocks <- c(Z_blocks, list(Za))
    if (use_group)  Z_blocks <- c(Z_blocks, list(Zg))
    if (use_loc_re) Z_blocks <- c(Z_blocks, list(Zl))
    Z_mat <- do.call(cbind, Z_blocks)
    q <- ncol(Z_mat)
    if (verbose) cat(sprintf("Pre-computing ZtZ (%d x %d)...\n", q, q))
    ZtZ_ds  <- crossprod(Z_mat)
    Zty     <- as.numeric(crossprod(Z_mat, y))
    ZtX_pre <- crossprod(Z_mat, X)
  }

  # 3b. Loop pre-computation
  if (method == "loop") {
    group_rows <- split(seq_len(n), grp_factor)
    actor_col  <- max.col(Za, ties.method = "first")
    if (use_loc) loc_col <- max.col(Zl, ties.method = "first")
  }

  # 4. Parameter counting
  n_ap    <- if (zero_actor && zero_partner) 0L else if (zero_actor || zero_partner) 1L else if (zero_rho) 2L else 3L
  n_theta <- n_ap + (if (zero_group) 0L else 1L) + (if (use_loc_re) 1L else 0L) + 1L

  # 5. Helpers
  unpack_theta <- function(theta) {
    idx <- 1L
    if (zero_actor && zero_partner) { sA <- 0; sP <- 0; rho <- 0; sAP <- 0
    } else if (zero_actor)  { sA <- 0; rho <- 0; sAP <- 0; sP <- exp(theta[idx]); idx <- idx+1L
    } else if (zero_partner){ sP <- 0; rho <- 0; sAP <- 0; sA <- exp(theta[idx]); idx <- idx+1L
    } else if (zero_rho)    { sA <- exp(theta[idx]); idx <- idx+1L; sP <- exp(theta[idx]); idx <- idx+1L; rho <- 0; sAP <- 0
    } else { sA <- exp(theta[idx]); idx <- idx+1L; sP <- exp(theta[idx]); idx <- idx+1L
              rho <- tanh(max(-10,min(10,theta[idx]))); idx <- idx+1L; sAP <- rho*sA*sP }
    sg <- if (zero_group)  0 else { v <- exp(theta[idx]); idx <- idx+1L; v }
    sl <- if (use_loc_re) { v <- exp(theta[idx]); idx <- idx+1L; v } else 0
    se <- exp(theta[n_theta])
    list(sA=sA,sP=sP,rho=rho,sAP=sAP,sg=sg,sl=sl,se=se,idx=idx)
  }

  .call_count <- 0L
  .t_start    <- proc.time()["elapsed"]
  .in_hessian <- FALSE

  print_progress <- function(par, val) {
    if (!verbose || .in_hessian || .call_count %% as.integer(verbose_every) != 0L) return()
    elapsed <- round((proc.time()["elapsed"] - .t_start) / 60, 1)
    sg_str <- if (zero_group) "  sg=0(fixed)" else sprintf("  sg=%.4f", par$sg)
    sl_str <- if (use_loc_re) sprintf("  sl=%.4f", par$sl) else ""
    if (zero_actor && zero_partner)
      cat(sprintf("  [eval %4d | %5.1f min]%s%s  se=%.4f  negLL=%.2f\n",
                  .call_count, elapsed, sg_str, sl_str, par$se, val))
    else if (zero_actor || zero_partner)
      cat(sprintf("  [eval %4d | %5.1f min]  s=%.4f%s%s  se=%.4f  negLL=%.2f\n",
                  .call_count, elapsed, if(zero_actor) par$sP else par$sA, sg_str, sl_str, par$se, val))
    else if (zero_rho)
      cat(sprintf("  [eval %4d | %5.1f min]  sA=%.4f  sP=%.4f%s%s  se=%.4f  negLL=%.2f\n",
                  .call_count, elapsed, par$sA, par$sP, sg_str, sl_str, par$se, val))
    else
      cat(sprintf("  [eval %4d | %5.1f min]  sA=%.4f  sP=%.4f  rho=%+.3f%s%s  se=%.4f  negLL=%.2f\n",
                  .call_count, elapsed, par$sA, par$sP, par$rho, sg_str, sl_str, par$se, val))
  }

  # Block likelihood
  reml_negloglik_block <- function(theta) {
    .call_count <<- .call_count + 1L
    par <- unpack_theta(theta)
    sA <- par$sA; sP <- par$sP; sAP <- par$sAP; sg <- par$sg; sl <- par$sl; se <- par$se; se2 <- se^2
    M <- ZtZ_ds / se2; log_det_D <- 0
    if (zero_actor && zero_partner) {
    } else if (zero_actor)  { diag(M)[b_idx] <- diag(M)[b_idx] + 1/sP^2; log_det_D <- log_det_D + p*log(sP^2)
    } else if (zero_partner){ diag(M)[a_idx] <- diag(M)[a_idx] + 1/sA^2; log_det_D <- log_det_D + p*log(sA^2)
    } else if (zero_rho)    { diag(M)[a_idx] <- diag(M)[a_idx]+1/sA^2; diag(M)[b_idx] <- diag(M)[b_idx]+1/sP^2
                               log_det_D <- log_det_D + p*log(sA^2) + p*log(sP^2)
    } else {
      det_tau <- sA^2*sP^2 - sAP^2; if (det_tau < 1e-12) return(1e10)
      diag(M)[a_idx] <- diag(M)[a_idx]+sP^2/det_tau; diag(M)[b_idx] <- diag(M)[b_idx]+sA^2/det_tau
      ap_off <- -sAP/det_tau
      for (i in seq_len(p)) { M[a_idx[i],b_idx[i]] <- M[a_idx[i],b_idx[i]]+ap_off; M[b_idx[i],a_idx[i]] <- M[b_idx[i],a_idx[i]]+ap_off }
      log_det_D <- log_det_D + p*log(det_tau)
    }
    if (!zero_group)  { diag(M)[g_idx] <- diag(M)[g_idx]+1/sg^2; log_det_D <- log_det_D+n_groups*log(sg^2) }
    if (use_loc_re)   { diag(M)[l_idx] <- diag(M)[l_idx]+1/sl^2; log_det_D <- log_det_D+n_locs*log(sl^2) }
    L_try <- tryCatch(chol(M), error=function(e) NULL); if (is.null(L_try)) return(1e10)
    log_det_M <- 2*sum(log(diag(L_try))); log_det_V <- log_det_M + log_det_D + n*log(se2)
    rhs_all <- cbind(Zty, ZtX_pre)/se2
    sol_all  <- backsolve(L_try, forwardsolve(t(L_try), rhs_all))
    Viy      <- y/se2 - (Z_mat %*% sol_all[,1])/se2
    ViX      <- X/se2 - (Z_mat %*% sol_all[,-1,drop=FALSE])/se2
    XtViX <- crossprod(X,ViX); XtViy <- crossprod(X,Viy)
    beta  <- tryCatch(as.numeric(solve(XtViX,XtViy)), error=function(e) NULL); if (is.null(beta)) return(1e10)
    log_det_XtViX <- as.numeric(determinant(XtViX,logarithm=TRUE)$modulus); if (!is.finite(log_det_XtViX)) return(1e10)
    r <- y - as.numeric(X %*% beta)
    Ztr <- crossprod(Z_mat,r); sol_r <- backsolve(L_try, forwardsolve(t(L_try), Ztr/se2))
    Vir <- r/se2 - (Z_mat %*% sol_r)/se2; quad <- as.numeric(crossprod(r,Vir))
    val <- 0.5*((n-p_fixed)*log(2*pi) + log_det_V + log_det_XtViX + quad)
    print_progress(par, val); val
  }

  # Loop likelihood
  reml_negloglik_loop <- function(theta) {
    .call_count <<- .call_count + 1L
    par <- unpack_theta(theta)
    sA <- par$sA; sP <- par$sP; sAP <- par$sAP; sg <- par$sg; sl <- par$sl; se <- par$se; se2 <- se^2
    var_A <- sA^2; var_P <- sP^2; var_g <- sg^2; var_l <- sl^2
    V <- diag(se2, n)
    if (!zero_actor && var_A > 0) for (k in seq_len(p)) { rk <- which(actor_col==k); if (length(rk)>0) V[rk,rk] <- V[rk,rk]+var_A }
    if (!zero_partner && var_P > 0) for (k in seq_len(p)) { rk <- which(Zp[,k]>0); if (length(rk)==0) next; wk <- Zp[rk,k]; V[rk,rk] <- V[rk,rk]+outer(wk,wk)*var_P }
    if (!zero_rho && !zero_actor && !zero_partner && abs(sAP)>0) for (i in seq_len(n)) { ai <- actor_col[i]; rai <- which(Zp[,ai]>0); if (length(rai)==0) next; wai <- Zp[rai,ai]; V[i,rai] <- V[i,rai]+wai*sAP; V[rai,i] <- V[rai,i]+wai*sAP }
    if (!zero_group && var_g>0) for (grp in names(group_rows)) { ri <- group_rows[[grp]]; V[ri,ri] <- V[ri,ri]+var_g }
    if (use_loc_re && var_l>0) { lgrps <- split(seq_len(n), loc_col); for (loc in names(lgrps)) { ri <- lgrps[[loc]]; V[ri,ri] <- V[ri,ri]+var_l } }
    Lv <- tryCatch(chol(V), error=function(e) NULL); if (is.null(Lv)) return(1e10)
    log_det_V <- 2*sum(log(diag(Lv)))
    Viy <- backsolve(Lv, forwardsolve(t(Lv), y)); ViX <- backsolve(Lv, forwardsolve(t(Lv), X))
    XtViX <- crossprod(X,ViX); XtViy <- crossprod(X,Viy)
    beta <- tryCatch(as.numeric(solve(XtViX,XtViy)), error=function(e) NULL); if (is.null(beta)) return(1e10)
    log_det_XtViX <- as.numeric(determinant(XtViX,logarithm=TRUE)$modulus); if (!is.finite(log_det_XtViX)) return(1e10)
    r <- y-as.numeric(X %*% beta); Vir <- backsolve(Lv, forwardsolve(t(Lv),r)); quad <- as.numeric(crossprod(r,Vir))
    val <- 0.5*((n-p_fixed)*log(2*pi)+log_det_V+log_det_XtViX+quad)
    print_progress(par, val); val
  }

  reml_negloglik <- if (method == "block") reml_negloglik_block else reml_negloglik_loop

  # 6. Starting values
  sd_y <- stats::sd(y, na.rm = TRUE)
  if (is.null(start)) {
    start <- c()
    if      (zero_actor && zero_partner) {}
    else if (zero_actor)   start <- c(start, log_sP = log(sd_y*0.25))
    else if (zero_partner) start <- c(start, log_sA = log(sd_y*0.4))
    else if (zero_rho)     start <- c(start, log_sA=log(sd_y*0.4), log_sP=log(sd_y*0.25))
    else                   start <- c(start, log_sA=log(sd_y*0.4), log_sP=log(sd_y*0.25), atanh_rho=0)
    if (!zero_group)  start <- c(start, log_sg = log(sd_y*0.15))
    if (use_loc_re)   start <- c(start, log_sl = log(sd_y*0.5))
    start <- c(start, log_se = log(sd_y*0.7))
  }

  # 7. Optimization
  if (optimizer == "L-BFGS-B") {
    if (verbose) cat("Optimization stage 1 (Nelder-Mead, coarse)...\n")
    opt0 <- stats::optim(start, reml_negloglik, method="Nelder-Mead", control=list(maxit=500L, reltol=1e-4))
    if (verbose) cat("Optimization stage 2 (Nelder-Mead, medium)...\n")
    opt1 <- stats::optim(opt0$par, reml_negloglik, method="Nelder-Mead", control=list(maxit=500L, reltol=1e-7))
    if (verbose) cat("Optimization stage 3 (L-BFGS-B, fine)...\n")
    opt2 <- tryCatch(
      stats::optim(opt1$par, reml_negloglik, method="L-BFGS-B",
                   lower=rep(-8,n_theta), upper=rep(8,n_theta),
                   control=list(maxit=maxit, factr=1e3, pgtol=tol*0.001)),
      error=function(e) { if(verbose) cat("L-BFGS-B failed:",conditionMessage(e),"-- using NM.\n"); opt1 })
    if (opt2$value > opt1$value) opt2 <- opt1
  } else {
    if (verbose) cat("Optimization stage 1 (Nelder-Mead)...\n")
    opt1 <- stats::optim(start, reml_negloglik, method="Nelder-Mead", control=list(maxit=stage1_maxit, reltol=tol))
    if (verbose) cat("Optimization stage 2 (Nelder-Mead)...\n")
    opt2 <- stats::optim(opt1$par, reml_negloglik, method="Nelder-Mead", control=list(maxit=stage2_maxit, reltol=tol*0.001))
  }

  theta_hat <- opt2$par; conv <- opt2$convergence
  if (verbose) {
    if (conv==0) cat(sprintf("Optimizer converged successfully (%d total evaluations).\n", .call_count))
    else         cat(sprintf("WARNING: Optimizer did not converge (code = %d, %d evaluations).\n", conv, .call_count))
  }

  # 8. Back-transform
  {
    idx <- 1L
    if (zero_actor && zero_partner)  { sA_hat<-0; sP_hat<-0; rho_hat<-0
    } else if (zero_actor)  { sA_hat<-0; rho_hat<-0; sP_hat<-exp(theta_hat[idx]); idx<-idx+1L
    } else if (zero_partner){ sP_hat<-0; rho_hat<-0; sA_hat<-exp(theta_hat[idx]); idx<-idx+1L
    } else if (zero_rho)    { sA_hat<-exp(theta_hat[idx]); idx<-idx+1L; sP_hat<-exp(theta_hat[idx]); idx<-idx+1L; rho_hat<-0
    } else { sA_hat<-exp(theta_hat[idx]); idx<-idx+1L; sP_hat<-exp(theta_hat[idx]); idx<-idx+1L
              rho_hat<-tanh(max(-10,min(10,theta_hat[idx]))); idx<-idx+1L }
    sg_hat <- if (!zero_group) { v<-exp(theta_hat[idx]); idx<-idx+1L; v } else 0
    sl_hat <- if (use_loc_re)  { v<-exp(theta_hat[idx]); idx<-idx+1L; v } else NA_real_
    se_hat <- exp(theta_hat[n_theta])
  }
  sAP_hat <- rho_hat*sA_hat*sP_hat
  var_A <- sA_hat^2; var_P <- sP_hat^2; var_g <- sg_hat^2; var_e <- se_hat^2
  var_l <- if (use_loc_re) sl_hat^2 else NA_real_
  rho_boundary_hit <- !zero_rho && !zero_actor && abs(rho_hat) >= rho_boundary

  # 9. GLS at final parameters
  if (method == "block") {
    se2_h <- se_hat^2; M_h <- ZtZ_ds/se2_h
    if (zero_actor && zero_partner) {
    } else if (zero_actor)  { diag(M_h)[b_idx] <- diag(M_h)[b_idx]+1/var_P
    } else if (zero_partner){ diag(M_h)[a_idx] <- diag(M_h)[a_idx]+1/var_A
    } else if (zero_rho)    { diag(M_h)[a_idx] <- diag(M_h)[a_idx]+1/var_A; diag(M_h)[b_idx] <- diag(M_h)[b_idx]+1/var_P
    } else {
      dt <- var_A*var_P-sAP_hat^2; diag(M_h)[a_idx] <- diag(M_h)[a_idx]+var_P/dt; diag(M_h)[b_idx] <- diag(M_h)[b_idx]+var_A/dt
      ap_off <- -sAP_hat/dt
      for (i in seq_len(p)) { M_h[a_idx[i],b_idx[i]] <- M_h[a_idx[i],b_idx[i]]+ap_off; M_h[b_idx[i],a_idx[i]] <- M_h[b_idx[i],a_idx[i]]+ap_off }
    }
    if (!zero_group) diag(M_h)[g_idx] <- diag(M_h)[g_idx]+1/var_g
    if (use_loc_re)  diag(M_h)[l_idx] <- diag(M_h)[l_idx]+1/var_l
    L_h <- chol(M_h)
    sol_y <- backsolve(L_h, forwardsolve(t(L_h), Zty/se2_h)); Viy_h <- y/se2_h-(Z_mat %*% sol_y)/se2_h
    ZtX <- crossprod(Z_mat,X); sol_X <- backsolve(L_h, forwardsolve(t(L_h), ZtX/se2_h))
    ViX_h <- X/se2_h-(Z_mat %*% sol_X)/se2_h
    XtViX_h <- crossprod(X,ViX_h); XtViy_h <- crossprod(X,Viy_h)
  } else {
    V_f <- diag(se_hat^2, n)
    if (!zero_actor && var_A>0) for (k in seq_len(p)) { rk <- which(actor_col==k); if(length(rk)>0) V_f[rk,rk] <- V_f[rk,rk]+var_A }
    if (!zero_group && var_g>0) for (grp in names(group_rows)) { ri <- group_rows[[grp]]; V_f[ri,ri] <- V_f[ri,ri]+var_g }
    if (use_loc_re && !is.na(var_l) && var_l>0) { lgrps <- split(seq_len(n),loc_col); for (loc in names(lgrps)) { ri <- lgrps[[loc]]; V_f[ri,ri] <- V_f[ri,ri]+var_l } }
    if (!zero_partner && var_P>0) for (k in seq_len(p)) { rk <- which(Zp[,k]>0); if(length(rk)==0) next; wk <- Zp[rk,k]; V_f[rk,rk] <- V_f[rk,rk]+outer(wk,wk)*var_P }
    Lv_f <- tryCatch(chol(V_f), error=function(e) NULL)
    if (is.null(Lv_f)) { beta_hat <- rep(NA_real_,p_fixed); se_beta <- rep(NA_real_,p_fixed)
    } else { ViX_h <- backsolve(Lv_f,forwardsolve(t(Lv_f),X)); Viy_h <- backsolve(Lv_f,forwardsolve(t(Lv_f),y))
              XtViX_h <- crossprod(X,ViX_h); XtViy_h <- crossprod(X,Viy_h) }
  }
  beta_hat <- as.numeric(solve(XtViX_h,XtViy_h)); se_beta <- sqrt(diag(solve(XtViX_h)))
  names(beta_hat) <- colnames(X); names(se_beta) <- colnames(X)

  # 10. Standard errors
  se_estimates <- rep(NA_real_, n_theta)
  if (se_method == "hessian" && !rho_boundary_hit) {
    if (method == "loop") warning("se_method = 'hessian' with method = 'loop' may be very slow. Consider method = 'block'.")
    .in_hessian <- TRUE
    if (verbose) cat(sprintf("Computing Hessian (~%d evaluations)...\n", n_theta*(n_theta+1)*2))
    H <- if (requireNamespace("numDeriv", quietly=TRUE)) {
      tryCatch(numDeriv::hessian(reml_negloglik, theta_hat), error=function(e) NULL)
    } else {
      eps <- 1e-4; H_fd <- matrix(0,n_theta,n_theta)
      for (i in seq_len(n_theta)) for (j in seq_len(n_theta)) {
        tp<-theta_hat;tp[i]<-tp[i]+eps;tp[j]<-tp[j]+eps; tm<-theta_hat;tm[i]<-tm[i]-eps;tm[j]<-tm[j]-eps
        tpm<-theta_hat;tpm[i]<-tpm[i]+eps;tpm[j]<-tpm[j]-eps; tmp<-theta_hat;tmp[i]<-tmp[i]-eps;tmp[j]<-tmp[j]+eps
        H_fd[i,j] <- (reml_negloglik(tp)-reml_negloglik(tpm)-reml_negloglik(tmp)+reml_negloglik(tm))/(4*eps^2)
      }; H_fd
    }
    if (verbose) cat("Hessian complete.\n")
    if (!is.null(H)) {
      cov_theta <- tryCatch(solve(H), error=function(e) NULL)
      if (!is.null(cov_theta)) {
        J <- matrix(0,n_theta,n_theta); idx <- 1L
        if (zero_actor && zero_partner) {
        } else if (zero_actor || zero_partner) { J[idx,idx] <- 2*(if(zero_actor) var_P else var_A); idx <- idx+1L
        } else if (zero_rho) { J[idx,idx]<-2*var_A;idx<-idx+1L; J[idx,idx]<-2*var_P;idx<-idx+1L
        } else { J[idx,idx]<-2*var_A;idx<-idx+1L; J[idx,idx]<-2*var_P;idx<-idx+1L
                  J[idx,1]<-sAP_hat;J[idx,2]<-sAP_hat; J[idx,idx]<-(1-rho_hat^2)*sA_hat*sP_hat;idx<-idx+1L }
        if (!zero_group) { J[idx,idx]<-2*var_g;idx<-idx+1L }
        if (use_loc_re)  { J[idx,idx]<-2*var_l;idx<-idx+1L }
        J[n_theta,n_theta] <- 2*var_e
        cov_nat <- J %*% cov_theta %*% t(J); se_estimates <- sqrt(pmax(0,diag(cov_nat)))
      }
    }
  }
  if (rho_boundary_hit) se_estimates[3] <- NA_real_

  # 11. Output
  reml_ll <- -opt2$value; aic <- -2*reml_ll+2*n_theta; bic <- -2*reml_ll+log(n)*n_theta

  if (zero_actor && zero_partner)  { comp_names<-character(0); nat_params<-numeric(0)
  } else if (zero_actor)   { comp_names<-"Partner variance (sigma_P^2)"; nat_params<-var_P
  } else if (zero_partner) { comp_names<-"Actor variance (sigma_A^2)";   nat_params<-var_A
  } else if (zero_rho)     { comp_names<-c("Actor variance (sigma_A^2)","Partner variance (sigma_P^2)"); nat_params<-c(var_A,var_P)
  } else { comp_names<-c("Actor variance (sigma_A^2)","Partner variance (sigma_P^2)","Actor-partner covariance (sigma_AP)"); nat_params<-c(var_A,var_P,sAP_hat) }
  if (!zero_group)  { comp_names<-c(comp_names,"Group variance (sigma_g^2)");    nat_params<-c(nat_params,var_g) }
  if (use_loc_re)   { comp_names<-c(comp_names,"Location variance (sigma_l^2)"); nat_params<-c(nat_params,var_l) }
  comp_names<-c(comp_names,"Residual variance (sigma_e^2)"); nat_params<-c(nat_params,var_e)

  estimates_df <- data.frame(component=comp_names, estimate=nat_params, se=se_estimates,
                              z=nat_params/se_estimates, row.names=NULL, stringsAsFactors=FALSE)

  var_total <- var_A+var_P+var_e
  if (!zero_group) var_total <- var_total+var_g
  if (use_loc_re)  var_total <- var_total+var_l

  correlations <- c(
    rho_AP       = if(zero_rho||zero_actor) 0 else if(is.finite(rho_hat)) rho_hat else NA_real_,
    ICC_group    = if(!zero_group) as.numeric(var_g/var_total) else 0,
    ICC_location = if(use_loc_re) as.numeric(var_l/var_total) else NA_real_
  )
  fit_stats <- c(reml_loglik=reml_ll, AIC=aic, BIC=bic, n_obs=n, n_groups=n_groups,
                 n_locations=if(use_loc) n_locs else NA_real_, n_persons=n_persons)

  group_size_summary <- table(row_group_size); names(dimnames(group_size_summary)) <- "group_size"

  # Partners contributing to each row (group_size - 1). Needed by
  # print.cpsrm to report how much of an observed score's total variance
  # traces to partner identity when weight_partners = FALSE: each row's
  # linear predictor sums this many independent partner effects, so their
  # combined contribution to that row's variance is n_partners * sigma_P^2,
  # not sigma_P^2 alone. Only meaningful as a single number when group size
  # is constant across rows (variable_group_sizes == FALSE).
  n_partners_per_row <- if (!variable_group_sizes) {
    unique(row_group_size) - 1L
  } else {
    mean(row_group_size) - 1
  }

  result <- structure(
    list(estimates=estimates_df, fixed.effects=list(estimate=beta_hat,se=se_beta),
         correlations=correlations, fit=fit_stats, convergence=conv,
         rho_boundary_hit=rho_boundary_hit, zero_rho=zero_rho, zero_actor=zero_actor,
         zero_partner=zero_partner, zero_group=zero_group, zero_location=zero_location,
         weight_partners=weight_partners, method=method,
         variable_group_sizes=variable_group_sizes, group_size_summary=group_size_summary,
         n_partners_per_row=n_partners_per_row,
         use_location=use_loc, use_location_re=use_loc_re, optimizer=optimizer,
         dv_name=dv, n_evals=.call_count, optim.result=opt2, call=cl),
    class="cpsrm")

  print(result); invisible(result)
}


#' Print a cpsrm Object
#' @param x an object of class \code{"cpsrm"}
#' @param digits integer; number of digits to print. Default \code{4}
#' @param ... additional arguments (currently unused)
#' @return \code{x}, invisibly
#' @export
print.cpsrm <- function(x, digits=4, ...) {
  cat("\n========================================\n")
  cat(" Co-Partner Social Relations Model\n")
  cat(sprintf(" Outcome: %s\n", x$dv_name))
  cat(sprintf(" Method: %s\n", x$method))
  if (x$variable_group_sizes) {
    sz <- x$group_size_summary
    cat(sprintf(" Group sizes: %s\n", paste(sprintf("%s (n=%s)", names(sz), as.integer(sz)), collapse=", ")))
  }
  if (x$zero_rho)      cat(" (Actor-partner correlation fixed at zero)\n")
  if (x$zero_actor)    cat(" (Actor variance fixed at zero)\n")
  if (x$zero_partner)  cat(" (Partner variance fixed at zero)\n")
  if (x$zero_group)    cat(" (Group variance fixed at zero)\n")
  if (x$zero_location) cat(" (Location variance fixed at zero)\n")
  if (x$weight_partners) {
    cat(" (Partner dummies weighted by 1/(group_size - 1))\n")
  } else {
    npr     <- x$n_partners_per_row
    npr_txt <- if (!x$variable_group_sizes) {
      sprintf("%d", as.integer(round(npr)))
    } else {
      sprintf("~%.1f (varies by row)", npr)
    }
    cat(sprintf(paste0(
      " (Partner dummies unweighted: sigma_P^2 is the per-person partner-\n",
      "  effect variance, on the same basis as sigma_A^2. Each row sums %s\n",
      "  independent partner effects, so partner's contribution to a single\n",
      "  observed score's variance is %s x sigma_P^2 -- see RAW vs. COMBINED\n",
      "  below, and Details in ?cpsrm_run.)\n"),
      npr_txt, npr_txt))
  }
  cat(sprintf(" Optimizer: %s (%d evaluations)\n", x$optimizer, as.integer(x$n_evals)))
  cat("========================================\n\n")
  if (x$convergence==0) cat("Convergence: SUCCESS\n\n")
  else cat(sprintf("Convergence: WARNING - optimizer did not converge (code = %d)\n\n", x$convergence))
  if (!x$zero_rho && x$rho_boundary_hit)
    cat("NOTE: |rho_AP| near 1 (boundary). SE for AP covariance suppressed.\n\n")
  fe <- x$fixed.effects; cat("Fixed Effects:\n")
  for (i in seq_along(fe$estimate))
    cat(sprintf("  %-20s = %8.4f  (SE = %.4f)\n", names(fe$estimate)[i], fe$estimate[i], fe$se[i]))
  cat("\nVariance Components:\n")
  est            <- x$estimates
  var_rows       <- !grepl("covariance", est$component)
  is_partner_row <- grepl("^Partner variance", est$component)

  # COMBINED only applies when partners are unweighted AND group size is
  # constant across the dataset (see Details in ?cpsrm_run).
  combine_partner <- !x$weight_partners && !x$variable_group_sizes
  mult <- if (combine_partner) x$n_partners_per_row else 1

  contrib_raw      <- est$estimate
  contrib_combined <- est$estimate
  if (combine_partner)
    contrib_combined[is_partner_row] <- contrib_combined[is_partner_row] * mult

  var_total_raw      <- sum(contrib_raw[var_rows])
  var_total_combined <- sum(contrib_combined[var_rows])

  for (i in seq_len(nrow(est))) {
    se_str <- if (!is.na(est$se[i])) sprintf("  SE = %7.4f", est$se[i]) else "              "
    z_str  <- if (!is.na(est$z[i]) && is.finite(est$z[i])) sprintf("  z = %6.2f", est$z[i]) else ""
    pct_str <- if (!var_rows[i]) {
      ""
    } else if (combine_partner) {
      sprintf("  (RAW %5.1f%% / COMBINED %5.1f%%)",
              100 * contrib_raw[i]      / var_total_raw,
              100 * contrib_combined[i] / var_total_combined)
    } else {
      sprintf("  (%5.1f%%)", 100 * contrib_raw[i] / var_total_raw)
    }
    cat(sprintf("  %-40s  %9.5f%s%s%s\n", est$component[i], est$estimate[i], se_str, z_str, pct_str))
  }

  cat("\nVariance Decomposition (% of total):\n")
  if (combine_partner) {
    cat("  RAW = each component's own share of total variance; appropriate\n")
    cat("  for comparing Partner's effect size to Actor's. COMBINED scales\n")
    cat(sprintf(paste0(
      "  Partner's row by %s (partners summed into each row) to reflect its\n",
      "  true contribution to a single observed score's variance; use\n",
      "  COMBINED only for reading the total-variance decomposition. See\n",
      "  Details in ?cpsrm_run.\n\n"),
      sprintf("%d", as.integer(round(mult)))))
    for (i in which(var_rows))
      cat(sprintf("  %-42s  RAW %5.1f%%   COMBINED %5.1f%%\n",
                  est$component[i],
                  100 * contrib_raw[i]      / var_total_raw,
                  100 * contrib_combined[i] / var_total_combined))
  } else {
    for (i in which(var_rows))
      cat(sprintf("  %-42s  %5.1f%%\n", est$component[i], 100 * contrib_raw[i] / var_total_raw))
  }
  cat("\nCorrelations:\n")
  rho <- as.numeric(x$correlations["rho_AP"])
  cat(sprintf("  Actor-partner rho (generalized reciprocity) = %s\n",
              if(x$zero_rho||x$zero_actor) "0 (fixed)" else if(is.finite(rho)) sprintf("%.4f",rho) else "NA (boundary)"))
  if (!x$zero_group) cat(sprintf("  Group ICC                                   = %.4f\n", as.numeric(x$correlations["ICC_group"])))
  else cat("  Group ICC                                   = 0 (fixed)\n")
  if (x$use_location) {
    if (x$use_location_re) cat(sprintf("  Location ICC                                = %.4f\n", as.numeric(x$correlations["ICC_location"])))
    else cat("  Location ICC                                = 0 (fixed)\n")
  }
  cat("\nFit Statistics (REML):\n")
  cat(sprintf("  -2 * REML log-lik = %.3f\n", -2*x$fit["reml_loglik"]))
  cat(sprintf("  AIC               = %.3f\n",  x$fit["AIC"]))
  cat(sprintf("  BIC               = %.3f\n",  x$fit["BIC"]))
  cat(sprintf("  N (observations)  = %d\n",    as.integer(x$fit["n_obs"])))
  cat(sprintf("  N (groups)        = %d\n",    as.integer(x$fit["n_groups"])))
  if (x$use_location) cat(sprintf("  N (locations)     = %d\n", as.integer(x$fit["n_locations"])))
  cat(sprintf("  N (persons)       = %d\n",    as.integer(x$fit["n_persons"])))
  cat("\n"); invisible(x)
}

#' Summarize a cpsrm Object
#' @param object an object of class \code{"cpsrm"}
#' @param ... additional arguments passed to \code{\link{print.cpsrm}}
#' @return \code{object}, invisibly
#' @export
summary.cpsrm <- function(object, ...) { print(object, ...); invisible(object) }
