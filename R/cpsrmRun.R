# =============================================================================
# cpsrmRun.R  —  Co-Partner Social Relations Model
# =============================================================================
#
# Model:
#   Y_{i(jk)gt} = mu + A_i + P_j + P_k + G_g + [L_l] + e
#
# Random effects:
#   A_i  ~ N(0, sigma_A^2),  P_i  ~ N(0, sigma_P^2)
#   Cov(A_i, P_i) = sigma_AP  [= 0 when zero.rho = TRUE]
#   G_g  ~ N(0, sigma_g^2),  L_l  ~ N(0, sigma_l^2) [optional],  e ~ N(0, sigma_e^2)
#
# Woodbury formulation (avoids n x n matrix entirely):
#   V = se^2 * I_n + Z * D * Z'
#   Z = [Za | Zp | Zg | (Zl)]   (n x q, q << n)
#   |V| = se^{2n} * |D| * |D^{-1} + Z'Z/se^2|
#   V^{-1}v = v/se^2 - Z * M^{-1} * Z'v / se^4
#   where M = D^{-1} + Z'Z/se^2  (q x q, factored via dense Cholesky)
# =============================================================================


#' Run the Co-Partner Social Relations Model
#'
#' Fits the Co-Partner Social Relations Model (CP-SRM) using restricted maximum
#' likelihood (REML) via a Woodbury matrix identity formulation. In the CP-SRM,
#' each observation involves one actor and two partners simultaneously, so the
#' partner random effect enters twice per row. The model decomposes variance
#' in a directed outcome into actor, partner, group, optional location, and
#' residual components.
#'
#' @param dv character; name of the dependent variable column
#' @param actor.id character; name of the actor identifier column
#' @param group.id character; name of the group identifier column
#' @param data a \code{data.frame} containing all required variables
#' @param part.id character or \code{NULL}; name of the partner identifier
#'   column. Required when \code{dummies = NULL} so that
#'   \code{\link{createDummies}} can be called internally
#' @param dummies a named list with elements \code{actor} and \code{partner},
#'   each a character vector of dummy column names already present in
#'   \code{data}. If \code{NULL} (default), \code{\link{createDummies}} is
#'   called internally using \code{actor.id} and \code{part.id}
#' @param location.id character or \code{NULL}; optional higher-level
#'   clustering variable (e.g. course, site, cohort). Default \code{NULL}
#' @param zero.rho logical; fix actor-partner correlation at zero. Default
#'   \code{FALSE}
#' @param zero.actor logical; remove actor random effect
#'   (\eqn{\sigma_A^2 = \sigma_{AP} = 0}). Default \code{FALSE}
#' @param zero.partner logical; remove partner random effect
#'   (\eqn{\sigma_P^2 = 0}). Default \code{FALSE}
#' @param weight.partners logical; weight each partner dummy by
#'   \eqn{1/(group\_size - 1)} so that \eqn{\sigma_P^2} is the variance of
#'   the average partner contribution. Default \code{TRUE}
#' @param fixed.effects character vector or \code{NULL}; additional fixed
#'   effect predictor column names. Default \code{NULL}
#' @param start named numeric vector or \code{NULL}; optional starting values
#'   for the optimizer. Default \code{NULL}
#' @param maxit integer; maximum optimizer iterations. Default \code{200000}
#' @param stage1.maxit integer; maximum iterations for stage 1 Nelder-Mead.
#'   Default \code{1000}
#' @param stage2.maxit integer; maximum iterations for stage 2 Nelder-Mead.
#'   Default \code{1000}
#' @param tol numeric; convergence tolerance. Default \code{1e-10}
#' @param se.method character; \code{"hessian"} (default) computes standard
#'   errors via numerical Hessian and delta method; \code{"none"} skips SE
#'   computation
#' @param rho.boundary numeric; \eqn{|\rho|} threshold above which a boundary
#'   warning is issued and the SE for the AP covariance is suppressed. Default
#'   \code{0.99}
#' @param optimizer character; \code{"L-BFGS-B"} (default, three-stage) or
#'   \code{"Nelder-Mead"} (two-stage fallback)
#' @param verbose logical; print progress during optimization. Default
#'   \code{FALSE}
#' @param verbose.every integer; print every this many function evaluations
#'   when \code{verbose = TRUE}. Default \code{50}
#'
#' @return an object of class \code{"cpsrm"}, a list containing:
#' \describe{
#'   \item{\code{estimates}}{data frame of variance component estimates, SEs,
#'     and z-statistics}
#'   \item{\code{fixed.effects}}{list with \code{estimate} and \code{se} for
#'     fixed effects}
#'   \item{\code{correlations}}{named vector with actor-partner rho and ICCs}
#'   \item{\code{fit}}{named vector with REML log-likelihood, AIC, BIC, and
#'     sample size counts}
#'   \item{\code{convergence}}{optimizer convergence code (0 = success)}
#'   \item{\code{call}}{the matched call}
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Build dummies first, then pass explicitly
#' d <- createDummies(
#'   group.id = "groupId", act.id = "actId", part.id = "partId",
#'   d = sampleDyadData[sampleDyadData$timeId == 1, ],
#'   merge.original = TRUE
#' )
#' # Note: cpsrmRun requires a co-partner data structure (one actor, two
#' # partners per row). The example below illustrates the interface only.
#' # See package documentation for data preparation details.
#' }
cpsrmRun <- function(dv,
                     actor.id,
                     group.id,
                     data,
                     part.id         = NULL,
                     dummies         = NULL,
                     location.id     = NULL,
                     zero.rho        = FALSE,
                     zero.actor      = FALSE,
                     zero.partner    = FALSE,
                     weight.partners = TRUE,
                     fixed.effects   = NULL,
                     start           = NULL,
                     maxit           = 200000L,
                     stage1.maxit    = 1000L,
                     stage2.maxit    = 1000L,
                     tol             = 1e-10,
                     se.method       = c("hessian", "none"),
                     rho.boundary    = 0.99,
                     optimizer       = c("L-BFGS-B", "Nelder-Mead"),
                     verbose         = FALSE,
                     verbose.every   = 50L) {

  cl          <- match.call()
  se.method   <- match.arg(se.method)
  optimizer   <- match.arg(optimizer)
  use_loc     <- !is.null(location.id)

  # ------------------------------------------------------------------
  # 1. Resolve dummy columns
  # ------------------------------------------------------------------
  if (is.null(dummies)) {
    if (is.null(part.id))
      stop("'part.id' must be provided when 'dummies' is NULL so that ",
           "createDummies() can be called internally.")
    data <- createDummies(
      group.id       = group.id,
      act.id         = actor.id,
      part.id        = part.id,
      d              = data,
      include.self   = FALSE,
      merge.original = TRUE
    )
    max_size      <- max(data$pdSRM_act_num)
    actor.dummies  <- paste0("a", seq_len(max_size))
    partner.dummies <- paste0("p", seq_len(max_size))
  } else {
    if (!is.list(dummies) || is.null(dummies$actor) || is.null(dummies$partner))
      stop("'dummies' must be a named list with elements 'actor' and 'partner'.")
    actor.dummies   <- dummies$actor
    partner.dummies <- dummies$partner
  }

  # ------------------------------------------------------------------
  # 2. Validate inputs
  # ------------------------------------------------------------------
  stopifnot(is.data.frame(data))
  required_cols <- c(dv, actor.id, group.id, actor.dummies, partner.dummies,
                     fixed.effects, location.id)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Columns not found in 'data': ", paste(missing_cols, collapse = ", "))
  if (length(actor.dummies) != length(partner.dummies))
    stop("'actor.dummies' and 'partner.dummies' must have the same length.")

  # ------------------------------------------------------------------
  # 3. Extract data
  # ------------------------------------------------------------------
  y  <- as.numeric(data[[dv]])
  n  <- length(y)
  p  <- length(actor.dummies)

  Za <- as.matrix(data[, actor.dummies,   drop = FALSE])
  storage.mode(Za) <- "double"
  Zp <- as.matrix(data[, partner.dummies, drop = FALSE])
  storage.mode(Zp) <- "double"

  # Multiple membership weighting for partner dummies
  if (weight.partners) {
    p_rowsums <- rowSums(Zp)
    if (any(p_rowsums == 0))
      stop("Some rows have all-zero partner dummies. Check partner dummies.")
    Zp <- Zp / p_rowsums
  }

  grp_factor <- factor(data[[group.id]])
  n_groups   <- nlevels(grp_factor)
  Zg         <- stats::model.matrix(~ grp_factor - 1)

  if (use_loc) {
    loc_factor <- factor(data[[location.id]])
    n_locs     <- nlevels(loc_factor)
    Zl         <- stats::model.matrix(~ loc_factor - 1)
  } else {
    n_locs <- 0L
  }

  if (is.null(fixed.effects)) {
    X <- matrix(1, nrow = n, ncol = 1)
    colnames(X) <- "(Intercept)"
  } else {
    X <- stats::model.matrix(~ ., data = data[, fixed.effects, drop = FALSE])
  }
  p_fixed   <- ncol(X)
  n_persons <- p

  # Sanity checks
  if (any(abs(rowSums(Za) - 1) > 1e-9))
    warning("Some actor dummy rows do not sum to 1. Check actor dummies.")
  if (weight.partners) {
    if (any(abs(rowSums(Zp) - 1) > 1e-9))
      warning("After weighting, some partner dummy rows do not sum to 1.")
  } else {
    if (length(unique(round(rowSums(Zp), 6))) > 1)
      warning("Partner dummy row sums not constant. Check partner dummies.")
  }

  if (!requireNamespace("Matrix", quietly = TRUE))
    stop("The 'Matrix' package is required. ",
         "Install with install.packages('Matrix').")

  # ------------------------------------------------------------------
  # 4. Woodbury pre-computation (done ONCE)
  # ------------------------------------------------------------------
  if (zero.actor && zero.partner) {
    Z_mat <- if (use_loc) cbind(Zg, Zl) else Zg
    a_idx <- integer(0)
    b_idx <- integer(0)
    g_idx <- seq_len(n_groups)
    l_idx <- if (use_loc) n_groups + seq_len(n_locs) else integer(0)
  } else if (zero.actor) {
    Z_mat <- if (use_loc) cbind(Zp, Zg, Zl) else cbind(Zp, Zg)
    a_idx <- integer(0)
    b_idx <- seq_len(p)
    g_idx <- p + seq_len(n_groups)
    l_idx <- if (use_loc) p + n_groups + seq_len(n_locs) else integer(0)
  } else if (zero.partner) {
    Z_mat <- if (use_loc) cbind(Za, Zg, Zl) else cbind(Za, Zg)
    a_idx <- seq_len(p)
    b_idx <- integer(0)
    g_idx <- p + seq_len(n_groups)
    l_idx <- if (use_loc) p + n_groups + seq_len(n_locs) else integer(0)
  } else {
    Z_mat <- if (use_loc) cbind(Za, Zp, Zg, Zl) else cbind(Za, Zp, Zg)
    a_idx <- seq_len(p)
    b_idx <- p + seq_len(p)
    g_idx <- 2*p + seq_len(n_groups)
    l_idx <- if (use_loc) 2*p + n_groups + seq_len(n_locs) else integer(0)
  }
  q <- ncol(Z_mat)

  if (verbose) cat(sprintf("Pre-computing ZtZ (%d x %d)...\n", q, q))

  ZtZ_ds  <- crossprod(Z_mat)
  Zty     <- as.numeric(crossprod(Z_mat, y))
  ZtX_pre <- crossprod(Z_mat, X)

  # ------------------------------------------------------------------
  # 5. Number of parameters
  # ------------------------------------------------------------------
  n_theta <- if (zero.actor && zero.partner) {
    if (use_loc) 3L else 2L
  } else if (zero.actor || zero.partner) {
    if (use_loc) 4L else 3L
  } else if (zero.rho) {
    if (use_loc) 5L else 4L
  } else {
    if (use_loc) 6L else 5L
  }

  # ------------------------------------------------------------------
  # 6. REML negative log-likelihood (Woodbury)
  # ------------------------------------------------------------------
  .call_count <- 0L
  .t_start    <- proc.time()["elapsed"]
  .in_hessian <- FALSE

  reml_negloglik <- function(theta) {
    .call_count <<- .call_count + 1L

    if (zero.actor && zero.partner) {
      sA <- 0; sP <- 0; rho <- 0; sAP <- 0
      sg <- exp(theta[1])
      sl <- if (use_loc) exp(theta[2]) else 0
      se <- exp(theta[n_theta])
    } else if (zero.actor) {
      sA <- 0; rho <- 0; sAP <- 0
      sP <- exp(theta[1]); sg <- exp(theta[2])
      sl <- if (use_loc) exp(theta[3]) else 0
      se <- exp(theta[n_theta])
    } else if (zero.partner) {
      sP <- 0; rho <- 0; sAP <- 0
      sA <- exp(theta[1]); sg <- exp(theta[2])
      sl <- if (use_loc) exp(theta[3]) else 0
      se <- exp(theta[n_theta])
    } else if (zero.rho) {
      sA <- exp(theta[1]); sP <- exp(theta[2])
      sg <- exp(theta[3])
      sl <- if (use_loc) exp(theta[4]) else 0
      se <- exp(theta[n_theta])
      rho <- 0; sAP <- 0
    } else {
      sA  <- exp(theta[1]); sP <- exp(theta[2])
      rho <- tanh(max(-10, min(10, theta[3])))
      sg  <- exp(theta[4])
      sl  <- if (use_loc) exp(theta[5]) else 0
      se  <- exp(theta[n_theta])
      sAP <- rho * sA * sP
    }
    se2 <- se^2

    M <- ZtZ_ds / se2

    if (zero.actor && zero.partner) {
      log_det_D <- n_groups * log(sg^2)
      if (use_loc) log_det_D <- log_det_D + n_locs * log(sl^2)
    } else if (zero.actor) {
      diag(M)[b_idx] <- diag(M)[b_idx] + 1/sP^2
      log_det_D <- p * log(sP^2) + n_groups * log(sg^2)
      if (use_loc) log_det_D <- log_det_D + n_locs * log(sl^2)
    } else if (zero.partner) {
      diag(M)[a_idx] <- diag(M)[a_idx] + 1/sA^2
      log_det_D <- p * log(sA^2) + n_groups * log(sg^2)
      if (use_loc) log_det_D <- log_det_D + n_locs * log(sl^2)
    } else if (zero.rho) {
      diag(M)[a_idx] <- diag(M)[a_idx] + 1/sA^2
      diag(M)[b_idx] <- diag(M)[b_idx] + 1/sP^2
      log_det_D <- p * log(sA^2) + p * log(sP^2) + n_groups * log(sg^2)
      if (use_loc) log_det_D <- log_det_D + n_locs * log(sl^2)
    } else {
      det_tau <- sA^2 * sP^2 - sAP^2
      if (det_tau < 1e-12) return(1e10)
      diag(M)[a_idx] <- diag(M)[a_idx] + sP^2/det_tau
      diag(M)[b_idx] <- diag(M)[b_idx] + sA^2/det_tau
      ap_off <- -sAP/det_tau
      for (i in seq_len(p)) {
        M[a_idx[i], b_idx[i]] <- M[a_idx[i], b_idx[i]] + ap_off
        M[b_idx[i], a_idx[i]] <- M[b_idx[i], a_idx[i]] + ap_off
      }
      log_det_D <- p * log(det_tau) + n_groups * log(sg^2)
      if (use_loc) log_det_D <- log_det_D + n_locs * log(sl^2)
    }
    diag(M)[g_idx] <- diag(M)[g_idx] + 1/sg^2
    if (use_loc) diag(M)[l_idx] <- diag(M)[l_idx] + 1/sl^2

    L_try <- tryCatch(chol(M), error = function(e) NULL)
    if (is.null(L_try)) return(1e10)
    log_det_M <- 2 * sum(log(diag(L_try)))
    log_det_V <- log_det_M + log_det_D + n * log(se2)

    rhs_all <- cbind(Zty, ZtX_pre) / se2
    sol_all  <- backsolve(L_try, forwardsolve(t(L_try), rhs_all))
    Viy      <- y / se2 - (Z_mat %*% sol_all[, 1]) / se2
    ViX      <- X / se2 - (Z_mat %*% sol_all[, -1, drop = FALSE]) / se2

    XtViX <- crossprod(X, ViX)
    XtViy <- crossprod(X, Viy)
    beta  <- tryCatch(as.numeric(solve(XtViX, XtViy)), error = function(e) NULL)
    if (is.null(beta)) return(1e10)
    log_det_XtViX <- as.numeric(determinant(XtViX, logarithm = TRUE)$modulus)
    if (!is.finite(log_det_XtViX)) return(1e10)
    r <- y - as.numeric(X %*% beta)

    Ztr   <- crossprod(Z_mat, r)
    sol_r <- backsolve(L_try, forwardsolve(t(L_try), Ztr / se2))
    Vir   <- r / se2 - (Z_mat %*% sol_r) / se2
    quad  <- as.numeric(crossprod(r, Vir))

    val <- 0.5 * ((n - p_fixed) * log(2*pi) + log_det_V + log_det_XtViX + quad)

    if (verbose && !.in_hessian &&
        .call_count %% as.integer(verbose.every) == 0L) {
      elapsed <- round((proc.time()["elapsed"] - .t_start) / 60, 1)
      if (zero.actor)
        cat(sprintf(
          "  [eval %4d | %5.1f min]  sP=%.4f  sg=%.4f%s  se=%.4f  negLL=%.2f\n",
          .call_count, elapsed, sP, sg,
          if (use_loc) sprintf("  sl=%.4f", sl) else "", se, val))
      else if (zero.rho)
        cat(sprintf(
          "  [eval %4d | %5.1f min]  sA=%.4f  sP=%.4f  sg=%.4f%s  se=%.4f  negLL=%.2f\n",
          .call_count, elapsed, sA, sP, sg,
          if (use_loc) sprintf("  sl=%.4f", sl) else "", se, val))
      else
        cat(sprintf(
          "  [eval %4d | %5.1f min]  sA=%.4f  sP=%.4f  rho=%+.3f  sg=%.4f%s  se=%.4f  negLL=%.2f\n",
          .call_count, elapsed, sA, sP, rho, sg,
          if (use_loc) sprintf("  sl=%.4f", sl) else "", se, val))
    }
    val
  }

  # ------------------------------------------------------------------
  # 7. Starting values
  # ------------------------------------------------------------------
  sd_y <- stats::sd(y, na.rm = TRUE)
  if (is.null(start)) {
    if (zero.actor && zero.partner) {
      start <- c(log_sg = log(sd_y * 0.15))
      if (use_loc) start <- c(start, log_sl = log(sd_y * 0.5))
      start <- c(start, log_se = log(sd_y * 0.7))
    } else if (zero.actor) {
      start <- c(log_sP = log(sd_y * 0.25), log_sg = log(sd_y * 0.15))
      if (use_loc) start <- c(start, log_sl = log(sd_y * 0.5))
      start <- c(start, log_se = log(sd_y * 0.7))
    } else if (zero.partner) {
      start <- c(log_sA = log(sd_y * 0.4), log_sg = log(sd_y * 0.15))
      if (use_loc) start <- c(start, log_sl = log(sd_y * 0.5))
      start <- c(start, log_se = log(sd_y * 0.7))
    } else if (zero.rho) {
      start <- c(log_sA = log(sd_y * 0.4), log_sP = log(sd_y * 0.25),
                 log_sg = log(sd_y * 0.15))
      if (use_loc) start <- c(start, log_sl = log(sd_y * 0.5))
      start <- c(start, log_se = log(sd_y * 0.7))
    } else {
      start <- c(log_sA = log(sd_y * 0.4), log_sP = log(sd_y * 0.25),
                 atanh_rho = 0, log_sg = log(sd_y * 0.15))
      if (use_loc) start <- c(start, log_sl = log(sd_y * 0.5))
      start <- c(start, log_se = log(sd_y * 0.7))
    }
  }

  # ------------------------------------------------------------------
  # 8. Optimization
  # ------------------------------------------------------------------
  if (optimizer == "L-BFGS-B") {
    if (verbose) cat("Optimization stage 1 (Nelder-Mead, coarse)...\n")
    opt0 <- stats::optim(start, reml_negloglik, method = "Nelder-Mead",
                         control = list(maxit = 500L, reltol = 1e-4))
    if (verbose) cat("Optimization stage 2 (Nelder-Mead, medium)...\n")
    opt1 <- stats::optim(opt0$par, reml_negloglik, method = "Nelder-Mead",
                         control = list(maxit = 500L, reltol = 1e-7))
    if (verbose) cat("Optimization stage 3 (L-BFGS-B, fine)...\n")
    opt2 <- tryCatch(
      stats::optim(opt1$par, reml_negloglik, method = "L-BFGS-B",
                   lower = rep(-8, n_theta), upper = rep(8, n_theta),
                   control = list(maxit = maxit, factr = 1e3,
                                  pgtol = tol * 0.001)),
      error = function(e) {
        if (verbose) cat("L-BFGS-B failed:", conditionMessage(e),
                         "-- using Nelder-Mead result.\n")
        opt1
      }
    )
    if (opt2$value > opt1$value) opt2 <- opt1
  } else {
    if (verbose) cat("Optimization stage 1 (Nelder-Mead)...\n")
    opt1 <- stats::optim(start, reml_negloglik, method = "Nelder-Mead",
                         control = list(maxit = stage1.maxit, reltol = tol))
    if (verbose) cat("Optimization stage 2 (Nelder-Mead)...\n")
    opt2 <- stats::optim(opt1$par, reml_negloglik, method = "Nelder-Mead",
                         control = list(maxit = stage2.maxit,
                                        reltol = tol * 0.001))
  }

  theta_hat <- opt2$par
  conv      <- opt2$convergence

  if (verbose) {
    if (conv == 0) {
      cat(sprintf("Optimizer converged successfully (%d total evaluations).\n",
                  .call_count))
    } else {
      cat(sprintf(
        "WARNING: Optimizer did not converge (code = %d, %d evaluations).\n",
        conv, .call_count))
    }
  }

  # ------------------------------------------------------------------
  # 9. Back-transform
  # ------------------------------------------------------------------
  if (zero.actor && zero.partner) {
    sA_hat <- 0; sP_hat <- 0; rho_hat <- 0
    sg_hat <- exp(theta_hat[1])
    sl_hat <- if (use_loc) exp(theta_hat[2]) else NA_real_
    se_hat <- exp(theta_hat[n_theta])
  } else if (zero.actor) {
    sA_hat <- 0; rho_hat <- 0
    sP_hat <- exp(theta_hat[1]); sg_hat <- exp(theta_hat[2])
    sl_hat <- if (use_loc) exp(theta_hat[3]) else NA_real_
    se_hat <- exp(theta_hat[n_theta])
  } else if (zero.partner) {
    sP_hat <- 0; rho_hat <- 0
    sA_hat <- exp(theta_hat[1]); sg_hat <- exp(theta_hat[2])
    sl_hat <- if (use_loc) exp(theta_hat[3]) else NA_real_
    se_hat <- exp(theta_hat[n_theta])
  } else if (zero.rho) {
    sA_hat <- exp(theta_hat[1]); sP_hat <- exp(theta_hat[2])
    rho_hat <- 0;                 sg_hat <- exp(theta_hat[3])
    sl_hat  <- if (use_loc) exp(theta_hat[4]) else NA_real_
    se_hat  <- exp(theta_hat[n_theta])
  } else {
    sA_hat  <- exp(theta_hat[1]); sP_hat <- exp(theta_hat[2])
    rho_hat <- tanh(max(-10, min(10, theta_hat[3])))
    sg_hat  <- exp(theta_hat[4])
    sl_hat  <- if (use_loc) exp(theta_hat[5]) else NA_real_
    se_hat  <- exp(theta_hat[n_theta])
  }
  sAP_hat <- rho_hat * sA_hat * sP_hat
  var_A   <- sA_hat^2; var_P <- sP_hat^2
  var_g   <- sg_hat^2; var_e <- se_hat^2
  var_l   <- if (use_loc) sl_hat^2 else NA_real_
  rho_boundary_hit <- !zero.rho && !zero.actor && abs(rho_hat) >= rho.boundary

  # ------------------------------------------------------------------
  # 10. GLS fixed effects at final parameters
  # ------------------------------------------------------------------
  se2_h <- se_hat^2
  M_h   <- ZtZ_ds / se2_h
  if (zero.actor && zero.partner) {
    # no actor or partner blocks
  } else if (zero.actor) {
    diag(M_h)[b_idx] <- diag(M_h)[b_idx] + 1/var_P
  } else if (zero.partner) {
    diag(M_h)[a_idx] <- diag(M_h)[a_idx] + 1/var_A
  } else if (zero.rho) {
    diag(M_h)[a_idx] <- diag(M_h)[a_idx] + 1/var_A
    diag(M_h)[b_idx] <- diag(M_h)[b_idx] + 1/var_P
  } else {
    dt <- var_A * var_P - sAP_hat^2
    diag(M_h)[a_idx] <- diag(M_h)[a_idx] + var_P/dt
    diag(M_h)[b_idx] <- diag(M_h)[b_idx] + var_A/dt
    ap_off <- -sAP_hat/dt
    for (i in seq_len(p)) {
      M_h[a_idx[i], b_idx[i]] <- M_h[a_idx[i], b_idx[i]] + ap_off
      M_h[b_idx[i], a_idx[i]] <- M_h[b_idx[i], a_idx[i]] + ap_off
    }
  }
  diag(M_h)[g_idx] <- diag(M_h)[g_idx] + 1/var_g
  if (use_loc) diag(M_h)[l_idx] <- diag(M_h)[l_idx] + 1/var_l
  L_h <- chol(M_h)

  sol_y   <- backsolve(L_h, forwardsolve(t(L_h), Zty/se2_h))
  Viy_h   <- y/se2_h - (Z_mat %*% sol_y)/se2_h
  ZtX     <- crossprod(Z_mat, X)
  sol_X   <- backsolve(L_h, forwardsolve(t(L_h), ZtX/se2_h))
  ViX_h   <- X/se2_h - (Z_mat %*% sol_X)/se2_h
  XtViX_h <- crossprod(X, ViX_h)
  XtViy_h <- crossprod(X, Viy_h)
  beta_hat <- as.numeric(solve(XtViX_h, XtViy_h))
  se_beta  <- sqrt(diag(solve(XtViX_h)))
  names(beta_hat) <- colnames(X)
  names(se_beta)  <- colnames(X)

  # ------------------------------------------------------------------
  # 11. Standard errors (numerical Hessian + delta method)
  # ------------------------------------------------------------------
  se_estimates <- rep(NA_real_, n_theta)

  if (se.method == "hessian" && !rho_boundary_hit) {
    .in_hessian <- TRUE
    if (verbose) {
      n_hess_evals <- if (requireNamespace("numDeriv", quietly = TRUE)) {
        n_theta * (n_theta + 1) * 2
      } else {
        n_theta^2 * 4
      }
      cat(sprintf("Computing Hessian (~%d evaluations)...\n", n_hess_evals))
    }
    H <- if (requireNamespace("numDeriv", quietly = TRUE)) {
      tryCatch(numDeriv::hessian(reml_negloglik, theta_hat),
               error = function(e) NULL)
    } else {
      eps  <- 1e-4
      H_fd <- matrix(0, n_theta, n_theta)
      for (i in seq_len(n_theta)) for (j in seq_len(n_theta)) {
        tp  <- theta_hat; tp[i]  <- tp[i]  + eps; tp[j]  <- tp[j]  + eps
        tm  <- theta_hat; tm[i]  <- tm[i]  - eps; tm[j]  <- tm[j]  - eps
        tpm <- theta_hat; tpm[i] <- tpm[i] + eps; tpm[j] <- tpm[j] - eps
        tmp <- theta_hat; tmp[i] <- tmp[i] - eps; tmp[j] <- tmp[j] + eps
        H_fd[i, j] <- (reml_negloglik(tp) - reml_negloglik(tpm) -
                         reml_negloglik(tmp) + reml_negloglik(tm)) / (4*eps^2)
      }
      H_fd
    }
    if (verbose) cat("Hessian complete.\n")
    if (!is.null(H)) {
      cov_theta <- tryCatch(solve(H), error = function(e) NULL)
      if (!is.null(cov_theta)) {
        J <- matrix(0, n_theta, n_theta)
        if (zero.actor) {
          J[1, 1] <- 2*var_P; J[2, 2] <- 2*var_g
          if (use_loc) { J[3, 3] <- 2*var_l; J[n_theta, n_theta] <- 2*var_e
          } else        { J[n_theta, n_theta] <- 2*var_e }
        } else if (zero.rho) {
          J[1, 1] <- 2*var_A; J[2, 2] <- 2*var_P; J[3, 3] <- 2*var_g
          if (use_loc) { J[4, 4] <- 2*var_l; J[n_theta, n_theta] <- 2*var_e
          } else        { J[n_theta, n_theta] <- 2*var_e }
        } else {
          J[1, 1] <- 2*var_A; J[2, 2] <- 2*var_P
          J[3, 1] <- sAP_hat; J[3, 2] <- sAP_hat
          J[3, 3] <- (1 - rho_hat^2) * sA_hat * sP_hat
          J[4, 4] <- 2*var_g
          if (use_loc) { J[5, 5] <- 2*var_l; J[n_theta, n_theta] <- 2*var_e
          } else        { J[n_theta, n_theta] <- 2*var_e }
        }
        cov_nat      <- J %*% cov_theta %*% t(J)
        se_estimates <- sqrt(pmax(0, diag(cov_nat)))
      }
    }
  }
  if (rho_boundary_hit) se_estimates[3] <- NA_real_

  # ------------------------------------------------------------------
  # 12. Fit statistics and output
  # ------------------------------------------------------------------
  reml_ll <- -opt2$value
  aic     <- -2*reml_ll + 2*n_theta
  bic     <- -2*reml_ll + log(n)*n_theta

  if (zero.actor && zero.partner) {
    comp_names <- c("Group variance (sigma_g^2)")
    nat_params <- c(var_g)
  } else if (zero.actor) {
    comp_names <- c("Partner variance (sigma_P^2)", "Group variance (sigma_g^2)")
    nat_params <- c(var_P, var_g)
  } else if (zero.partner) {
    comp_names <- c("Actor variance (sigma_A^2)", "Group variance (sigma_g^2)")
    nat_params <- c(var_A, var_g)
  } else if (zero.rho) {
    comp_names <- c("Actor variance (sigma_A^2)", "Partner variance (sigma_P^2)",
                    "Group variance (sigma_g^2)")
    nat_params <- c(var_A, var_P, var_g)
  } else {
    comp_names <- c("Actor variance (sigma_A^2)", "Partner variance (sigma_P^2)",
                    "Actor-partner covariance (sigma_AP)",
                    "Group variance (sigma_g^2)")
    nat_params <- c(var_A, var_P, sAP_hat, var_g)
  }
  if (use_loc) {
    comp_names <- c(comp_names, "Location variance (sigma_l^2)")
    nat_params <- c(nat_params, var_l)
  }
  comp_names <- c(comp_names, "Residual variance (sigma_e^2)")
  nat_params <- c(nat_params, var_e)

  estimates_df <- data.frame(
    component = comp_names,
    estimate  = nat_params,
    se        = se_estimates,
    z         = nat_params / se_estimates,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  var_total <- var_A + var_P + var_g + var_e
  if (use_loc) var_total <- var_total + var_l

  correlations <- c(
    rho_AP      = if (zero.rho || zero.actor) 0
                  else if (is.finite(rho_hat)) rho_hat else NA_real_,
    ICC_group   = as.numeric(var_g / var_total),
    ICC_location = as.numeric(if (use_loc) var_l / var_total else NA_real_)
  )
  fit_stats <- c(
    reml_loglik = reml_ll, AIC = aic, BIC = bic, n_obs = n,
    n_groups    = n_groups,
    n_locations = if (use_loc) n_locs else NA_real_,
    n_persons   = n_persons
  )

  result <- structure(
    list(
      estimates        = estimates_df,
      fixed.effects    = list(estimate = beta_hat, se = se_beta),
      correlations     = correlations,
      fit              = fit_stats,
      convergence      = conv,
      rho_boundary_hit = rho_boundary_hit,
      zero_rho         = zero.rho,
      zero_actor       = zero.actor,
      zero_partner     = zero.partner,
      weight_partners  = weight.partners,
      use_location     = use_loc,
      optimizer        = optimizer,
      dv_name          = dv,
      n_evals          = .call_count,
      optim.result     = opt2,
      call             = cl
    ),
    class = "cpsrm"
  )

  print(result)
  invisible(result)
}


# =============================================================================
# print.cpsrm / summary.cpsrm
# =============================================================================

#' Print a cpsrm Object
#'
#' @param x an object of class \code{"cpsrm"}
#' @param digits integer; number of digits to print. Default \code{4}
#' @param ... additional arguments (currently unused)
#'
#' @return \code{x}, invisibly
#' @export
print.cpsrm <- function(x, digits = 4, ...) {
  cat("\n========================================\n")
  cat(" Co-Partner Social Relations Model\n")
  cat(sprintf(" Outcome: %s\n", x$dv_name))
  if (x$zero_rho)     cat(" (Actor-partner correlation fixed at zero)\n")
  if (x$zero_actor)   cat(" (Actor variance fixed at zero)\n")
  if (x$zero_partner) cat(" (Partner variance fixed at zero)\n")
  if (x$weight_partners) {
    cat(" (Partner dummies weighted by 1/(group_size - 1))\n")
  } else {
    cat(" (Partner dummies unweighted: sigma_P^2 = variance of sum of",
        "partner effects)\n")
  }
  cat(sprintf(" Optimizer: %s (%d evaluations)\n",
              x$optimizer, as.integer(x$n_evals)))
  cat("========================================\n\n")

  if (x$convergence == 0) {
    cat("Convergence: SUCCESS\n\n")
  } else {
    cat(sprintf(
      "Convergence: WARNING - optimizer did not converge (code = %d)\n\n",
      x$convergence))
  }

  if (!x$zero_rho && x$rho_boundary_hit)
    cat("NOTE: |rho_AP| near 1 (boundary). SE for AP covariance suppressed.\n\n")

  fe <- x$fixed.effects
  cat("Fixed Effects:\n")
  for (i in seq_along(fe$estimate))
    cat(sprintf("  %-20s = %8.4f  (SE = %.4f)\n",
                names(fe$estimate)[i], fe$estimate[i], fe$se[i]))

  cat("\nVariance Components:\n")
  est      <- x$estimates
  var_rows <- !grepl("covariance", est$component)
  var_total <- sum(est$estimate[var_rows])
  for (i in seq_len(nrow(est))) {
    se_str <- if (!is.na(est$se[i])) {
      sprintf("  SE = %7.4f", est$se[i])
    } else {
      "              "
    }
    z_str <- if (!is.na(est$z[i]) && is.finite(est$z[i])) {
      sprintf("  z = %6.2f", est$z[i])
    } else {
      ""
    }
    pct_str <- if (var_rows[i]) {
      sprintf("  (%5.1f%%)", 100 * est$estimate[i] / var_total)
    } else {
      ""
    }
    cat(sprintf("  %-40s  %9.5f%s%s%s\n",
                est$component[i], est$estimate[i], se_str, z_str, pct_str))
  }

  cat("\nVariance Decomposition (% of total):\n")
  for (i in which(var_rows))
    cat(sprintf("  %-42s  %5.1f%%\n",
                est$component[i], 100 * est$estimate[i] / var_total))

  cat("\nCorrelations:\n")
  rho <- as.numeric(x$correlations["rho_AP"])
  cat(sprintf("  Actor-partner rho (generalized reciprocity) = %s\n",
              if (x$zero_rho || x$zero_actor) "0 (fixed)"
              else if (is.finite(rho)) sprintf("%.4f", rho)
              else "NA (boundary)"))
  cat(sprintf("  Group ICC                                   = %.4f\n",
              as.numeric(x$correlations["ICC_group"])))
  if (x$use_location)
    cat(sprintf("  Location ICC                                = %.4f\n",
                as.numeric(x$correlations["ICC_location"])))

  cat("\nFit Statistics (REML):\n")
  cat(sprintf("  -2 * REML log-lik = %.3f\n",  -2 * x$fit["reml_loglik"]))
  cat(sprintf("  AIC               = %.3f\n",   x$fit["AIC"]))
  cat(sprintf("  BIC               = %.3f\n",   x$fit["BIC"]))
  cat(sprintf("  N (observations)  = %d\n",     as.integer(x$fit["n_obs"])))
  cat(sprintf("  N (groups)        = %d\n",     as.integer(x$fit["n_groups"])))
  if (x$use_location)
    cat(sprintf("  N (locations)     = %d\n",
                as.integer(x$fit["n_locations"])))
  cat(sprintf("  N (persons)       = %d\n",     as.integer(x$fit["n_persons"])))
  cat("\n")
  invisible(x)
}

#' Summarize a cpsrm Object
#'
#' @param object an object of class \code{"cpsrm"}
#' @param ... additional arguments passed to \code{\link{print.cpsrm}}
#'
#' @return \code{object}, invisibly
#' @export
summary.cpsrm <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
