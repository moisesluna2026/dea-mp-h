# =============================================================================
# DEA-MP/H: core functions
#
# Notation follows the article:
#   X      resources of each household (n = 6 columns), the DEA inputs
#   Y      demands of each household (m = 2 columns), the DEA outputs
#   score  categorical deprivation score S_i, in [0, d]
#   tau    comparison level of each household, tau = delta * floor(S / delta)
#   F_k    frontier of level k, computed once on the frozen reference R
#   theta  score of an evaluated household against F_k*(o), model (1)
# =============================================================================

RESOURCES <- c("num_renda_total", "n_alfabetizados", "n_adultos_educ_media_ou_mais",
               "n_ocupados_formais", "n_dormitorios", "n_banheiros")
DEMANDS   <- c("n_moradores", "n_adultos_em_idade_ativa")

log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")

# lapply in parallel (forked processes; sequential on Windows or when CORES = 1).
par_lapply <- function(X, FUN) {
  if (CORES <= 1L || .Platform$OS.type == "windows") return(lapply(X, FUN))
  r <- parallel::mclapply(X, FUN, mc.cores = CORES, mc.preschedule = FALSE)
  bad <- vapply(r, inherits, logical(1), "try-error")
  if (any(bad)) stop("Parallel task failed: ", as.character(r[[which(bad)[1]]]))
  r
}

# -----------------------------------------------------------------------------
# Data matrices
# Income: values <= 0 become R$ 1 and income is expressed in thousands of reais.
# Any other zero resource or demand becomes `zero_constant`, because the CCR model
# requires strictly positive data (Cooper et al., 2007).
# -----------------------------------------------------------------------------
prepare_matrices <- function(d, zero_constant = PARAM$zero_constant, delta = PARAM$delta) {
  X <- as.matrix(d[, RESOURCES, with = FALSE])
  X[, 1] <- ifelse(X[, 1] <= 0, 1, X[, 1]) / 1000
  X[X <= 0] <- zero_constant
  Y <- as.matrix(d[, DEMANDS, with = FALSE])
  Y[Y <= 0] <- zero_constant
  list(X = X, Y = Y, tau = floor(d$score / delta + 1e-9) * delta)
}

# -----------------------------------------------------------------------------
# Model (1) in batch.
# For each evaluated household o and frontier F, model (1) is
#   max u'y_o  s.t.  v'x_o = 1,  u'y_f - v'x_f <= 0 (f in F),  v_r x_ro >= alpha,  u, v >= 0.
# With the change of variables p_r = v_r x_ro and q_s = u_s y_so (virtual shares),
# the normalization, the objective and the floor are the same for every household;
# only the coefficients of the frontier constraints change. This lets GLPK solve
# all households of one level in a single call (rDEA::multi_glpk_solve_LP).
# The transformation is exact; `evaluate_lpsolve` solves the original form and is
# used to verify it.
# -----------------------------------------------------------------------------
evaluate_batch <- function(XR, YR, frontier, XA, YA, alpha, chunk = 400L) {
  n <- ncol(XR); m <- ncol(YR); nf <- length(frontier)
  A   <- rbind(c(rep(0, m), rep(1, n)), matrix(0, nf, m + n))
  dir <- c("==", rep("<=", nf))
  rhs <- c(1, rep(0, nf))
  if (alpha > 0) {
    A   <- rbind(A, cbind(matrix(0, n, m), diag(n)))
    dir <- c(dir, rep(">=", n))
    rhs <- c(rhs, rep(alpha, n))
  }
  varying <- cbind(rep(1L + seq_len(nf), m + n), rep(seq_len(m + n), each = nf))
  out <- numeric(nrow(XA))
  for (start in seq.int(1, nrow(XA), by = chunk)) {
    jj <- start:min(nrow(XA), start + chunk - 1L)
    coef <- do.call(rbind, c(
      lapply(seq_len(m), function(s) outer(YR[frontier, s], 1 / YA[jj, s])),
      lapply(seq_len(n), function(r) -outer(XR[frontier, r], 1 / XA[jj, r]))))
    sol <- rDEA::multi_glpk_solve_LP(obj = c(rep(1, m), rep(0, n)), mat = A, dir = dir, rhs = rhs,
                                     max = TRUE, mmat_i = varying, mmat_val = coef)
    if (any(sol$status != 0)) stop("GLPK did not reach an optimal solution.")
    out[jj] <- sol$optimum
  }
  out
}

# Model (1) in its original multiplier form, one household at a time (lpSolve).
evaluate_lpsolve <- function(XR, YR, frontier, x_o, y_o, alpha) {
  n <- length(x_o); m <- length(y_o); nf <- length(frontier)
  A <- rbind(c(rep(0, m), x_o),
             cbind(YR[frontier, , drop = FALSE], -XR[frontier, , drop = FALSE]),
             cbind(matrix(0, n, m), diag(x_o, n)))
  r <- lpSolve::lp("max", c(y_o, rep(0, n)), A, c("=", rep("<=", nf), rep(">=", n)),
                   c(1, rep(0, nf), rep(alpha, n)))
  if (r$status != 0) stop("lpSolve did not reach an optimal solution.")
  r$objval
}

# Radial (envelope) efficiency of household i relative to the set `ref` (input orientation, CRS).
radial_efficiency <- function(X, Y, i, ref) {
  n <- ncol(X); m <- ncol(Y)
  A <- rbind(cbind(-X[i, ], t(X[ref, , drop = FALSE])), cbind(0, t(Y[ref, , drop = FALSE])))
  r <- lpSolve::lp("min", c(1, rep(0, length(ref))), A, c(rep("<=", n), rep(">=", m)),
                   c(rep(0, n), Y[i, ]))
  if (r$status != 0) stop("Envelope problem without optimal solution.")
  r$objval
}

# -----------------------------------------------------------------------------
# Input-oriented super-efficiency (Andersen and Petersen) of household i with respect
# to the set `cand` without i, radial CCR (alpha = 0). Solved in multiplier form,
#   max u'y_i  s.t.  v'x_i = 1,  u'y_j - v'x_j <= 0 (j in cand, j != i),  u, v >= 0,
# by constraint generation: the problem is solved on a small working set and every
# household of `cand` is then checked against the optimal weights (one matrix
# product); violating households are added until none is left.
# -----------------------------------------------------------------------------
super_efficiency <- function(X, Y, i, cand, start, Xc = X[cand, , drop = FALSE], Yc = Y[cand, , drop = FALSE], tol = 1e-9) {
  n <- ncol(X); m <- ncol(Y)
  self <- which(cand == i)
  working <- unique(c(start[start != i], head(cand[cand != i], 10)))
  repeat {
    A <- rbind(c(rep(0, m), X[i, ]), cbind(Y[working, , drop = FALSE], -X[working, , drop = FALSE]))
    r <- lpSolve::lp("max", c(Y[i, ], rep(0, n)), A, c("=", rep("<=", length(working))), c(1, rep(0, length(working))))
    if (r$status != 0) stop("Super-efficiency problem without optimal solution (household ", i, ").")
    u <- r$solution[seq_len(m)]; v <- r$solution[m + seq_len(n)]
    viol <- drop(Yc %*% u - Xc %*% v); viol[self] <- -Inf
    out <- which(viol > tol)
    if (!length(out)) return(r$objval)
    working <- unique(c(working, cand[head(out[order(viol[out], decreasing = TRUE)], 20)]))
  }
}

# -----------------------------------------------------------------------------
# Frontiers F_k, computed once on a single frozen reference R.
# For a given R, the frontiers are built from the most deprived level to the least
# deprived. Because the groups are nested, F_k = E(F_{k+1} U band_k): households that
# are interior in a more restricted group remain interior when the group is enlarged.
# This identity holds only while R does not change, so no household is removed
# during a pass. Inside each level, the radial frontier (alpha = 0) is found by
# constraint generation: candidates are evaluated against a working set, the most
# violating ones are added, and members that are clearly not radially efficient are
# pruned. The result does not depend on the starting set.
#
# Screening of atypical households (`screen`), in rounds:
#   1. build every frontier on the current reference R (one pass, as above);
#   2. for every group with at least min_ref reference households, compute the
#      input-oriented super-efficiency of each of its frontier households against
#      the whole active group (all households of R in the group, except itself);
#   3. remove from R, at once, every household whose super-efficiency exceeds
#      `screen` (plus `screen_tol`, so that values equal to the cut-off up to
#      rounding are kept) in at least one group;
#   4. repeat from 1 until a round removes nobody.
# Only the final R and its frontiers are kept, so every stored frontier belongs to
# the same reference, the groups stay nested and no removed household remains in a
# frontier. Households with super-efficiency above the cut-off push the frontier far
# beyond all other households (Bogetoft and Otto, 2011, Sec. 5.13). Removed
# households leave the reference but are still evaluated like any other household.
# screen = Inf switches the screening off (a single pass).
#
# `local` stores the score of each household inside its own cumulative group of R
# (the local frontier, without floor); it is NA for removed households.
# `se_frontier` lists the super-efficiency of the final frontier households of every
# screened group (empty when screen = Inf).
# -----------------------------------------------------------------------------
build_frontiers <- function(M, screen = Inf, min_ref = PARAM$min_ref, tol = 1e-7, prune_tol = 1e-6,
                            screen_tol = 1e-6) {
  X <- M$X; Y <- M$Y; tau <- M$tau
  levels_desc <- sort(unique(tau), decreasing = TRUE)
  screened <- data.table(household = integer(), level = numeric(), super_efficiency = numeric(), round = integer())

  frontier_of <- function(cand, start, t) {
    working <- unique(c(start[start %in% cand], head(cand, 10)))
    iter <- 0L
    repeat {
      iter <- iter + 1L
      if (iter > 200L) stop("Frontier construction did not converge at level ", t)
      eff <- evaluate_batch(X, Y, working, X[cand, , drop = FALSE], Y[cand, , drop = FALSE], alpha = 0)
      eff[cand %in% working] <- pmin(eff[cand %in% working], 1)
      outside <- which(eff > 1 + tol)
      if (!length(outside)) return(eff)
      # add the most violating candidates; more of them when many are outside, which
      # only shortens the search (the hull does not depend on the working set)
      n_add <- max(20L, min(500L, ceiling(0.05 * length(outside))))
      add <- cand[head(outside[order(eff[outside], decreasing = TRUE)], n_add)]
      working <- unique(c(working, add))
      # Pruning only reduces the size of the working set; it never changes the hull.
      # Households added in this iteration are kept, which rules out cycling when the
      # envelope and multiplier problems disagree at the level of solver tolerance.
      keep <- working %in% add |
        vapply(working, function(i) radial_efficiency(X, Y, i, working), 0) >= 1 - prune_tol
      working <- working[keep]
    }
  }

  # One pass on a fixed reference (logical vector `in_ref`).
  build_pass <- function(in_ref) {
    frontier <- integer(); by_level <- list(); local <- rep(NA_real_, nrow(X))
    for (t in levels_desc) {
      band <- which(tau == t & in_ref)
      cand <- c(frontier, band)
      if (length(cand)) {
        eff <- frontier_of(cand, frontier, t)
        frontier <- cand[eff >= 1 - tol]
        local[band] <- eff[match(band, cand)]
      }
      if (nrow(X) > 1e6) log_msg("  level", t, "| candidates", length(cand), "| frontier", length(frontier))
      by_level[[as.character(t)]] <- frontier
    }
    list(by_level = by_level, frontier = frontier, local = local)
  }

  in_ref <- rep(TRUE, nrow(X)); round <- 0L; se_frontier <- data.table()
  repeat {
    round <- round + 1L
    P <- build_pass(in_ref)
    if (!length(P$frontier)) stop("Screening removed every reference household")
    if (!is.finite(screen)) break
    se_frontier <- rbindlist(lapply(levels_desc, function(t) {
      group <- which(tau >= t - 1e-9 & in_ref)
      front <- P$by_level[[as.character(t)]]
      if (length(group) < min_ref || !length(front)) return(NULL)
      Xg <- X[group, , drop = FALSE]; Yg <- Y[group, , drop = FALSE]
      data.table(level = t, household = front,
                 super_efficiency = vapply(front, function(i) super_efficiency(X, Y, i, group, front, Xg, Yg), 0))
    }))
    out <- se_frontier[super_efficiency > screen + screen_tol]
    if (nrow(X) > 1e6 || nrow(out)) log_msg("  screening round", round, "| removed", uniqueN(out$household))
    if (!nrow(out)) break
    out <- out[order(-super_efficiency)][!duplicated(household)]
    screened <- rbind(screened, out[, .(household, level, super_efficiency, round = round)])
    in_ref[out$household] <- FALSE
  }
  if (any(unlist(P$by_level) %in% screened$household)) stop("A removed household remains in a stored frontier")
  list(by_level = P$by_level, frontier = P$frontier, local = P$local, tau = tau,
       in_reference = in_ref, screened = screened, screen = screen, rounds = round, se_frontier = se_frontier)
}

# Admissible level k*(o): the most restricted level compatible with the household's
# score that has at least `min_ref` reference households (screened households do not count).
admissible_level <- function(F, tau_eval, min_ref = PARAM$min_ref) {
  tops <- sort(as.numeric(names(F$by_level)))
  ref_tau <- F$tau[F$in_reference]
  size <- vapply(tops, function(tt) sum(ref_tau >= tt - 1e-9), 0)
  pick <- function(t) {
    k <- max(c(1L, which(tops <= t + 1e-9)))
    while (size[k] < min_ref && k > 1L) k <- k - 1L
    as.character(tops[k])
  }
  u <- unique(tau_eval)
  setNames(vapply(u, pick, ""), u)[as.character(tau_eval)] |> unname()
}

# Scores theta of evaluated households MA against the frozen frontiers F of reference MR.
score_households <- function(MR, F, MA, alpha = PARAM$alpha) {
  out <- rep(NA_real_, nrow(MA$X))
  lev <- admissible_level(F, MA$tau)
  for (k in unique(lev)) {
    ii <- which(lev == k)
    out[ii] <- evaluate_batch(MR$X, MR$Y, F$by_level[[k]], MA$X[ii, , drop = FALSE],
                              MA$Y[ii, , drop = FALSE], alpha)
  }
  out
}

# -----------------------------------------------------------------------------
# Aggregation and reading
# -----------------------------------------------------------------------------
truncate1 <- function(theta) pmin(theta, 1)
index_M <- function(theta, w) weighted.mean(truncate1(theta), w)
# weighted median: first value at which the cumulative weight reaches half of the total
weighted_median <- function(x, w) { o <- order(x); x[o][which(cumsum(w[o]) >= sum(w) / 2)[1]] }
anchor_median <- function(theta, w) weighted_median(truncate1(theta), w)

summarise_scores <- function(d, by, m0, kappa = PARAM$kappa) {
  d[, .(n = .N,
        M = index_M(s, w),
        T = index_M(s, w) / m0,
        H = 100 * weighted.mean(truncate1(s) >= kappa * m0, w),
        above1 = 100 * weighted.mean(s > 1 + 1e-6, w)), by = by]
}

# First-order dominance of 2010 over 2022 on [lo, hi]: G_2010(z) >= G_2022(z) for every z,
# with G(z) the weighted share of households with truncated score >= z.
# Checked at every jump of the empirical distributions, including the endpoints.
dominance <- function(d, lo = 0, hi = 1) {
  z <- d[, .(s = truncate1(s), dw = ifelse(ano == 2010, w / sum(w[ano == 2010]), -w / sum(w[ano == 2022])))]
  z <- z[, .(dw = sum(dw)), by = s][order(s)]
  z[, D := -cumsum(dw)]
  vals <- c(-sum(z[s < lo, dw]), z[s >= lo & s < hi, D], -sum(z[s < hi, dw]))
  list(dominates = min(vals) >= -1e-10, minimum = min(vals))
}

frontier_profile <- function(d) {
  data.table(n = nrow(d), nordeste = sum(d$uf %in% 21:29), norte = sum(d$uf %in% 11:17),
             moradores = mean(d$n_moradores), renda_zero_pct = 100 * mean(d$num_renda_total == 0),
             sem_banheiro_pct = 100 * mean(d$n_banheiros == 0))
}

region <- function(uf) fcase(uf %in% 11:17, "Norte", uf %in% 21:29, "Nordeste",
                             uf %in% c(31:33, 35), "Sudeste", uf %in% 41:43, "Sul", uf %in% 50:53, "Centro-Oeste")

# -----------------------------------------------------------------------------
# Categorical deprivation score S_i (six dimensions, each in [0, 1]).
# Every category variable is coded "higher = better"; the deprivation of a
# dimension is (best - observed) / (best - worst).
# -----------------------------------------------------------------------------
SCORE_SCALE <- list(educ_max = c(0, 3), cat_agua = c(1, 5), cat_esgoto = c(0, 5),
                    cat_lixo = c(0, 2), cat_parede = c(0, 5), cat_cond_ocup = c(1, 5))

add_score <- function(d) {
  for (v in names(SCORE_SCALE)) {
    s <- SCORE_SCALE[[v]]
    d[, (paste0("priv_", v)) := (s[2] - get(v)) / (s[2] - s[1])]
  }
  d[, score := rowSums(.SD), .SDcols = paste0("priv_", names(SCORE_SCALE))]
  d
}

# Eligibility. A household enters the analysis when every resource, demand and
# score dimension is observed. For comparability with the harmonized base used
# during the study, the rule also requires the two variables that were dropped
# from the final specification (persons without severe disability and the
# occupation of the household head); `eligibility_check.R` reports how many
# households this extra requirement removes.
ELIGIBILITY_VARS <- c(RESOURCES, DEMANDS, names(SCORE_SCALE), "n_sem_deficiencia_severa", "ocup_resp")
flag_eligible <- function(d) d[, completo := complete.cases(d[, ELIGIBILITY_VARS, with = FALSE])]

# -----------------------------------------------------------------------------
# Reading helpers
# -----------------------------------------------------------------------------
zip_members <- function(zip) system2("unzip", c("-Z1", shQuote(zip)), stdout = TRUE)

# Reads selected fields of a fixed-width text file stored inside a zip archive.
# `spec` is a data.table with columns var, start, length (positions as in the
# IBGE layout). Fields are cut with awk and read by data.table::fread.
read_fixed_width_zip <- function(zip, member, spec) {
  cut <- paste0("substr($0,", spec$start, ",", spec$length, ")", collapse = "\";\"")
  cmd <- sprintf("unzip -p %s %s | LC_ALL=C awk '{print %s}'", shQuote(zip), shQuote(member), cut)
  fread(cmd = cmd, sep = ";", header = FALSE, col.names = spec$var, colClasses = "character",
        na.strings = "", strip.white = TRUE, showProgress = FALSE)
}

file_sha256 <- function(path) {
  tool <- if (nzchar(Sys.which("sha256sum"))) c("sha256sum") else c("shasum", "-a", "256")
  vapply(path, function(p) strsplit(system2(tool[1], c(tool[-1], shQuote(p)), stdout = TRUE), " +")[[1]][1], "")
}

# Deterministic order used before any random draw.
sort_households <- function(d) setorderv(d, intersect(c("uf", "municipio", "id_domicilio"), names(d)))
