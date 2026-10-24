# =============================================================================
# Stage 07. Audit of the method on the municipal data (complete reference).
#  (a) Solver check: batch GLPK in virtual shares vs lpSolve in the original
#      multiplier form, 2,000 households.
#  (b) Monotonicity: one positive resource of 200 households is halved; the score
#      must not fall (alpha = 0) and must rise (alpha > 0).
#  (c) Non-interference, directed: income of 15 local-frontier households of Itabuna
#      is halved and income of 15 others is doubled (median income if zero). The local
#      frontier is re-estimated; the fixed-reference score is recomputed.
#  (d) Non-interference, random: 30 households with positive income, income halved.
#  (e) Subgroup consistency: income of 10% of Itabuna's households is halved.
#  (f) Directed worsening in other dimensions: 15 local-frontier households of
#      Itabuna lose one bathroom, 15 lose one formal job and 15 gain one resident;
#      same comparison between the local frontier and the fixed reference.
#  (g) Categorical monotonicity: every household is scored at its admissible level
#      and at the next more restricted admissible level (the move caused by a
#      worsening of the categorical score); the score must not fall. Run on the main
#      reference and on references screened with cut-offs 2 and 4, where households
#      are removed and the nesting of the frozen frontiers is put to the test.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- file.path(PATHS$output, "audit"); dir.create(O, showWarnings = FALSE)
z <- readRDS(file.path(PATHS$work, "municipal.rds")); b <- z$b; M <- z$M; F <- z$F; s <- z$s
it <- readRDS(file.path(PATHS$work, "itabuna_local.rds"))
pos <- function(x) x > PARAM$zero_constant + 1e-12
# draws k elements of x without replacement (safe when x has length 1)
pick <- function(x, k) x[sample.int(length(x), min(k, length(x)))]
subset_M <- function(MM, ii) list(X = MM$X[ii, , drop = FALSE], Y = MM$Y[ii, , drop = FALSE], tau = MM$tau[ii])

# (a) solver check
set.seed(SEEDS$solver_check)
ii <- sample.int(nrow(b), min(2000L, nrow(b))); lev <- admissible_level(F, M$tau[ii])
chk <- data.table(i = ii, glpk = s$s[ii],
                  lpsolve = vapply(seq_along(ii), function(j)
                    evaluate_lpsolve(M$X, M$Y, F$by_level[[lev[j]]], M$X[ii[j], ], M$Y[ii[j], ], PARAM$alpha), 0))
fwrite(chk, file.path(O, "solver_check.csv"))
log_msg("solver check: max abs difference", signif(max(abs(chk$glpk - chk$lpsolve)), 3))

# (b) monotonicity
set.seed(SEEDS$monotonicity)
ii <- sample.int(nrow(b), min(200L, nrow(b))); MA <- subset_M(M, ii); cut <- integer(length(ii))
for (j in seq_along(ii)) { ks <- which(pos(MA$X[j, ])); cut[j] <- ks[sample.int(length(ks), 1)]; MA$X[j, cut[j]] <- MA$X[j, cut[j]] / 2 }
mono <- rbindlist(lapply(c(0, 0.05, 0.10), function(a)
  data.table(alpha = a, resource = RESOURCES[cut],
             change = score_households(M, F, MA, a) - score_households(M, F, subset_M(M, ii), a))))
fwrite(mono, file.path(O, "monotonicity.csv"))
print(mono[, .(increased = sum(change > 1e-9), unchanged = sum(abs(change) <= 1e-9), decreased = sum(change < -1e-9),
               minimum = min(change)), by = alpha])

# (c) directed interference
base <- it$F$local; on_front <- which(base >= 1 - 1e-7)
med <- median(it$M$X[pos(it$M$X[, 1]), 1])
set.seed(SEEDS$interference_directed)
worse <- on_front[pos(it$M$X[on_front, 1])]; worse <- pick(worse, 15)
better <- pick(on_front, 15)
targets <- rbind(data.table(dom = worse, type = "income halved"), data.table(dom = better, type = "income raised"))
fixed_before_all <- s$s[it$ids]
out <- par_lapply(seq_len(nrow(targets)), function(j) {
  a <- targets$dom[j]; MM <- it$M
  MM$X[a, 1] <- if (targets$type[j] == "income halved") MM$X[a, 1] / 2 else if (!pos(MM$X[a, 1])) med else 2 * MM$X[a, 1]
  nf <- build_frontiers(MM); dl <- nf$local - base; dl[a] <- 0
  r <- data.table(type = targets$type[j], household = a,
                         others_up = sum(dl > 1e-6), others_down = sum(dl < -1e-6),
                         local_M_before = index_M(base, it$b$w), local_M_after = index_M(nf$local, it$b$w),
                         local_own_before = base[a], local_own_after = nf$local[a],
                         fixed_own_before = fixed_before_all[a],
                         fixed_own_after = score_households(M, F, subset_M(MM, a), PARAM$alpha))
  log_msg("directed interference", j, "of", nrow(targets))
  r
})
fwrite(rbindlist(out), file.path(O, "interference_directed.csv"))

# (d) random interference
set.seed(SEEDS$interference_random)
aa <- pick(which(pos(it$M$X[, 1])), 30)
fwrite(rbindlist(par_lapply(aa, function(a) {
  MM <- it$M; MM$X[a, 1] <- MM$X[a, 1] / 2; dl <- build_frontiers(MM)$local - base; dl[a] <- 0
  data.table(household = a, others_changed = sum(abs(dl) > 1e-6),
             fixed_own_change = score_households(M, F, subset_M(MM, a), PARAM$alpha) - fixed_before_all[a])
})), file.path(O, "interference_random.csv"))
log_msg("random interference done")

# (e) subgroup consistency
set.seed(SEEDS$subgroup)
aa <- pick(which(pos(it$M$X[, 1])), round(0.1 * nrow(it$b)))
MM <- it$M; MM$X[aa, 1] <- MM$X[aa, 1] / 2
local_after <- build_frontiers(MM)$local
fixed_after <- fixed_before_all; fixed_after[aa] <- score_households(M, F, subset_M(MM, aa), PARAM$alpha)
all_after <- s$s; all_after[it$ids] <- fixed_after
fwrite(data.table(local_itabuna_before = index_M(base, it$b$w), local_itabuna_after = index_M(local_after, it$b$w),
                  fixed_itabuna_before = index_M(fixed_before_all, it$b$w), fixed_itabuna_after = index_M(fixed_after, it$b$w),
                  fixed_eight_before = index_M(s$s, b$w), fixed_eight_after = index_M(all_after, b$w)),
       file.path(O, "subgroup_consistency.csv"))
# (f) directed worsening in other dimensions
set.seed(SEEDS$interference_other)
shock_other <- function(MM, a, dim) {
  if (dim == "bathroom") MM$X[a, 6] <- max(MM$X[a, 6] - 1, PARAM$zero_constant)
  if (dim == "formal_job") MM$X[a, 4] <- max(MM$X[a, 4] - 1, PARAM$zero_constant)
  if (dim == "resident") MM$Y[a, 1] <- MM$Y[a, 1] + 1
  MM
}
eligible_other <- list(bathroom = on_front[it$M$X[on_front, 6] >= 1],
                       formal_job = on_front[it$M$X[on_front, 4] >= 1],
                       resident = on_front)
targets2 <- rbindlist(lapply(names(eligible_other), function(dm) {
  ii <- pick(eligible_other[[dm]], 15)
  if (length(ii)) data.table(dimension = dm, dom = ii) else NULL
}))
out2 <- par_lapply(seq_len(nrow(targets2)), function(j) {
  a <- targets2$dom[j]; MM <- shock_other(it$M, a, targets2$dimension[j])
  nf <- build_frontiers(MM); dl <- nf$local - base; dl[a] <- 0
  data.table(dimension = targets2$dimension[j], household = a,
             others_up = sum(dl > 1e-6), others_down = sum(dl < -1e-6),
             local_M_before = index_M(base, it$b$w), local_M_after = index_M(nf$local, it$b$w),
             fixed_own_before = fixed_before_all[a],
             fixed_own_after = score_households(M, F, subset_M(MM, a), PARAM$alpha))
})
fwrite(rbindlist(out2), file.path(O, "interference_directed_other.csv"))
log_msg("directed worsening in other dimensions done")
# (g) categorical monotonicity across nested groups
categorical_monotonicity <- function(FF, label) {
  tops <- sort(as.numeric(names(FF$by_level))); ref_tau <- FF$tau[FF$in_reference]
  adm <- tops[vapply(tops, function(t) sum(ref_tau >= t - 1e-9), 0) >= PARAM$min_ref]
  rbindlist(lapply(seq_along(adm)[-length(adm)], function(k) {
    ii <- which(abs(M$tau - adm[k]) < 1e-9)
    if (!length(ii)) return(NULL)
    a  <- evaluate_batch(M$X, M$Y, FF$by_level[[as.character(adm[k])]], M$X[ii, , drop = FALSE], M$Y[ii, , drop = FALSE], PARAM$alpha)
    up <- evaluate_batch(M$X, M$Y, FF$by_level[[as.character(adm[k + 1])]], M$X[ii, , drop = FALSE], M$Y[ii, , drop = FALSE], PARAM$alpha)
    data.table(reference = label, level = adm[k], next_level = adm[k + 1], households = length(ii),
               removed_from_reference = nrow(FF$screened), falls = sum(up < a - 1e-7), largest_fall = max(0, a - up))
  }))
}
cm <- rbindlist(c(list(categorical_monotonicity(F, "main")),
                  lapply(c(2, 4), function(sc) categorical_monotonicity(build_frontiers(M, screen = sc), paste0("screen_", sc)))))
fwrite(cm, file.path(O, "categorical_monotonicity.csv"))
log_msg("categorical monotonicity done")
log_msg("stage 07 complete")
