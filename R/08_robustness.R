# =============================================================================
# Stage 08. Robustness and uncertainty.
# Municipal data:
#   - household bootstrap of M (1,000 replicates; reference and scores held fixed)
#     and of the pairwise differences between municipalities;
#   - replacement constant for zeros (0.001, 0.01, 0.1);
#   - screening of atypical households: no screening and cut-offs 2 and 4
#     (main specification: 3);
#   - alternative references: V1 without the frontier households, V2 without
#     households of more than 10 residents;
#   - 20 half-sample draws: half of each municipality x level cell forms the
#     reference, the other half is evaluated;
#   - share floor alpha in {0, 0.01, 0.025, 0.05, 0.075, 0.10, 0.15};
#   - level spacing delta in {0.25, 0.5, 1.0}.
# National data (complete reference, all households evaluated):
#   - no screening and cut-offs 2 and 4; dominance under each. Run one at a time
#     because of memory use;
#   - bootstrap of the 2010-2022 change by UF (300 replicates);
#   - pairwise dominance between UFs in 2022.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- file.path(PATHS$output, "robustness"); dir.create(O, showWarnings = FALSE)
z <- readRDS(file.path(PATHS$work, "municipal.rds")); b <- z$b; M <- z$M; F <- z$F; s <- z$s
spearman_IDHM <- function(tab) cor(tab$M, MUNICIPALITIES$IDHM[match(tab$nome, MUNICIPALITIES$nome)], method = "spearman")

# Bootstrap of municipal means
set.seed(SEEDS$bootstrap_municipal)
B <- 1000L; nm <- unique(b$nome); boot <- matrix(NA_real_, B, length(nm))
for (k in seq_along(nm)) {
  ii <- which(b$nome == nm[k]); v <- truncate1(s$s[ii]); w <- b$w[ii]
  for (j in seq_len(B)) { jj <- sample.int(length(ii), length(ii), replace = TRUE); boot[j, k] <- weighted.mean(v[jj], w[jj]) }
}
fwrite(data.table(nome = nm, M_boot = colMeans(boot), low = apply(boot, 2, quantile, .025), high = apply(boot, 2, quantile, .975)),
       file.path(O, "bootstrap_municipalities.csv"))
pairs <- list()
for (i in 1:(length(nm) - 1)) for (j in (i + 1):length(nm)) {
  dd <- boot[, i] - boot[, j]
  pairs[[length(pairs) + 1]] <- data.table(a = nm[i], b = nm[j], difference = mean(dd), low = quantile(dd, .025), high = quantile(dd, .975))
}
fwrite(rbindlist(pairs), file.path(O, "bootstrap_differences_municipalities.csv"))
log_msg("municipal bootstrap done")

# Replacement constant for zeros
fwrite(rbindlist(par_lapply(c(0.001, 0.01, 0.1), function(e) {
  MM <- prepare_matrices(b, zero_constant = e); FF <- build_frontiers(MM, screen = PARAM$screen)
  r <- data.table(nome = b$nome, w = b$w, s = score_households(MM, FF, MM, PARAM$alpha))[, .(M = index_M(s, w)), by = nome]
  log_msg("zero constant", e); r[, zero_constant := e]
})), file.path(O, "zero_constant.csv"))

# Screening and alternative references (municipal)
versions <- list(main = list(ii = seq_len(nrow(b)), screen = PARAM$screen),
                 no_screen = list(ii = seq_len(nrow(b)), screen = Inf),
                 screen_2 = list(ii = seq_len(nrow(b)), screen = 2),
                 screen_4 = list(ii = seq_len(nrow(b)), screen = 4),
                 V1_without_frontier = list(ii = setdiff(seq_len(nrow(b)), F$frontier), screen = PARAM$screen),
                 V2_up_to_10_residents = list(ii = which(b$n_moradores <= 10), screen = PARAM$screen))
alt_mun <- rbindlist(par_lapply(names(versions), function(v) {
  if (v == "main") { ss <- s$s; nf <- length(F$frontier); ns <- nrow(F$screened) } else {
    MM <- prepare_matrices(b[versions[[v]]$ii]); FF <- build_frontiers(MM, screen = versions[[v]]$screen)
    ss <- score_households(MM, FF, M, PARAM$alpha); nf <- length(FF$frontier); ns <- nrow(FF$screened)
  }
  md <- anchor_median(ss, b$w); log_msg("municipal reference", v)
  data.table(nome = b$nome, w = b$w, s = ss)[, .(M = index_M(s, w), T = index_M(s, w) / md,
                                                 H = 100 * weighted.mean(truncate1(s) >= PARAM$kappa * md, w)), by = nome][
    , `:=`(version = v, m0 = md, frontier_size = nf, screened = ns)]
}))
fwrite(alt_mun, file.path(O, "alternative_references_municipalities.csv"))
fwrite(alt_mun[, .(spearman_IDHM = spearman_IDHM(.SD), m0 = m0[1], frontier_size = frontier_size[1], screened = screened[1]), by = version],
       file.path(O, "alternative_references_municipalities_summary.csv"))

# Half-sample draws
all <- seq_len(nrow(b))
fwrite(rbindlist(par_lapply(SEEDS$half_draws, function(seed) {
  set.seed(seed); chosen <- logical(nrow(b))
  for (g in split(all, list(b$nome, M$tau), drop = TRUE)) chosen[g[sample.int(length(g), floor(length(g) / 2))]] <- TRUE
  ii <- which(chosen); jj <- which(!chosen)
  MM <- prepare_matrices(b[ii]); ss <- score_households(MM, build_frontiers(MM, screen = PARAM$screen), prepare_matrices(b[jj]), PARAM$alpha)
  log_msg("half-sample draw", seed)
  data.table(nome = b$nome[jj], w = b$w[jj], s = ss)[, .(M = index_M(s, w)), by = nome][, seed := seed]
})), file.path(O, "half_sample_draws.csv"))

# Share floor alpha (same frontiers; alpha <= 1/6 with six resources)
alpha_grid <- rbindlist(lapply(c(0, 0.01, 0.025, 0.05, 0.075, 0.10, 0.15), function(a) {
  ss <- score_households(M, F, M, a)
  data.table(nome = b$nome, w = b$w, s = ss)[, .(M = index_M(s, w)), by = nome][, alpha := a]
}))
alpha_grid[, rank := frank(-M), by = alpha]
fwrite(alpha_grid, file.path(O, "alpha_grid.csv"))
main <- alpha_grid[abs(alpha - PARAM$alpha) < 1e-12]
fwrite(alpha_grid[, .(spearman_IDHM = spearman_IDHM(.SD),
                      spearman_with_main = cor(M, main$M[match(nome, main$nome)], method = "spearman")),
                  by = alpha], file.path(O, "alpha_grid_summary.csv"))
log_msg("alpha grid done")

# Level spacing delta (groups, frontiers and admissible levels change)
delta_grid <- rbindlist(par_lapply(c(0.25, 0.5, 1.0), function(dl) {
  MM <- prepare_matrices(b, delta = dl); FF <- build_frontiers(MM, screen = PARAM$screen)
  lev <- admissible_level(FF, MM$tau)
  r <- data.table(nome = b$nome, w = b$w, s = score_households(MM, FF, MM, PARAM$alpha))[, .(M = index_M(s, w)), by = nome]
  log_msg("delta", dl)
  r[, `:=`(delta = dl, levels = length(FF$by_level), final_frontier = length(FF$frontier),
           households_moved_to_lower_level = sum(as.numeric(lev) < MM$tau - 1e-9))]
}))
delta_grid[, rank := frank(-M), by = delta]
fwrite(delta_grid, file.path(O, "delta_grid.csv"))
fwrite(delta_grid[, .(spearman_IDHM = spearman_IDHM(.SD)), by = delta], file.path(O, "delta_grid_summary.csv"))

# Excluded variables (section 5.1). Persons without severe disability as a share of residents,
# and the occupation of the household head added as a seventh categorical dimension
# (coded 0 to 5, higher = better), scored against its own fixed reference.
b7 <- copy(b); b7[, score := score + (5 - ocup_resp) / 5]
M7 <- prepare_matrices(b7); F7 <- build_frontiers(M7, screen = PARAM$screen)
th7 <- score_households(M7, F7, M7, PARAM$alpha)
M_by <- data.table(nome = b$nome, w = b$w, th6 = s$s, th7)[, .(M6 = index_M(th6, w), M7 = index_M(th7, w)), by = nome]
fwrite(data.table(
  item  = c("share_residents_without_severe_disability", "share_scores_unchanged_with_head_occupation",
            "spearman_scores_with_without_head_occupation", "northeast_above_south_with_head_occupation"),
  value = c(b[, sum(w * n_sem_deficiencia_severa) / sum(w * n_moradores)], mean(abs(s$s - th7) < 1e-6),
            cor(s$s, th7, method = "spearman"),
            M_by[nome %in% MUNICIPALITIES$nome[1:5], min(M7)] > M_by[nome %in% MUNICIPALITIES$nome[6:8], max(M7)])),
  file.path(O, "excluded_variables.csv"))
log_msg("excluded variables check done")

# National: screening and alternative references (sequential: large matrices)
m0_main <- readRDS(file.path(PATHS$work, "national.rds"))$m0
rm(z, b, M, F, s); invisible(gc())
keep <- c("uf", "municipio", "id_domicilio", "peso_amostral", RESOURCES, DEMANDS, "score")
slim <- function(d) sort_households(d[, intersect(keep, names(d)), with = FALSE])
b10 <- slim(readRDS(file.path(PATHS$work, "households_2010.rds"))[completo == TRUE])
b22 <- slim(readRDS(file.path(PATHS$work, "households_2022.rds"))[completo == TRUE & age_suppressed == FALSE])
invisible(gc())
M10 <- prepare_matrices(b10); M22 <- prepare_matrices(b22)
nat_versions <- list(no_screen = list(ii = seq_len(nrow(b10)), screen = Inf),
                     screen_2 = list(ii = seq_len(nrow(b10)), screen = 2),
                     screen_4 = list(ii = seq_len(nrow(b10)), screen = 4))
# The national version without households of more than 10 residents (V2) is not run:
# without them the national frontier grows to about 15,000 households, and evaluating
# 13.7 million households against it is not feasible on a laptop. V2 is reported for
# the eight municipalities.
# Each version is cached in work/, so an interrupted run resumes where it stopped.
alt <- lapply(names(nat_versions), function(v) {
  cache <- file.path(PATHS$work, paste0("robust_national_", v, "_rounds.rds"))
  if (file.exists(cache)) { log_msg("national reference", v, "| read from cache"); return(readRDS(cache)) }
  ii <- nat_versions[[v]]$ii
  MR <- list(X = M10$X[ii, , drop = FALSE], Y = M10$Y[ii, , drop = FALSE], tau = M10$tau[ii])
  FF <- build_frontiers(MR, screen = nat_versions[[v]]$screen)
  d <- rbind(data.table(uf = b10$uf, ano = 2010L, w = b10$peso_amostral, s = score_households(MR, FF, M10, PARAM$alpha)),
             data.table(uf = b22$uf, ano = 2022L, w = b22$peso_amostral, s = score_households(MR, FF, M22, PARAM$alpha)))
  md <- anchor_median(d[ano == 2010, s], d[ano == 2010, w])
  dom <- d[, .(full = dominance(.SD)$dominates, restricted = dominance(.SD, md, 3 * md)$dominates,
               restricted_main_anchor = dominance(.SD, m0_main, 3 * m0_main)$dominates), by = uf]
  uf_v <- summarise_scores(d, c("uf", "ano"), md)
  log_msg("national reference", v, "| frontier", length(FF$frontier), "| screened", nrow(FF$screened),
          "| dominance full", sum(dom$full), "restricted", sum(dom$restricted))
  res <- list(uf = uf_v[, version := v], dom = dom[, version := v],
              br = summarise_scores(d, "ano", md)[, `:=`(version = v, m0 = md, frontier_size = length(FF$frontier), screened = nrow(FF$screened))])
  saveRDS(res, cache); rm(MR, FF, d); invisible(gc())
  res
})
fwrite(rbindlist(lapply(alt, `[[`, "uf")), file.path(O, "alternative_references_ufs.csv"))
fwrite(rbindlist(lapply(alt, `[[`, "dom")), file.path(O, "alternative_references_dominance.csv"))
fwrite(rbindlist(lapply(alt, `[[`, "br")), file.path(O, "alternative_references_brazil.csv"))
rm(M10, M22, b10, b22); invisible(gc())
zn <- readRDS(file.path(PATHS$work, "national.rds")); zn$FR <- NULL

# National: bootstrap of the change 2010-2022 by UF
set.seed(SEEDS$bootstrap_national)
fwrite(rbindlist(lapply(sort(unique(zn$s$uf)), function(u) {
  a <- zn$s[uf == u & ano == 2010]; c2 <- zn$s[uf == u & ano == 2022]
  vv <- replicate(300, { ia <- sample.int(nrow(a), nrow(a), TRUE); ib <- sample.int(nrow(c2), nrow(c2), TRUE)
                         100 * (index_M(c2$s[ib], c2$w[ib]) / index_M(a$s[ia], a$w[ia]) - 1) })
  data.table(uf = u, change_pct = 100 * (index_M(c2$s, c2$w) / index_M(a$s, a$w) - 1), low = quantile(vv, .025), high = quantile(vv, .975))
})), file.path(O, "bootstrap_change_ufs.csv"))

# National: pairwise dominance between UFs in 2022 (first UF labelled as "2010" in the helper)
s22 <- zn$s[ano == 2022]; us <- sort(unique(s22$uf)); pp <- list()
for (i in 1:(length(us) - 1)) for (j in (i + 1):length(us)) {
  d <- rbind(s22[uf == us[i], .(s, w, ano = 2010)], s22[uf == us[j], .(s, w, ano = 2022)])
  a_dom <- dominance(d)$dominates; d[, ano := ifelse(ano == 2010, 2022, 2010)]
  pp[[length(pp) + 1]] <- data.table(a = us[i], b = us[j], a_dominates = a_dom, b_dominates = dominance(d)$dominates)
}
fwrite(rbindlist(pp), file.path(O, "pairwise_dominance_ufs_2022.csv"))
log_msg("stage 08 complete")
