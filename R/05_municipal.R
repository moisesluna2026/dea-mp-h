# =============================================================================
# Stage 05. Municipal illustration (2010): eight municipalities, complete reference.
# Every eligible household of the eight municipalities forms the reference R,
# after the screening of atypical households (super-efficiency above PARAM$screen);
# the frontiers are computed once and every household is scored against them.
# The same data are also scored with a local frontier (one per municipality),
# to show why scores from separate frontiers are not comparable.
# Outputs: table 1 (descriptives), table 2 (scores), frontier profile.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- file.path(PATHS$output, "municipal"); dir.create(O, showWarnings = FALSE)

b <- readRDS(file.path(PATHS$work, "households_2010.rds"))[completo == TRUE]
b <- merge(b, MUNICIPALITIES, by = c("uf", "municipio")); sort_households(b)
b[, w := peso_amostral]
log_msg("eight municipalities:", nrow(b), "households")

M <- prepare_matrices(b)
F <- build_frontiers(M, screen = PARAM$screen)
log_msg("frontiers built; final frontier has", length(F$frontier), "households;", nrow(F$screened), "households screened out")
s <- b[, .(nome, uf, municipio, IDHM, w, score)]
for (a in c(0, 0.05, 0.10)) { s[, (paste0("theta_", a)) := score_households(M, F, M, a)]; log_msg("scores, alpha =", a) }
s[, s := theta_0.05]
m0 <- anchor_median(s$s, s$w)

tab2 <- summarise_scores(s, "nome", m0)
tab2 <- merge(tab2, s[, .(M_alpha0 = index_M(theta_0, w), M_alpha10 = index_M(theta_0.1, w)), by = nome], by = "nome")

# Local frontier: each municipality is its own reference (alpha = 0, as in usual DEA practice).
local <- rbindlist(par_lapply(MUNICIPALITIES$nome, function(nm) {
  ii <- which(b$nome == nm); MM <- prepare_matrices(b[ii]); LL <- build_frontiers(MM)
  if (nm == "Itabuna-BA") saveRDS(list(ids = ii, M = MM, F = LL, b = b[ii]), file.path(PATHS$work, "itabuna_local.rds"))
  log_msg("local frontier", nm)
  data.table(nome = nm, M_local = index_M(LL$local, b$w[ii]))
}))
tab2 <- merge(merge(tab2, local, by = "nome"), MUNICIPALITIES[, .(nome, IDHM)], by = "nome")
tab2[, rank := frank(-M)]
fwrite(tab2[order(rank)], file.path(O, "table2_municipalities.csv"))
fwrite(data.table(score = c("fixed reference", "local frontier"),
                  spearman_IDHM = c(cor(tab2$M, tab2$IDHM, method = "spearman"),
                                    cor(tab2$M_local, tab2$IDHM, method = "spearman"))),
       file.path(O, "spearman_IDHM.csv"))

tab1 <- b[, .(n = .N, moradores = weighted.mean(n_moradores, w), renda_mediana = weighted_median(num_renda_total, w),
              renda_zero_pct = 100 * weighted.mean(num_renda_total == 0, w), formais = weighted.mean(n_ocupados_formais, w),
              banheiros = weighted.mean(n_banheiros, w), escore = weighted.mean(score, w)), by = nome]
fwrite(tab1, file.path(O, "table1_descriptives.csv"))
fwrite(frontier_profile(b[F$frontier]), file.path(O, "frontier_profile.csv"))
fwrite(cbind(F$screened, b[F$screened$household, c("nome", RESOURCES, DEMANDS, "score"), with = FALSE]),
       file.path(O, "screened_households.csv"))
fwrite(data.table(level = names(F$by_level), frontier_size = lengths(F$by_level),
                  reference_size = vapply(as.numeric(names(F$by_level)), function(t) sum(F$tau[F$in_reference] >= t - 1e-9), 0L)),
       file.path(O, "frontiers_by_level.csv"))
fwrite(F$se_frontier, file.path(O, "super_efficiency_final_frontiers.csv"))
fwrite(data.table(item = c("households", "screened_out_of_reference", "screening_rounds", "max_super_efficiency_final_frontiers", "anchor_median_m0"),
                  value = c(nrow(b), nrow(F$screened), F$rounds, max(F$se_frontier$super_efficiency), m0)),
       file.path(O, "configuration.csv"))

saveRDS(list(b = b, M = M, F = F, s = s, m0 = m0), file.path(PATHS$work, "municipal.rds"))
log_msg("stage 05 complete; m0 =", signif(m0, 6))
