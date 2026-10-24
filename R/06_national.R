# =============================================================================
# Stage 06. National extension: 27 UFs, 2010 and 2022 against the same 2010 reference.
# Reference: every eligible household of the 2010 sample, after the screening of
#   atypical households (super-efficiency above PARAM$screen). No sampling is involved.
# Evaluation: every eligible household of 2010 and 2022, with the census weights.
# Anchor: weighted median of the truncated 2010 scores (Brazil), fixed for both years.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- file.path(PATHS$output, "national"); dir.create(O, showWarnings = FALSE)

load_base <- function(f) {
  d <- readRDS(file.path(PATHS$work, f))[completo == TRUE]
  sort_households(d)
}
b10 <- load_base("households_2010.rds")
b22all <- load_base("households_2022.rds")
b22 <- b22all[age_suppressed == FALSE]
log_msg("eligible households: 2010", nrow(b10), "| 2022", nrow(b22), "| 2022 with suppressed ages", sum(b22all$age_suppressed))

MR <- prepare_matrices(b10)
FR <- build_frontiers(MR, screen = PARAM$screen)
log_msg("national frontiers built; final frontier has", length(FR$frontier), "households;", nrow(FR$screened), "households screened out")
fwrite(frontier_profile(b10[FR$frontier]), file.path(O, "frontier_profile.csv"))
fwrite(cbind(FR$screened, b10[FR$screened$household, c("uf", "municipio", RESOURCES, DEMANDS, "score"), with = FALSE]),
       file.path(O, "screened_households.csv"))
fwrite(data.table(level = names(FR$by_level), frontier_size = lengths(FR$by_level)), file.path(O, "frontiers_by_level.csv"))

s10 <- score_households(MR, FR, MR, PARAM$alpha); log_msg("2010 scored")
s22 <- score_households(MR, FR, prepare_matrices(b22), PARAM$alpha); log_msg("2022 scored")
sn <- rbind(data.table(uf = b10$uf, ano = 2010L, w = b10$peso_amostral, s = s10),
            data.table(uf = b22$uf, ano = 2022L, w = b22$peso_amostral, s = s22))
sn[, regiao := region(uf)]
m0 <- anchor_median(sn[ano == 2010, s], sn[ano == 2010, w])

uf_tab <- summarise_scores(sn, c("uf", "ano"), m0)
fwrite(uf_tab, file.path(O, "ufs.csv"))
fwrite(summarise_scores(sn, c("regiao", "ano"), m0), file.path(O, "regions.csv"))
fwrite(summarise_scores(sn, "ano", m0), file.path(O, "brazil.csv"))

dom <- merge(sn[, { r <- dominance(.SD); .(full = r$dominates, min_full = r$minimum) }, by = uf],
             sn[, { r <- dominance(.SD, m0, 3 * m0); .(restricted = r$dominates, min_restricted = r$minimum) }, by = uf], by = "uf")
fwrite(dom, file.path(O, "dominance.csv"))

fwrite(rbindlist(lapply(c(2010, 2022), function(y) {
  d <- if (y == 2010) b10 else b22
  data.table(ano = y, moradores = weighted.mean(d$n_moradores, d$peso_amostral),
             renda_por_morador = sum(d$num_renda_total * d$peso_amostral) / sum(d$n_moradores * d$peso_amostral),
             formais_por_adulto = sum(d$n_ocupados_formais * d$peso_amostral) / sum(d$n_adultos_em_idade_ativa * d$peso_amostral),
             renda_zero_pct = 100 * weighted.mean(d$num_renda_total <= 0, d$peso_amostral))
})), file.path(O, "descriptives_brazil.csv"))

# Households with suppressed ages (2022): scored against the same reference and
# added to their UF, as a bound on the effect of excluding them.
ex <- b22all[age_suppressed == TRUE]
ex[, s := score_households(MR, FR, prepare_matrices(ex), PARAM$alpha)]
inc <- merge(merge(uf_tab[ano == 2022, .(uf, M_excluded = M)],
                   ex[, .(w_ex = sum(peso_amostral), sum_ex = sum(peso_amostral * truncate1(s))), by = uf], by = "uf", all.x = TRUE),
             b22[, .(w_in = sum(peso_amostral)), by = uf], by = "uf")
inc[is.na(w_ex), `:=`(w_ex = 0, sum_ex = 0)]
inc[, M_included := (M_excluded * w_in + sum_ex) / (w_in + w_ex)]
fwrite(inc[, .(uf, weighted_households_excluded = w_ex, M_excluded, M_included)], file.path(O, "age_suppression_sensitivity.csv"))

fwrite(FR$se_frontier, file.path(O, "super_efficiency_final_frontiers.csv"))
fwrite(data.table(item = c("reference_2010", "screened_out_of_reference", "screening_rounds", "max_super_efficiency_final_frontiers",
                           "evaluated_2010", "evaluated_2022", "excluded_2022_age_suppressed", "anchor_median_m0"),
                  value = c(nrow(b10), nrow(FR$screened), FR$rounds, max(FR$se_frontier$super_efficiency),
                            nrow(b10), nrow(b22), nrow(ex), m0)),
       file.path(O, "configuration.csv"))
saveRDS(list(FR = FR, s = sn, m0 = m0), file.path(PATHS$work, "national.rds"))
log_msg("stage 06 complete; m0 =", signif(m0, 6))
