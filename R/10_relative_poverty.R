# =============================================================================
# Stage 10: relative multidimensional poverty, incidence and intensity (Table 6).
# A household is classified in relative multidimensional poverty when its
# truncated score is at least kappa times the weighted anchor median of the base
# year (PARAM$kappa = 2). For each population: incidence H, intensity A (mean
# truncated score among classified households) and adjusted index M = H x A.
# Uses the scores saved by stages 05 and 06; no model is re-estimated.
# =============================================================================
source("R/config.R"); source("R/functions.R")
suppressMessages(library(data.table))
O <- file.path(PATHS$output, "relative"); dir.create(O, recursive = TRUE, showWarnings = FALSE)
relpov <- function(d, m0, k) {
  h <- pmin(d$s, 1); w <- d$w; p <- h >= k * m0
  list(M = sum(w * h) / sum(w), H = sum(w * p) / sum(w),
       A = if (any(p)) sum(w * h * p) / sum(w * p) else NA_real_,
       M_adj = sum(w * h * p) / sum(w))
}
k <- PARAM$kappa
mu <- readRDS(file.path(PATHS$work, "municipal.rds"))
fwrite(mu$s[, relpov(.SD, mu$m0, k), by = nome][order(-M)], file.path(O, "municipal.csv"))
na <- readRDS(file.path(PATHS$work, "national.rds")); sn <- na$s
fwrite(sn[, relpov(.SD, na$m0, k), by = .(uf, ano)][order(uf, ano)], file.path(O, "ufs.csv"))
fwrite(sn[, relpov(.SD, na$m0, k), by = .(regiao, ano)][order(regiao, ano)], file.path(O, "regions.csv"))
fwrite(sn[, relpov(.SD, na$m0, k), by = ano], file.path(O, "brazil.csv"))
sens <- rbindlist(lapply(c(1.5, 2, 3), function(kk) sn[, c(kappa = kk, relpov(.SD, na$m0, kk)), by = ano]))
fwrite(sens, file.path(O, "brazil_kappa_sensitivity.csv"))
log_msg("stage 10 complete; kappa =", k)
