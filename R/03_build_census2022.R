# =============================================================================
# Stage 03. Household base of the 2022 Census (public sample microdata, 27 UFs).
# Same variables and definitions as stage 02. Limits of the public 2022 files:
#   - no municipality code (the territorial unit is the UF);
#   - age only in five-year groups (P0180); the cut-offs used (15 to 64 years,
#     15 years or more) coincide with group limits, so they are exact in both years;
#   - ages suppressed for disclosure control are coded P0180 = 99. Households with
#     at least one suppressed age are flagged (age_suppressed) and excluded from the
#     main estimates, because their counts of adults are unknown, not zero.
# Harmonization with 2010: see docs/harmonization.md.
# Output: work/households_2022.rds
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)

COLS_D <- c("D0020", "D0100", "D0110", "D0130", "D0140", "D0190", "D0210", "D0230",
            "D0250", "D0260", "D0280", "D0300", "D0310", "D0350")
COLS_P <- c("P0100", "P0170", "P0180", "P0420", "P0430", "P0440", "P0460",
            "P0640", "P0770", "P0930", "P0940", "P0950", "P0990", "P1000")
members <- zip_members(PATHS$census2022_zip)

read_member <- function(type, uf, cols) {
  m <- grep(sprintf("(^|/)%s_%d_.*\\.csv$", type, uf), members, value = TRUE)
  if (length(m) != 1) stop("File ", type, " of UF ", uf, " not found (or not unique) in ", PATHS$census2022_zip)
  fread(cmd = sprintf("unzip -p %s %s", shQuote(PATHS$census2022_zip), shQuote(m)),
        sep = ";", select = cols, encoding = "UTF-8", showProgress = FALSE)
}

build_uf <- function(uf) {
  dom <- read_member("Domicilios", uf, COLS_D)[D0130 == 1]          # permanent private occupied
  p   <- read_member("Pessoas", uf, COLS_P)[P0100 %in% dom$D0100]
  p[, adulto_15_64      := P0180 %in% 4:13]
  p[, alfabetizado      := P0640 %in% 1]
  p[, adulto_medio_mais := P0180 %in% 4:17 & P0770 %in% c(3, 4)]
  p[, ocupado           := P0950 %in% 1]
  p[, formal            := ocupado & (P1000 %in% 1 | P0990 %in% c(2, 4))]   # formal contract, military, statutory
  p[, def_severa        := P0420 %in% 1 | P0430 %in% 1 | P0440 %in% 1 | P0460 %in% 1]
  p[, educ_pessoa       := fcase(P0770 == 1, 0, P0770 == 2, 1, P0770 == 3, 2, P0770 == 4, 3, default = NA_real_)]
  p[, ocup_pessoa := fcase(formal, 5,
                           ocupado & ((P0990 %in% 8 & P0940 %in% 1) | P0990 %in% 7), 4,
                           ocupado & P0990 %in% 8, 3,
                           ocupado, 2,
                           P0930 %in% 1 & P0950 %in% 0, 1, default = 0)]
  a <- p[, .(n_moradores                  = .N,
             n_adultos_em_idade_ativa     = sum(adulto_15_64),
             n_alfabetizados              = sum(alfabetizado),
             n_adultos_educ_media_ou_mais = sum(adulto_medio_mais),
             n_ocupados_formais           = sum(formal),
             n_sem_deficiencia_severa     = sum(!def_severa),
             educ_max  = if (all(is.na(educ_pessoa))) NA_real_ else max(educ_pessoa, na.rm = TRUE),
             ocup_resp = if (any(P0170 %in% 1)) ocup_pessoa[P0170 %in% 1][1] else NA_real_,
             age_suppressed = any(P0180 %in% 99)),
         by = .(D0100 = P0100)]
  d <- merge(dom, a, by = "D0100")
  d[, .(uf = D0020, id_domicilio = D0100, peso_amostral = D0110, situacao = D0140,
        n_moradores, n_adultos_em_idade_ativa, num_renda_total = D0350,
        n_alfabetizados, n_adultos_educ_media_ou_mais, n_ocupados_formais, n_sem_deficiencia_severa,
        n_dormitorios = D0230, n_banheiros = pmin(D0280, 9),
        educ_max, ocup_resp, age_suppressed,
        cat_agua = fcase(D0260 == 1 & D0300 %in% c(1, 2), 5,
                         D0260 %in% 2:4 & D0300 %in% c(1, 2), 4,
                         D0260 %in% 1:4 & D0300 == 3, 3,
                         D0260 %in% 5:8 & D0300 %in% c(1, 2), 2,
                         D0260 %in% 5:8 & D0300 == 3, 1, default = NA_real_),
        cat_esgoto = fcase(D0250 == 1, 5, D0250 %in% c(2, 3), 4, D0250 == 4, 3,
                           D0250 %in% c(5, 7), 2, D0250 == 6, 1, D0250 == 9, 0, default = NA_real_),
        cat_lixo = fcase(D0310 == 1, 2, D0310 == 2, 1, D0310 %in% 3:6, 0, default = NA_real_),
        cat_parede = fcase(D0210 == 1, 5, D0210 == 2, 4, D0210 == 4, 3, D0210 == 5, 2,
                           D0210 == 3, 1, D0210 %in% c(6, 7), 0, default = NA_real_),
        cat_cond_ocup = fcase(D0190 == 1, 5, D0190 == 2, 4, D0190 == 3, 3, D0190 == 4, 2,
                              D0190 %in% 5:7, 1, default = NA_real_))]
}

d <- rbindlist(lapply(UFS, function(u) { r <- build_uf(u); log_msg("UF", u, format(nrow(r), big.mark = ","), "households"); r }))
stopifnot(!anyDuplicated(d$id_domicilio))
add_score(d); flag_eligible(d); d[, ano := 2022L]; sort_households(d)
saveRDS(d, file.path(PATHS$work, "households_2022.rds"))
fwrite(d[, .(households = .N, eligible = sum(completo), eligible_age_known = sum(completo & !age_suppressed)), by = uf][order(uf)],
       file.path(PATHS$output, "base_2022_counts.csv"))
log_msg("stage 03 complete:", format(sum(d$completo & !d$age_suppressed), big.mark = ","), "eligible households with known ages")
