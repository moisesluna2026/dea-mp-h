# =============================================================================
# Stage 02. Household base of the 2010 Census (sample, 27 UFs) from the official
# IBGE fixed-width files. Positions follow Layout_microdados_Amostra (Documentacao.zip).
# Unit: permanent private occupied households (V4001 = 01).
# Output: work/households_2010.rds (one row per household).
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)

SPEC_DOM <- data.table(
  var    = c("V0001", "V0002", "V0300", "V0010", "V1006", "V4001", "V0201", "V0202", "V0204",
             "V0205", "V0206", "V0207", "V0208", "V0209", "V0210", "V0401", "V6529"),
  start  = c(1, 3, 21, 29, 53, 54, 58, 74, 80, 85, 86, 87, 88, 90, 91, 105, 109),
  length = c(2, 5, 8, 16, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 7))
SPEC_PES <- data.table(
  var    = c("V0002", "V0300", "V0502", "V6036", "V0614", "V0615", "V0616", "V0617", "V0627",
             "V6400", "V0648", "V0650", "V6910"),
  start  = c(3, 21, 54, 62, 70, 71, 72, 73, 146, 158, 209, 211, 392),
  length = c(5, 8, 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1))
IPCA_FACTOR <- PARAM$ipca_jul2022 / PARAM$ipca_jul2010

build_zip <- function(zip) {
  members <- zip_members(zip)
  f_dom <- grep("domicilios.*\\.txt$", members, ignore.case = TRUE, value = TRUE)
  f_pes <- grep("pessoas.*\\.txt$", members, ignore.case = TRUE, value = TRUE)
  stopifnot(length(f_dom) == 1, length(f_pes) == 1)

  dom <- read_fixed_width_zip(zip, f_dom, SPEC_DOM)
  dom[, setdiff(names(dom), "V0010") := lapply(.SD, as.numeric), .SDcols = setdiff(names(dom), "V0010")]
  dom[, V0010 := as.numeric(V0010) / 1e13]          # 3 integer and 13 decimal digits
  dom <- dom[V4001 == 1]

  p <- read_fixed_width_zip(zip, f_pes, SPEC_PES)
  p[, names(p) := lapply(.SD, as.numeric)]
  p <- p[dom[, .(V0002, V0300)], on = .(V0002, V0300), nomatch = NULL]

  p[, adulto_15_64      := V6036 >= 15 & V6036 <= 64]
  p[, alfabetizado      := V0627 %in% 1]
  p[, adulto_medio_mais := V6036 >= 15 & V6400 %in% c(3, 4)]
  p[, ocupado           := V6910 %in% 1]
  p[, formal            := ocupado & V0648 %in% c(1, 2, 3)]          # formal contract, military, statutory
  p[, def_severa        := V0614 %in% 1 | V0615 %in% 1 | V0616 %in% 1 | V0617 %in% 1]
  p[, educ_pessoa       := fcase(V6400 == 1, 0, V6400 == 2, 1, V6400 == 3, 2, V6400 == 4, 3, default = NA_real_)]
  p[, ocup_pessoa := fcase(formal, 5,
                           ocupado & ((V0648 %in% 5 & V0650 %in% c(1, 2)) | V0648 %in% 6), 4,
                           ocupado & V0648 %in% 5, 3,
                           ocupado, 2,
                           V6910 %in% 2, 1, default = 0)]
  a <- p[, .(n_moradores                  = .N,
             n_adultos_em_idade_ativa     = sum(adulto_15_64),
             n_alfabetizados              = sum(alfabetizado),
             n_adultos_educ_media_ou_mais = sum(adulto_medio_mais),
             n_ocupados_formais           = sum(formal),
             n_sem_deficiencia_severa     = sum(!def_severa),
             educ_max  = if (all(is.na(educ_pessoa))) NA_real_ else max(educ_pessoa, na.rm = TRUE),
             ocup_resp = if (any(V0502 %in% 1)) ocup_pessoa[V0502 %in% 1][1] else NA_real_),
         by = .(V0002, V0300)]
  d <- merge(dom, a, by = c("V0002", "V0300"))
  # Consistency check: person records per household must equal the number of
  # residents declared in the household file (V0401).
  if (any(d$n_moradores != d$V0401, na.rm = TRUE)) stop("Person records do not match V0401 in ", basename(zip))
  d[, .(uf = V0001, municipio = V0002, id_domicilio = V0300, peso_amostral = V0010, situacao = V1006,
        n_moradores, n_adultos_em_idade_ativa,
        num_renda_total_2010 = V6529, num_renda_total = V6529 * IPCA_FACTOR,   # reais of July 2022
        n_alfabetizados, n_adultos_educ_media_ou_mais, n_ocupados_formais, n_sem_deficiencia_severa,
        n_dormitorios = V0204, n_banheiros = pmin(V0205, 9),
        educ_max, ocup_resp,
        cat_agua = fcase(V0208 == 1 & V0209 %in% c(1, 2), 5,
                         V0208 %in% c(2, 3, 9, 10) & V0209 %in% c(1, 2), 4,
                         V0208 %in% c(1, 2, 3, 9, 10) & V0209 == 3, 3,
                         V0208 %in% 4:8 & V0209 %in% c(1, 2), 2,
                         V0208 %in% 4:8 & V0209 == 3, 1, default = NA_real_),
        cat_esgoto = fcase(V0207 == 1, 5, V0207 == 2, 4, V0207 == 3, 3, V0207 %in% c(4, 6), 2,
                           V0207 == 5, 1, V0206 == 2, 0, default = NA_real_),
        cat_lixo = fcase(V0210 == 1, 2, V0210 == 2, 1, V0210 %in% 3:7, 0, default = NA_real_),
        cat_parede = fcase(V0202 %in% c(1, 4), 5, V0202 == 2, 4, V0202 == 3, 3, V0202 == 6, 2,
                           V0202 == 5, 1, V0202 %in% 7:9, 0, default = NA_real_),
        cat_cond_ocup = fcase(V0201 == 1, 5, V0201 == 2, 4, V0201 == 3, 3, V0201 == 4, 2,
                              V0201 %in% c(5, 6), 1, default = NA_real_))]
}

zips <- file.path(PATHS$census2010_dir, setdiff(list.files(PATHS$census2010_dir, "\\.zip$"), "Documentacao.zip"))
stopifnot(length(zips) == 28)
parts <- lapply(zips, function(z) { r <- build_zip(z); log_msg(basename(z), format(nrow(r), big.mark = ","), "households"); r })
d <- rbindlist(parts); rm(parts); invisible(gc())
stopifnot(!anyDuplicated(d[, .(uf, municipio, id_domicilio)]), setequal(unique(d$uf), UFS))

add_score(d); flag_eligible(d); d[, ano := 2010L]; sort_households(d)
saveRDS(d, file.path(PATHS$work, "households_2010.rds"))
fwrite(d[, .(households = .N, eligible = sum(completo)), by = uf][order(uf)],
       file.path(PATHS$output, "base_2010_counts.csv"))
log_msg("stage 02 complete:", format(nrow(d), big.mark = ","), "households,", format(sum(d$completo), big.mark = ","), "eligible")
