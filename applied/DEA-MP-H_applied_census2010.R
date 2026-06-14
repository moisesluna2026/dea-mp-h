# =============================================================================
# DEA-MP/H: Data Envelopment Analysis for Multidimensional Poverty
#           Household-level application
#
# Appendix A - Replication script
# Empirical application: 2010 Brazilian Demographic Census, sample
# questionnaire, 8 selected municipalities
# =============================================================================
#
# METHOD OVERVIEW (see Section 4 of the article for full description)
#
# The method comprises three steps, applied separately to each geographic
# unit (in this study, municipality):
#
#   Step 1 - Identification (ordinal dominance restriction)
#     For each household, a categorical deprivation score is computed from
#     eight dimensions (water supply, sanitation, waste collection,
#     electricity, wall material, dwelling occupancy status, household
#     maximum educational attainment, and household head's occupation).
#     Each dimension is inverted (higher = more deprived) and normalized
#     by its maximum value, yielding a 0-1 scale per dimension. The total
#     deprivation score is the sum of the eight normalized dimensions,
#     ranging from 0 to 8.
#
#     Comparison clusters are defined by thresholds k = 0.5, 1.0, 1.5, ...
#     up to the maximum score observed in the geographic unit. A household
#     with deprivation score s belongs to every cluster k <= s. This
#     operationalizes the ordinal dominance restriction of Banker and
#     Morey (1986): a household is only compared with peers in an equal
#     or worse overall deprivation position.
#
#   Step 2 - Graduation (DEA CCR, input-oriented)
#     Within each cluster, an input-oriented CCR model (Charnes, Cooper
#     and Rhodes, 1978) is estimated using the deaR package. Outputs are
#     household size and the number of working-age residents (15-64
#     years, ILO definition); inputs are positive household resources
#     (income, education, formal employment, absence of severe
#     disability, and housing standard). All inputs are absolute counts,
#     not rates, to avoid double-counting with the outputs.
#
#   Step 3 - Final score (MAX across clusters)
#     Each household appears in every cluster k <= its deprivation score.
#     The final poverty score is the maximum efficiency score obtained
#     across all these clusters. A score of 1 indicates the household is
#     on the maximum-poverty frontier within at least one cluster.
#
# DATA
#   Source: IBGE, 2010 Demographic Census, sample questionnaire (10%
#   sample). Unit of analysis: permanent private household. After quality
#   filters, the dataset used in this study contains 84,232 households
#   across 8 municipalities (5 in the Northeast and 3 in the South region
#   of Brazil).
#
# REQUIREMENTS
#   R packages: deaR, data.table, openxlsx
#   Install with: install.packages(c("deaR", "data.table", "openxlsx"))
# =============================================================================


# =============================================================================
# SECTION 0 - PACKAGES
# =============================================================================

library(deaR)
library(data.table)
library(openxlsx)

cat("Packages loaded.\n\n")


# =============================================================================
# SECTION 1 - PATHS
# Replace the placeholders below with the paths on your machine.
# =============================================================================

# [FILL IN] Path to the household-level dataset (.rds), one row per
# household, containing the variables listed in Sections 2 and 3.
INPUT_FILE <- "path/to/household_dataset.rds"

# [FILL IN] Output directory for results
OUTPUT_DIR <- "path/to/output_folder"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)


# =============================================================================
# SECTION 2 - CATEGORICAL DEPRIVATION SCORE (Step 1)
# =============================================================================

cat("Loading data...\n")
dt <- as.data.table(readRDS(INPUT_FILE))
cat(sprintf("Households: %d\n\n", nrow(dt)))

# Each of the eight categorical variables below is assumed to already be
# coded on an ordinal scale where 0 represents the most favorable category
# (e.g., piped water, sewer connection, owned dwelling, higher education)
# and higher values represent progressively less favorable categories.
# The variables and their maximum raw values (max_raw) in this study were:
#
#   water_supply      (score_agua)      max_raw = 5
#   sanitation        (score_esgoto)    max_raw = 5
#   waste_collection  (score_lixo)      max_raw = 2
#   electricity       (score_energia)   max_raw = 1  (binary)
#   wall_material     (score_parede)    max_raw = 5
#   occupancy_status  (score_cond_ocup) max_raw = 5
#   education_max     (educ_max)        max_raw = 4
#   head_occupation   (score_ocup_resp) max_raw = 5
#
# Each variable is inverted (max_raw - raw_value) so that higher values
# indicate greater deprivation, then normalized by max_raw to a 0-1 scale.
# The total deprivation score is the sum of the eight normalized values,
# ranging from 0 to 8.

cat_vars <- c("water_supply", "sanitation", "waste_collection",
              "electricity", "wall_material", "occupancy_status",
              "education_max", "head_occupation")

max_raw <- c(water_supply = 5, sanitation = 5, waste_collection = 2,
              electricity = 1, wall_material = 5, occupancy_status = 5,
              education_max = 4, head_occupation = 5)

# Invert and normalize each categorical variable
for (v in cat_vars) {
  dt[is.na(get(v)) | get(v) < 0, (v) := 0]
  dt[, (v) := (max_raw[v] - as.numeric(get(v))) / max_raw[v]]
}

# Total deprivation score (0 to 8)
dt[, deprivation_score := rowSums(.SD), .SDcols = cat_vars]

cat(sprintf("Deprivation score: min=%.2f | max=%.2f | mean=%.2f\n\n",
            min(dt$deprivation_score), max(dt$deprivation_score),
            mean(dt$deprivation_score)))


# =============================================================================
# SECTION 3 - DEA MODEL VARIABLES (Step 2)
# =============================================================================

# Outputs (2): people to be supported by the household's resources
OUTPUTS <- c("household_size", "working_age_adults")

# Inputs (7): positive household resources, absolute counts
INPUTS <- c("total_income", "n_literate", "n_secondary_educ_or_higher",
            "n_formal_employment", "n_no_severe_disability",
            "n_bedrooms", "n_bathrooms")

# Treat missing values and non-positive values
# (DEA requires strictly positive inputs and outputs; non-positive values
# are replaced by a small technical minimum)
dt[is.na(total_income) | total_income <= 0, total_income := 1]
for (v in c(INPUTS, OUTPUTS)) {
  dt[, (v) := as.numeric(get(v))]
  dt[is.na(get(v)) | get(v) <= 0, (v) := 0.001]
}

dt[, DMU := as.character(household_id)]


# =============================================================================
# SECTION 4 - GEOGRAPHIC UNITS
# =============================================================================

# [FILL IN] Geographic unit identifier and labels.
# In this study, the geographic unit is the municipality, identified by
# the 5-digit IBGE municipality code used in the Census sample files.

geo_var <- "municipality_code"

geo_labels <- c(
  "02404" = "Blumenau-SC",
  "04009" = "Campina Grande-PB",
  "05108" = "Caxias do Sul-RS",
  "08003" = "Mossoro-RN",
  "09600" = "Olinda-PE",
  "10707" = "Paulista-PE",
  "14802" = "Itabuna-BA",
  "15200" = "Maringa-PR"
)

geo_codes <- sort(names(geo_labels))

cat("Geographic units:", paste(geo_labels, collapse = ", "), "\n\n")


# =============================================================================
# SECTION 5 - MAIN LOOP: BY GEOGRAPHIC UNIT AND BY CLUSTER (Steps 1-3)
# =============================================================================

start_time   <- Sys.time()
list_results <- list()
list_summary <- list()

for (geo in geo_codes) {

  geo_label <- geo_labels[geo]
  cat(sprintf("==========================================\n"))
  cat(sprintf("UNIT: %s\n", geo_label))

  dt_geo <- dt[get(geo_var) == geo]
  k_min  <- min(dt_geo$deprivation_score)
  k_max  <- max(dt_geo$deprivation_score)

  # Cluster thresholds in steps of 0.5
  k_seq <- seq(ceiling(k_min * 2) / 2, k_max, by = 0.5)

  cat(sprintf("  Households: %d | scores: %.2f to %.2f | clusters: %d\n\n",
              nrow(dt_geo), k_min, k_max, length(k_seq)))

  res_geo <- data.table(
    household_id      = dt_geo$household_id,
    deprivation_score = dt_geo$deprivation_score
  )

  for (k in k_seq) {

    idx    <- which(dt_geo$deprivation_score >= k)
    n_clus <- length(idx)
    col_k  <- paste0("k_", gsub("\\.", "_", as.character(k)))

    # Trivial cluster: assign maximum deprivation score
    if (n_clus <= 2) {
      res_geo[idx, (col_k) := 1]
      cat(sprintf("  k=%.1f: %6d households | score=1 (minimum cluster)\n",
                  k, n_clus))
      next
    }

    cluster_dt <- dt_geo[idx]
    df_dea <- as.data.frame(cluster_dt[, c("DMU", INPUTS, OUTPUTS),
                                        with = FALSE])

    nI <- length(INPUTS)
    nO <- length(OUTPUTS)

    datadea <- tryCatch(
      suppressWarnings(make_deadata(
        datadea = df_dea, dmus = 1,
        inputs  = 2:(1 + nI),
        outputs = (2 + nI):(1 + nI + nO)
      )),
      error = function(e) {
        cat(sprintf("  k=%.1f: ERROR (make_deadata) - %s\n", k, e$message))
        NULL
      }
    )
    if (is.null(datadea)) next

    # CCR model, input orientation: for each household, how much could
    # its inputs be proportionally reduced while sustaining the same
    # household size and number of working-age adults? Households with
    # few positive resources relative to the people they support are
    # closest to the poverty frontier (score -> 1).
    model <- tryCatch(
      model_basic(datadea = datadea, orientation = "io", rts = "crs"),
      error = function(e) {
        cat(sprintf("  k=%.1f: ERROR (model_basic) - %s\n", k, e$message))
        NULL
      }
    )
    if (is.null(model)) next

    scores <- efficiencies(model)
    res_geo[idx, (col_k) := as.numeric(scores)]

    cat(sprintf("  k=%.1f: %6d households | min=%.4f | max=%.4f | frontier=%d\n",
                k, n_clus,
                round(min(scores, na.rm = TRUE), 4),
                round(max(scores, na.rm = TRUE), 4),
                sum(scores >= 0.999)))

    rm(cluster_dt, df_dea, datadea, model, scores); gc()
  }

  # Step 3: final score = MAX across all clusters
  cols_k <- grep("^k_", names(res_geo), value = TRUE)

  res_geo[, poverty_score := apply(.SD, 1, function(x) {
    vals <- x[!is.na(x)]
    if (length(vals) == 0) return(NA_real_)
    max(vals)
  }), .SDcols = cols_k]

  res_geo[, cluster_max := apply(.SD, 1, function(x) {
    nms     <- names(x)
    vals    <- as.numeric(x)
    idx_max <- which.max(vals)
    if (length(idx_max) == 0 || is.na(vals[idx_max])) return(NA_character_)
    nms[idx_max]
  }), .SDcols = cols_k]

  res_geo[, on_frontier := ifelse(poverty_score >= 0.999, "Yes", "No")]

  final_geo <- merge(
    res_geo[, .(household_id, deprivation_score, poverty_score,
                cluster_max, on_frontier)],
    dt_geo[, c("household_id", OUTPUTS, INPUTS), with = FALSE],
    by = "household_id"
  )[order(-poverty_score)]

  final_geo[, geo_unit := geo_label]

  n_front <- sum(final_geo$on_frontier == "Yes")
  summary_geo <- data.table(
    geo_unit       = geo_label,
    n_households   = nrow(final_geo),
    n_frontier     = n_front,
    pct_frontier   = round(n_front / nrow(final_geo) * 100, 2),
    score_mean     = round(mean(final_geo$poverty_score, na.rm = TRUE), 4),
    score_median   = round(median(final_geo$poverty_score, na.rm = TRUE), 4),
    score_sd       = round(sd(final_geo$poverty_score, na.rm = TRUE), 4),
    income_frontier = round(mean(final_geo[on_frontier == "Yes"]$total_income, na.rm = TRUE), 0),
    income_others   = round(mean(final_geo[on_frontier == "No"]$total_income, na.rm = TRUE), 0),
    deprivation_frontier = round(mean(final_geo[on_frontier == "Yes"]$deprivation_score, na.rm = TRUE), 2),
    deprivation_others   = round(mean(final_geo[on_frontier == "No"]$deprivation_score, na.rm = TRUE), 2)
  )

  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
  cat(sprintf("\n  Done | mean score=%.4f | frontier=%d (%.2f%%)\n\n",
              summary_geo$score_mean, n_front, summary_geo$pct_frontier))

  saveRDS(final_geo, file.path(OUTPUT_DIR,
          sprintf("%s_scores.rds", gsub("[^a-zA-Z0-9]", "_", geo_label))))

  list_results[[geo]] <- final_geo
  list_summary[[geo]]  <- summary_geo

  rm(dt_geo, res_geo, final_geo); gc()
}

total_time <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
cat(sprintf("==========================================\n"))
cat(sprintf("TOTAL TIME: %.1f minutes\n\n", as.numeric(total_time)))


# =============================================================================
# SECTION 6 - CONSOLIDATION AND EXPORT
# =============================================================================

final_all   <- rbindlist(list_results, fill = TRUE)
summary_all <- rbindlist(list_summary)[order(-score_mean)]

cat("=== RESULTS SUMMARY ===\n\n")
print(summary_all)

saveRDS(final_all, file.path(OUTPUT_DIR, "DEA_MP_H_scores.rds"))
fwrite(final_all,  file.path(OUTPUT_DIR, "DEA_MP_H_scores.csv"))

wb <- createWorkbook()
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_all)
addWorksheet(wb, "Household Scores")
writeData(wb, "Household Scores", final_all[order(geo_unit, -poverty_score)])

saveWorkbook(wb, file.path(OUTPUT_DIR, "DEA_MP_H_results.xlsx"), overwrite = TRUE)

cat("\nExport complete.\n")
cat("Output directory:", OUTPUT_DIR, "\n")
