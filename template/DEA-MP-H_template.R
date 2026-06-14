# =============================================================================
# DEA-MP/H: Data Envelopment Analysis for Multidimensional Poverty
#           Household-level application
#
# Generic configurable template
#
# This template implements the DEA-MP/H method, combining:
#   (1) An ordinal dominance restriction (adapted from Banker and Morey,
#       1986) to define comparison clusters from a categorical deprivation
#       score, so that households are compared only with peers in an
#       equal or worse overall deprivation position.
#   (2) A DEA CCR input-oriented model (Charnes, Cooper and Rhodes, 1978)
#       to derive endogenous weights and compute a continuous poverty
#       score within each cluster.
#
# The final poverty score ranges from 0 to 1:
#   score = 1  -> household is on the maximum-poverty frontier within at
#                  least one cluster
#   score < 1  -> household is below the frontier in every cluster in
#                  which it appears
#
# HOW TO USE THIS TEMPLATE
#   Edit only SECTION 1 (configuration). Sections 2-6 implement the method
#   and do not need to be modified for standard applications. Every field
#   that must be adapted to your data is marked with [FILL IN].
#
# REFERENCES
#   Alkire, S., & Foster, J. (2011). Counting and multidimensional poverty
#     measurement. Journal of Public Economics, 95(7-8), 476-487.
#   Banker, R. D., & Morey, R. C. (1986). Efficiency analysis for
#     exogenously fixed inputs and outputs. Operations Research, 34(4),
#     513-521.
#   Charnes, A., Cooper, W. W., & Rhodes, E. (1978). Measuring the
#     efficiency of decision making units. European Journal of
#     Operational Research, 2(6), 429-444.
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
# SECTION 1 - CONFIGURATION
# Edit this section to match your data. Do not modify Sections 2-6.
# =============================================================================

# ---- 1.1 File paths -------------------------------------------------------

# [FILL IN] Path to your household-level dataset (.rds), one row per
# household, containing the variables configured below.
INPUT_FILE <- "path/to/your/household_dataset.rds"

# [FILL IN] Output directory for results
OUTPUT_DIR <- "path/to/your/output_folder"

# ---- 1.2 Identifiers -------------------------------------------------------

# [FILL IN] Unique household identifier
ID_VAR <- "household_id"

# [FILL IN] Geographic unit identifier (e.g., municipality, district,
# country code). The method is run separately for each geographic unit.
GEO_VAR <- "geo_unit_code"

# [FILL IN] Labels for geographic units (named character vector).
# Format: c("code1" = "Label 1", "code2" = "Label 2", ...)
# Leave as NULL to use the codes themselves as labels.
GEO_LABELS <- c(
  "code1" = "Geographic unit 1",
  "code2" = "Geographic unit 2"
  # add more as needed
)

# ---- 1.3 DEA model variables -----------------------------------------------

# [FILL IN] Outputs: people (or units) to be supported by the household's
# resources. The DEA-MP/H model used in the reference study employs two
# outputs - household size and the number of working-age residents - but
# the method admits one or more outputs depending on the research
# question.
OUTPUTS <- c("household_size", "working_age_adults")

# [FILL IN] Inputs: positive household resources, expressed as absolute
# counts (not rates or proportions, to avoid double-counting with the
# outputs). The reference study used seven inputs spanning income,
# education, employment, health, and housing standard.
INPUTS <- c("total_income", "n_literate", "n_secondary_educ_or_higher",
            "n_formal_employment", "n_no_severe_disability",
            "n_bedrooms", "n_bathrooms")

# ---- 1.4 Categorical variables for clustering ------------------------------

# [FILL IN] Names of the categorical variables used to compute the
# deprivation score. Each variable must be coded on an ordinal scale
# where 0 represents the most favorable category and higher integer
# values represent progressively less favorable categories (the
# recoding is done in Section 1.5).
CAT_VARS <- c("water_supply", "sanitation", "waste_collection",
              "electricity", "wall_material", "occupancy_status",
              "education_max", "head_occupation")

# [FILL IN] Maximum raw value (after recoding in Section 1.5) for each
# categorical variable. Used to normalize each dimension to a 0-1 scale
# before summing. The total deprivation score ranges from 0 to
# length(CAT_VARS).
CAT_MAX <- c(water_supply = 5, sanitation = 5, waste_collection = 2,
              electricity = 1, wall_material = 5, occupancy_status = 5,
              education_max = 4, head_occupation = 5)

# [FILL IN] Cluster threshold step. The reference study uses 0.5, which
# balances comparison granularity against cluster size and computational
# stability. Smaller steps increase granularity but may produce very
# small clusters at high deprivation levels.
THRESHOLD_STEP <- 0.5

# ---- 1.5 Categorical variable recoding -------------------------------------
# Implement the recoding logic for your raw categorical variables here.
# The function receives the full data.table and must return it with the
# variables listed in CAT_VARS added, coded as described in Section 1.4:
# 0 = no deprivation, higher integer values = more deprived, up to the
# maximum specified in CAT_MAX.

recode_categorical <- function(dt) {

  # Example: water supply (5 categories, 0 = piped water inside the
  # dwelling, 4 = surface water with no treatment)
  # dt[, water_supply := dplyr::case_when(
  #   water_supply_raw == 1 ~ 0,
  #   water_supply_raw == 2 ~ 1,
  #   water_supply_raw == 3 ~ 2,
  #   water_supply_raw == 4 ~ 3,
  #   water_supply_raw == 5 ~ 4,
  #   TRUE                  ~ 0
  # )]

  # Example: electricity (binary, 0 = has access, 1 = no access)
  # dt[, electricity := fifelse(electricity_raw == 2, 1, 0)]

  # Example: household maximum educational attainment (5 categories,
  # 0 = tertiary or higher, 4 = no schooling)
  # dt[, education_max := dplyr::case_when(
  #   educ_max_raw == 5 ~ 0,
  #   educ_max_raw == 4 ~ 1,
  #   educ_max_raw == 3 ~ 2,
  #   educ_max_raw == 2 ~ 3,
  #   educ_max_raw == 1 ~ 4,
  #   TRUE              ~ 0
  # )]

  # Replace remaining missing values with 0 (conservative: no additional
  # deprivation assumed)
  for (v in CAT_VARS) {
    if (v %in% names(dt)) dt[is.na(get(v)), (v) := 0]
  }

  return(dt)
}


# =============================================================================
# SECTION 2 - DATA LOADING AND DEPRIVATION SCORE (Step 1)
# Do not modify unless adapting to a different file format.
# =============================================================================

cat("Loading data...\n")
dt <- as.data.table(readRDS(INPUT_FILE))
cat(sprintf("Households: %d | Variables: %d\n\n", nrow(dt), ncol(dt)))

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

required_vars <- c(ID_VAR, GEO_VAR, OUTPUTS, INPUTS)
missing_vars  <- setdiff(required_vars, names(dt))
if (length(missing_vars) > 0) {
  stop("Variables not found in data: ", paste(missing_vars, collapse = ", "),
       "\nCheck ID_VAR, GEO_VAR, OUTPUTS and INPUTS in Section 1.")
}
cat("Required model variables found.\n\n")

cat("Recoding categorical variables...\n")
dt <- recode_categorical(dt)

missing_cat <- setdiff(CAT_VARS, names(dt))
if (length(missing_cat) > 0) {
  stop("Categorical variables not found after recoding: ",
       paste(missing_cat, collapse = ", "),
       "\nCheck recode_categorical() in Section 1.5.")
}

# Invert and normalize each categorical variable to a 0-1 scale, then sum
for (v in CAT_VARS) {
  dt[is.na(get(v)) | get(v) < 0, (v) := 0]
  dt[, (v) := (CAT_MAX[v] - as.numeric(get(v))) / CAT_MAX[v]]
}

dt[, deprivation_score := rowSums(.SD), .SDcols = CAT_VARS]

cat(sprintf("Deprivation score: min=%.2f | max=%.2f | mean=%.2f\n",
            min(dt$deprivation_score), max(dt$deprivation_score),
            mean(dt$deprivation_score)))
cat(sprintf("(Maximum possible score: %d, one point per categorical variable)\n\n",
            length(CAT_VARS)))


# =============================================================================
# SECTION 3 - TREAT MISSING VALUES IN MODEL VARIABLES
# =============================================================================

# DEA requires strictly positive inputs and outputs. Non-positive values
# (including zeros and missing values) are replaced by a small technical
# minimum so that the household remains in the model. This is a
# computational artifact, not a substantive value: the original zero is
# preserved in interpretation (e.g., a household with zero income is
# genuinely income-deprived; the value 0.001 is only used to allow the
# solver to run).

for (v in c(INPUTS, OUTPUTS)) {
  dt[, (v) := as.numeric(get(v))]
  dt[is.na(get(v)) | get(v) <= 0, (v) := 0.001]
}

dt[, DMU := as.character(get(ID_VAR))]

if (is.null(GEO_LABELS)) {
  geo_units  <- sort(unique(dt[[GEO_VAR]]))
  GEO_LABELS <- setNames(as.character(geo_units), as.character(geo_units))
}
geo_codes <- sort(names(GEO_LABELS))
cat("Geographic units:", paste(GEO_LABELS, collapse = ", "), "\n\n")


# =============================================================================
# SECTION 4 - MAIN LOOP: BY GEOGRAPHIC UNIT AND BY CLUSTER (Steps 1-3)
# =============================================================================

start_time   <- Sys.time()
list_results <- list()
list_summary <- list()
list_clusters <- list()

for (geo in geo_codes) {

  geo_label <- GEO_LABELS[geo]
  cat(sprintf("==========================================\n"))
  cat(sprintf("UNIT: %s\n", geo_label))

  dt_geo <- dt[get(GEO_VAR) == geo]
  k_min  <- min(dt_geo$deprivation_score)
  k_max  <- max(dt_geo$deprivation_score)

  k_seq <- seq(ceiling(k_min / THRESHOLD_STEP) * THRESHOLD_STEP,
                k_max, by = THRESHOLD_STEP)

  cat(sprintf("  Households: %d | scores: %.2f to %.2f | clusters: %d\n\n",
              nrow(dt_geo), k_min, k_max, length(k_seq)))

  res_geo <- data.table(
    id                = dt_geo[[ID_VAR]],
    deprivation_score = dt_geo$deprivation_score
  )
  setnames(res_geo, "id", ID_VAR)

  cluster_stats <- list()

  for (k in k_seq) {

    idx    <- which(dt_geo$deprivation_score >= k)
    n_clus <- length(idx)
    col_k  <- paste0("k_", gsub("\\.", "_", as.character(k)))

    # Trivial cluster: assign the maximum poverty score
    if (n_clus <= 2) {
      res_geo[idx, (col_k) := 1]
      cat(sprintf("  k=%.2f: %6d households | score=1 (minimum cluster)\n",
                  k, n_clus))
      cluster_stats[[as.character(k)]] <- data.table(
        k = k, n = n_clus, n_frontier = n_clus, pct_frontier = 100,
        score_mean = 1, score_min = 1, score_max = 1
      )
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
        cat(sprintf("  k=%.2f: ERROR (make_deadata) - %s\n", k, e$message))
        NULL
      }
    )
    if (is.null(datadea)) next

    # CCR model, input orientation: for each household, how much could
    # its inputs be proportionally reduced while sustaining the same
    # outputs? Households with few positive resources relative to the
    # outputs they sustain are closest to the poverty frontier
    # (score -> 1).
    model <- tryCatch(
      model_basic(datadea = datadea, orientation = "io", rts = "crs"),
      error = function(e) {
        cat(sprintf("  k=%.2f: ERROR (model_basic) - %s\n", k, e$message))
        NULL
      }
    )
    if (is.null(model)) next

    scores <- efficiencies(model)
    res_geo[idx, (col_k) := as.numeric(scores)]

    n_front <- sum(scores >= 0.999, na.rm = TRUE)
    cat(sprintf("  k=%.2f: %6d households | min=%.4f | max=%.4f | frontier=%d\n",
                k, n_clus,
                round(min(scores, na.rm = TRUE), 4),
                round(max(scores, na.rm = TRUE), 4), n_front))

    cluster_stats[[as.character(k)]] <- data.table(
      k = k, n = n_clus, n_frontier = n_front,
      pct_frontier = round(mean(scores >= 0.999, na.rm = TRUE) * 100, 1),
      score_mean = round(mean(scores, na.rm = TRUE), 4),
      score_min  = round(min(scores, na.rm = TRUE), 4),
      score_max  = round(max(scores, na.rm = TRUE), 4)
    )

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
    res_geo[, c(ID_VAR, "deprivation_score", "poverty_score",
                "cluster_max", "on_frontier"), with = FALSE],
    dt_geo[, c(ID_VAR, OUTPUTS, INPUTS), with = FALSE],
    by = ID_VAR
  )[order(-poverty_score)]

  final_geo[, geo_unit := geo_label]

  n_front <- sum(final_geo$on_frontier == "Yes")
  summary_geo <- data.table(
    geo_unit     = geo_label,
    n_households = nrow(final_geo),
    n_frontier   = n_front,
    pct_frontier = round(n_front / nrow(final_geo) * 100, 2),
    score_mean   = round(mean(final_geo$poverty_score, na.rm = TRUE), 4),
    score_median = round(median(final_geo$poverty_score, na.rm = TRUE), 4),
    score_sd     = round(sd(final_geo$poverty_score, na.rm = TRUE), 4),
    score_cv     = round(sd(final_geo$poverty_score, na.rm = TRUE) /
                          mean(final_geo$poverty_score, na.rm = TRUE), 4)
  )

  cat(sprintf("\n  Done | mean score=%.4f | frontier=%d (%.2f%%)\n\n",
              summary_geo$score_mean, n_front, summary_geo$pct_frontier))

  saveRDS(final_geo, file.path(OUTPUT_DIR,
          sprintf("%s_scores.rds", gsub("[^a-zA-Z0-9]", "_", geo_label))))

  list_results[[geo]]  <- final_geo
  list_summary[[geo]]  <- summary_geo
  list_clusters[[geo]] <- rbindlist(cluster_stats)

  rm(dt_geo, res_geo, final_geo); gc()
}

total_time <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
cat(sprintf("==========================================\n"))
cat(sprintf("TOTAL TIME: %.1f minutes\n\n", as.numeric(total_time)))


# =============================================================================
# SECTION 5 - CONSOLIDATION
# =============================================================================

final_all   <- rbindlist(list_results, fill = TRUE)
summary_all <- rbindlist(list_summary)[order(-score_mean)]

clusters_all <- rbindlist(lapply(names(list_clusters), function(g) {
  d <- list_clusters[[g]]
  d[, geo_unit := GEO_LABELS[g]]
  d
}))

cat("=== RESULTS SUMMARY ===\n\n")
print(summary_all)

saveRDS(final_all, file.path(OUTPUT_DIR, "DEA_MP_H_scores.rds"))
fwrite(final_all,  file.path(OUTPUT_DIR, "DEA_MP_H_scores.csv"))


# =============================================================================
# SECTION 6 - EXPORT TO EXCEL
# =============================================================================

wb <- createWorkbook()
style_header <- createStyle(fgFill = "#1A2E4A", fontColour = "white",
                             textDecoration = "bold", halign = "center")

# Sheet 1: summary by geographic unit
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_all)
addStyle(wb, "Summary", style_header, rows = 1,
         cols = 1:ncol(summary_all), gridExpand = TRUE)

# Sheet 2: household-level scores
addWorksheet(wb, "Household Scores")
export_cols <- c("geo_unit", ID_VAR, "deprivation_score", "poverty_score",
                 "cluster_max", "on_frontier", OUTPUTS, INPUTS)
writeData(wb, "Household Scores",
          final_all[, export_cols, with = FALSE][order(geo_unit, -poverty_score)])
addStyle(wb, "Household Scores", style_header, rows = 1,
         cols = 1:length(export_cols), gridExpand = TRUE)

# Sheet 3: cluster-level statistics
addWorksheet(wb, "Cluster Statistics")
writeData(wb, "Cluster Statistics", clusters_all[order(geo_unit, k)])
addStyle(wb, "Cluster Statistics", style_header, rows = 1,
         cols = 1:ncol(clusters_all), gridExpand = TRUE)

# Sheet 4: empirical validation (frontier vs. others)
addWorksheet(wb, "Validation")
validation <- lapply(list_results, function(d) {
  front  <- d[on_frontier == "Yes"]
  others <- d[on_frontier == "No"]
  row <- data.table(geo_unit = d$geo_unit[1])
  row[, n_frontier := nrow(front)]
  row[, n_others   := nrow(others)]
  for (v in c(OUTPUTS, INPUTS)) {
    row[, paste0(v, "_frontier") := round(mean(front[[v]], na.rm = TRUE), 3)]
    row[, paste0(v, "_others")   := round(mean(others[[v]], na.rm = TRUE), 3)]
  }
  row[, deprivation_frontier := round(mean(front$deprivation_score, na.rm = TRUE), 2)]
  row[, deprivation_others   := round(mean(others$deprivation_score, na.rm = TRUE), 2)]
  row
})
validation_all <- rbindlist(validation, fill = TRUE)
writeData(wb, "Validation", validation_all)
addStyle(wb, "Validation", style_header, rows = 1,
         cols = 1:ncol(validation_all), gridExpand = TRUE)

for (s in names(wb)) {
  setColWidths(wb, s, cols = 1:2, widths = 25)
  tryCatch(setColWidths(wb, s, cols = 3:40, widths = 16), error = function(e) NULL)
}

saveWorkbook(wb, file.path(OUTPUT_DIR, "DEA_MP_H_results.xlsx"), overwrite = TRUE)

cat("\nExport complete.\n")
cat("Output directory:", OUTPUT_DIR, "\n")
cat("\nSheets:\n")
cat("  1. Summary            - poverty scores by geographic unit\n")
cat("  2. Household Scores   - final poverty score for each household\n")
cat("  3. Cluster Statistics - DEA statistics by cluster threshold k\n")
cat("  4. Validation         - frontier vs. others comparison\n")
