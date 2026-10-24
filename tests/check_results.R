# =============================================================================
# Checks run after the full pipeline. They test properties that must hold for any
# data (solver agreement, monotonicity, non-interference under the fixed reference)
# and, when tests/expected_values.csv is present, compare the main estimates with
# the values reported in the article.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- PATHS$output; ok <- TRUE
check <- function(label, cond) { cat(if (isTRUE(cond)) "PASS" else "FAIL", "|", label, "\n"); if (!isTRUE(cond)) ok <<- FALSE }

sc <- fread(file.path(O, "audit", "solver_check.csv"))
check("batch GLPK equals lpSolve in the original multiplier form (max abs diff < 1e-8)", max(abs(sc$glpk - sc$lpsolve)) < 1e-8)
mo <- fread(file.path(O, "audit", "monotonicity.csv"))
check("weak monotonicity: no score falls when a resource is halved", all(mo$change > -1e-9))
check("strict monotonicity with alpha > 0: every score rises", all(mo[alpha > 0, change] > 0))
di <- fread(file.path(O, "audit", "interference_directed.csv"))
check("fixed reference: own score moves in the direction of the change",
      all(di[type == "income halved", fixed_own_after >= fixed_own_before]) &&
      all(di[type == "income raised", fixed_own_after <= fixed_own_before]))
do <- fread(file.path(O, "audit", "interference_directed_other.csv"))
check("fixed reference: own score does not fall after losing a bathroom or formal job or gaining a resident",
      all(do$fixed_own_after >= do$fixed_own_before - 1e-9))
sg <- fread(file.path(O, "audit", "subgroup_consistency.csv"))
check("subgroup consistency under the fixed reference", sg$fixed_itabuna_after >= sg$fixed_itabuna_before &&
      sg$fixed_eight_after >= sg$fixed_eight_before)
cm <- fread(file.path(O, "audit", "categorical_monotonicity.csv"))
check("categorical monotonicity: no score falls when the household moves to a more restricted group (main, cut-offs 2 and 4)",
      all(c("main", "screen_2", "screen_4") %in% cm$reference) && all(cm$falls == 0))
for (lvl in c("municipal", "national"))
  check(paste0(lvl, ": final frontiers contain no household removed by the screening"),
        file.exists(file.path(O, lvl, "super_efficiency_final_frontiers.csv")) &&
        !any(fread(file.path(O, lvl, "super_efficiency_final_frontiers.csv"))$household %in%
             fread(file.path(O, lvl, "screened_households.csv"))$household))
ex <- fread(file.path(O, "numerical_example", "example_summary.csv"))
check("numerical example: M = 0.834375", abs(ex$M - 0.834375) < 1e-9)

if (file.exists("tests/expected_values.csv")) {
  ev <- fread("tests/expected_values.csv")          # columns: file, key_col, key, value_col, expected, tolerance
  for (i in seq_len(nrow(ev))) {
    t <- fread(file.path(O, ev$file[i]))
    v <- if (nzchar(ev$key_col[i])) t[as.character(get(ev$key_col[i])) == ev$key[i], get(ev$value_col[i])] else t[[ev$value_col[i]]][1]
    check(sprintf("%s: %s %s = %s", ev$file[i], ev$key[i], ev$value_col[i], ev$expected[i]),
          length(v) == 1 && abs(v - ev$expected[i]) <= ev$tolerance[i])
  }
}
if (!ok) stop("At least one check failed.") else cat("All checks passed.\n")
