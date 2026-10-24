# =============================================================================
# DEA-MP/H: complete reproduction.
#   Rscript run_all.R            all stages (01 to 09) and the tests
#   Rscript run_all.R 05 06      only the listed stages
# On macOS, keep the computer awake during long runs:  caffeinate -i Rscript run_all.R
# See README.md for requirements, runtimes and the description of each stage.
# =============================================================================
stages <- c("01" = "R/01_download.R", "02" = "R/02_build_census2010.R", "03" = "R/03_build_census2022.R",
            "04" = "R/04_numerical_example.R", "05" = "R/05_municipal.R", "06" = "R/06_national.R",
            "07" = "R/07_audit.R", "08" = "R/08_robustness.R", "09" = "R/09_manifest.R")
needed <- c("data.table", "rDEA", "lpSolve")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install the packages: ", paste(missing, collapse = ", "))
if (!nzchar(Sys.which("unzip")) || !nzchar(Sys.which("awk"))) stop("The commands unzip and awk must be on the PATH.")
args <- commandArgs(trailingOnly = TRUE)
run <- if (length(args)) stages[args] else stages
if (anyNA(run)) stop("Unknown stage. Valid stages: ", paste(names(stages), collapse = ", "))
for (k in names(run)) {
  t0 <- Sys.time(); cat("\n==== stage", k, "|", run[[k]], "|", format(t0), "\n")
  source(run[[k]], local = new.env())
  cat("==== stage", k, "finished in", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
}
if (!length(args)) source("tests/check_results.R", local = new.env())
