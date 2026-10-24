# =============================================================================
# Stage 01. Download the IBGE microdata (about 1.2 GB for 2010 and 1.0 GB for 2022).
# Files already present are not downloaded again. SHA-256 checksums of every
# input file are written to output/inputs_sha256.csv, so that a reader can
# confirm that the same release of the microdata was used.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
options(timeout = max(7200, getOption("timeout")))

files2010 <- c(paste0(c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT",
                        "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE",
                        "SP1", "SP2_RM", "TO"), ".zip"), "Documentacao.zip")
dir.create(PATHS$census2010_dir, recursive = TRUE, showWarnings = FALSE)
for (f in files2010) {
  dest <- file.path(PATHS$census2010_dir, f)
  if (!file.exists(dest)) { log_msg("downloading", f); download.file(paste0(URLS$census2010, f), dest, mode = "wb") }
}

dir.create(dirname(PATHS$census2022_zip), recursive = TRUE, showWarnings = FALSE)
if (!file.exists(PATHS$census2022_zip)) {
  log_msg("downloading Todas_as_UFs.zip (2022)")
  download.file(URLS$census2022, PATHS$census2022_zip, mode = "wb")
}

inputs <- c(file.path(PATHS$census2010_dir, files2010), PATHS$census2022_zip)
fwrite(data.table(file = basename(inputs), bytes = file.size(inputs), sha256 = file_sha256(inputs)),
       file.path(PATHS$output, "inputs_sha256.csv"))
log_msg("stage 01 complete")
