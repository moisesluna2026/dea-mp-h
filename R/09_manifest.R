# =============================================================================
# Stage 09. Record of the computing environment and SHA-256 manifest of all outputs.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
writeLines(capture.output(sessionInfo()), file.path(PATHS$output, "session_info.txt"))
fwrite(data.table(parameter = names(PARAM), value = vapply(PARAM, function(x) paste(x, collapse = " "), "")),
       file.path(PATHS$output, "parameters.csv"))
files <- list.files(PATHS$output, recursive = TRUE, full.names = TRUE)
files <- files[basename(files) != "outputs_sha256.csv"]
fwrite(data.table(file = sub(paste0("^", PATHS$output, "/"), "", files), sha256 = file_sha256(files)),
       file.path(PATHS$output, "outputs_sha256.csv"))
log_msg("stage 09 complete:", length(files), "output files listed")
