# =============================================================================
# DEA-MP/H: configuration
# Edit the paths below or set the environment variables before running.
# =============================================================================

PATHS <- list(
  # Folder with the IBGE 2010 sample microdata zips (AC.zip ... TO.zip, SP1.zip, SP2_RM.zip)
  census2010_dir = Sys.getenv("DEAMPH_CENSUS2010_DIR", "raw/census2010"),
  # IBGE 2022 public sample microdata, file Todas_as_UFs.zip (csv version)
  census2022_zip = Sys.getenv("DEAMPH_CENSUS2022_ZIP", "raw/census2022/Todas_as_UFs.zip"),
  work   = Sys.getenv("DEAMPH_WORK_DIR", "work"),
  output = Sys.getenv("DEAMPH_OUTPUT_DIR", "output")
)

URLS <- list(
  census2010 = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/",
  census2022 = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Microdados_e_Areas_de_Ponderacao/Microdados_de_acesso_Publico/csv/Todas_as_UFs.zip"
)

PARAM <- list(
  delta         = 0.5,    # spacing of the comparison levels, tau_k = k * delta
  min_ref       = 27L,    # minimum number of reference households in an admissible group
  alpha         = 0.05,   # floor on the share of each resource in the household's weighted resources
  zero_constant = 0.001,  # replacement for zero resources and demands (income zero becomes R$ 1)
  kappa         = 2,      # relative line for H: score at least kappa times the anchor median
  # IPCA number index, July 2010 and July 2022 (IBGE, SIDRA table 1737); 2010 income in reais of July 2022
  ipca_jul2010  = 3111.05,
  ipca_jul2022  = 6411.95,
  # screening of atypical reference households: input-oriented super-efficiency cut-off
  # (Bogetoft and Otto, 2011, Sec. 5.13, suggest values such as 3 or 4); Inf switches it off
  screen        = 3
)

# Independent re-estimations (interference tests, draws, grids) run in parallel on
# macOS and Linux. Results do not depend on the number of cores. Set DEAMPH_CORES=1 to disable.
CORES <- as.integer(Sys.getenv("DEAMPH_CORES", max(1L, min(4L, parallel::detectCores() - 1L))))

SEEDS <- list(bootstrap_municipal = 20261022L, solver_check = 19L,
              monotonicity = 1L, interference_directed = 18L, interference_random = 7L,
              subgroup = 11L, half_draws = 2029:2048, bootstrap_national = 20261023L,
              interference_other = 181L)

UFS <- c(11:17, 21:29, 31:33, 35, 41:43, 50:53)

# Eight municipalities of the municipal illustration (IBGE codes) and their 2010 IDHM
# (Atlas do Desenvolvimento Humano no Brasil; PNUD, Ipea & FJP, 2013).
MUNICIPALITIES <- data.table::data.table(
  nome      = c("Itabuna-BA", "Paulista-PE", "Olinda-PE", "Mossoro-RN", "Campina Grande-PB",
                "Maringa-PR", "Blumenau-SC", "Caxias do Sul-RS"),
  uf        = c(29, 26, 26, 24, 25, 41, 42, 43),
  municipio = c(14802, 10707, 9600, 8003, 4009, 15200, 2404, 5108),
  IDHM      = c(0.712, 0.732, 0.735, 0.720, 0.720, 0.808, 0.806, 0.782))

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
for (p in c(PATHS$work, PATHS$output)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
