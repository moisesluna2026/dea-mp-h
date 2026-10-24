# DEA-MP/H: household multidimensional deprivation with a fixed DEA reference

Replication package accompanying manuscript version 11 of *Pesos endógenos e monotonicidade na mensuração da pobreza multidimensional* (Endogenous weights and monotonicity in multidimensional poverty measurement).

The package rebuilds every number, table and robustness check of the article from the
public microdata of the Brazilian Demographic Census (IBGE, 2010 and 2022). No
intermediate file produced by the author is needed: the scripts download the
official files, build the household bases, estimate the index and run the audits.

## What the method does

Each household is described by **resources** (income, literate residents, adults with
secondary education, formal jobs, bedrooms, bathrooms) and **demands** (residents,
residents aged 15 to 64). A data envelopment analysis (DEA) model, input oriented with
constant returns to scale, measures how far the resources of a household fall short of
those of reference households on the most adverse frontier within their admissible group. The score θ is the
deprivation of the household: θ = 1 on the frontier, θ < 1 when the household has more
resources than the frontier would require for its demands. A household that is not part of
the reference can be more deprived than the frontier (θ > 1); the index truncates the score at 1.

Four features make the scores comparable across households, places and years:

1. **Fixed reference.** The frontiers are computed once, on all eligible households of a base
   year (2010), and then frozen. A household evaluated later, or in another place, is compared
   with the same frontier. A change in one household therefore never changes the score of
   another household (non-interference).
2. **Comparison groups by categorical deprivation (Banker and Morey).** A categorical score
   S (education, water, sewage, garbage, walls, tenure) defines nested groups: a household
   is compared with reference households at or above the lower limit of its admissible
   categorical band. The group used, k*(o), is the most restricted compatible band
   with at least 27 reference households; it can include scores below the evaluated household's score.
3. **Share floor.** Each resource must account for at least α = 5% of the household's
   weighted resources. With α > 0 the score is strictly monotone: losing any resource
   raises deprivation.
4. **Screening of atypical reference households.** A household whose input-oriented
   super-efficiency within its comparison group exceeds 3 pushes the frontier far beyond all
   other households (for example, 21 residents with no income, no literate resident and no
   bathroom, whose zero resources become 0.001). Such households are removed from the
   reference in rounds: after each round of removals every frontier is rebuilt on the
   remaining households, until no frontier household exceeds the cut-off. Only the final
   reference and its frontiers are frozen, so the groups stay nested and no removed household
   remains in a frontier. Removed households are still evaluated and counted in the index.

The index of a population is the weighted mean of min(θ, 1). It is read in relative terms
against the weighted median of the base year (Townsend's relative deprivation): T = M / m̃0,
and H is the share of households with a score of at least twice m̃0.

## Requirements

* R 4.2 or later, with the packages `data.table`, `rDEA` (needs the GLPK library) and `lpSolve`.
  ```r
  install.packages(c("data.table", "rDEA", "lpSolve"))
  ```
  On macOS, if `rDEA` fails to install, install GLPK first (`brew install glpk`).
* The command-line tools `unzip` and `awk` (present on macOS and Linux).
* About 8 GB of RAM and 10 GB of free disk space.
* Internet access to `ftp.ibge.gov.br` for stage 01 (or the files placed by hand, see below).

## Running

```sh
git clone https://github.com/moisesluna2026/dea-mp-h.git dea-mph && cd dea-mph
Rscript run_all.R                    # all stages, then the checks
caffeinate -i Rscript run_all.R      # macOS: keeps the computer awake
Rscript run_all.R 04                 # only the numerical example (seconds, needs only lpSolve)
Rscript run_all.R 05 07              # selected stages
```

Paths can be changed in `R/config.R` or with environment variables:

| Variable | Default | Content |
|---|---|---|
| `DEAMPH_CENSUS2010_DIR` | `raw/census2010` | the 28 zip files of the 2010 sample (AC.zip ... TO.zip, SP1.zip, SP2_RM.zip) and Documentacao.zip |
| `DEAMPH_CENSUS2022_ZIP` | `raw/census2022/Todas_as_UFs.zip` | public sample microdata of 2022, csv version |
| `DEAMPH_WORK_DIR` | `work` | intermediate files (household bases, frontiers) |
| `DEAMPH_OUTPUT_DIR` | `output` | results |

## Data

| Census | Source | Files |
|---|---|---|
| 2010 | IBGE, Censo Demográfico 2010, Resultados Gerais da Amostra, microdados. <https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/> | one zip per UF (São Paulo in two parts) with fixed-width files `Amostra_Domicilios_*.txt` and `Amostra_Pessoas_*.txt`; layout in `Documentacao.zip` |
| 2022 | IBGE, Censo Demográfico 2022, microdados da amostra de acesso público. <https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Microdados_e_Areas_de_Ponderacao/Microdados_de_acesso_Publico/csv/> | `Todas_as_UFs.zip` with `Domicilios_<UF>_*.csv` and `Pessoas_<UF>_*.csv` |
| IDHM 2010 | Atlas do Desenvolvimento Humano no Brasil (PNUD, Ipea and FJP, 2013) | eight values, typed in `R/config.R` |

IBGE releases corrections to the microdata. The release used in the article is identified
by the SHA-256 checksums in `output/inputs_sha256.csv`; a different release may change
the results slightly.

Variable definitions, the harmonization between 2010 and 2022 and the eligibility rule
are in [`docs/variables.md`](docs/variables.md).

## Stages and outputs

| Stage | Script | What it does | Main outputs | Approximate time |
|---|---|---|---|---|
| 01 | `01_download.R` | downloads the microdata; records SHA-256 of inputs | `inputs_sha256.csv` | depends on connection |
| 02 | `02_build_census2010.R` | household base of 2010 from the fixed-width files; checks the person count of every household against V0401 | `work/households_2010.rds`, `base_2010_counts.csv` | 10 min |
| 03 | `03_build_census2022.R` | household base of 2022 | `work/households_2022.rds`, `base_2022_counts.csv` | 3 min |
| 04 | `04_numerical_example.R` | example with four households (Section 3 of the article), including a frontier household that becomes poorer under a local and under the fixed reference | `numerical_example/` | seconds |
| 05 | `05_municipal.R` | eight municipalities, complete 2010 reference with screening; local frontiers for comparison | `municipal/table1_descriptives.csv`, `municipal/table2_municipalities.csv`, `municipal/spearman_IDHM.csv`, `municipal/screened_households.csv` | minutes |
| 06 | `06_national.R` | 27 UFs: every eligible household of 2010 forms the reference (with screening); every eligible household of 2010 and 2022 is evaluated; dominance | `national/ufs.csv`, `national/regions.csv`, `national/brazil.csv`, `national/dominance.csv`, `national/screened_households.csv` | 10 min |
| 07 | `07_audit.R` | solver check, monotonicity, non-interference (directed shocks to income, bathrooms, formal jobs and residents; random shocks), subgroup consistency | `audit/` | minutes |
| 08 | `08_robustness.R` | bootstrap, zero constant, screening cut-offs, alternative references, half-sample draws, grids of α and δ, national sensitivity | `robustness/` | 30 to 60 min |
| 09 | `09_manifest.R` | session information, parameters and SHA-256 of all outputs | `session_info.txt`, `parameters.csv`, `outputs_sha256.csv` | seconds |

Times refer to a 2020s laptop (Apple M-series, 8 GB) and are indicative. Independent re-estimations
run in parallel on macOS and Linux (`DEAMPH_CORES`, default: up to 4 cores); the results do not
depend on the number of cores.

## Parameters and random seeds

All parameters are in `R/config.R`: level spacing δ = 0.5; minimum reference group of 27
households; share floor α = 0.05; zero replacement 0.001; relative line κ = 2; screening
cut-off 3 for super-efficiency. The main estimates involve no random draw: the references
are complete and every eligible household is evaluated. Random draws appear only in the
bootstrap, the audits and the half-sample robustness check; each sets its own seed (listed
in `SEEDS`), and households are sorted by UF, municipality and identifier first, so that the
draws do not depend on the order of the input files.

## Checks

`tests/check_results.R` runs after the full pipeline and fails if any of these does not hold:
the batch solver (GLPK, virtual shares) agrees with an independent solver (lpSolve, original
multiplier form) to 1e-8; no score falls when a resource is halved, and every score rises
when α > 0; under the fixed reference, the score of a household moves in the direction of a
change in its own income; the mean of a municipality does not fall when 10% of its households
lose half of their income; the numerical example reproduces the values printed in the article.
When `tests/expected_values.csv` is present, the main estimates are also compared with the
values printed in the article.

## Implementation notes

* **Frontiers.** Because the comparison groups are nested, the frontier of level k is
  F_k = E(F_{k+1} ∪ band_k): households that are interior in a more restricted group stay
  interior when the group grows. Frontiers are computed from the most deprived level down,
  with constraint generation (`build_frontiers` in `R/functions.R`).
* **Screening.** Each round (i) builds every frontier on the current reference, with no
  removal during the pass, so that F_k = E(F_{k+1} U band_k) holds; (ii) computes the
  super-efficiency (Andersen and Petersen) of the frontier households of each comparison
  group with at least 27 reference households, against the whole active group without the
  household itself; (iii) removes at once every household above the cut-off (tolerance
  1e-6, so values equal to the cut-off up to rounding are kept). Rounds repeat until none
  is removed. The super-efficiency problem is solved in multiplier form by constraint
  generation, checking every household of the group against the optimal weights. Using
  super-efficiency to flag atypical observations follows Bogetoft and Otto (2011, Sec. 5.13).
  The cut-off of 3 and the alternatives 2, 4 and no screening are reported in `robustness/`;
  `audit/categorical_monotonicity.csv` checks, for the main reference and for cut-offs 2
  and 4, that no score falls when a household moves to a more restricted group.
* **Batch solution.** With the change of variables p_r = v_r x_ro and q_s = u_s y_so, the
  normalization, objective and share floor of model (1) are identical for every evaluated
  household; only the coefficients of the frontier constraints change. All households of one
  level are then solved in one call to `rDEA::multi_glpk_solve_LP`. The transformation is
  exact, and stage 07 verifies it against the original form.
* **Bootstrap.** Households are resampled within municipality (or UF and year) with the
  reference and the individual scores held fixed. This does not reproduce the census sample
  design (strata and weighting areas), so the intervals are likely too narrow.

## Citation

See `CITATION.cff`. The archived version and its DOI are linked from the GitHub release.

## License

Code: MIT (see `LICENSE`). The census microdata are public data distributed by IBGE and are
not redistributed in this repository.

## Release 1.1.0

This release accompanies manuscript version 11 (24 October 2026). It contains the code, variable documentation, generated results and computational checks. The current execution log records completion of the robustness calculations and successful checks, including categorical monotonicity and exclusion from all stored frontiers. Public Census microdata and individual intermediate RDS bases are not included; the scripts download the official inputs from IBGE.
