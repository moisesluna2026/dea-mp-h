# Variables, harmonization and eligibility

All variables are built at the household level for permanent private occupied
households. Codes refer to the IBGE sample microdata of the 2010 Census
(`Amostra_Domicilios` and `Amostra_Pessoas` files) and to the public sample
microdata of the 2022 Census (`Domicilios` and `Pessoas` files).

## Model variables

| Role | Variable | 2010 | 2022 |
|---|---|---|---|
| Demand | residents (`n_moradores`) | number of person records of the household | number of person records of the household |
| Demand | residents aged 15 to 64 (`n_adultos_em_idade_ativa`) | V6036 in 15..64 | P0180 in groups 4..13 |
| Resource | household income, reais of July 2022 (`num_renda_total`) | V6529 x IPCA factor | D0350 |
| Resource | literate residents (`n_alfabetizados`) | V0627 = 1 | P0640 = 1 |
| Resource | residents aged 15+ with complete secondary education or more (`n_adultos_educ_media_ou_mais`) | V6036 >= 15 and V6400 in {3, 4} | P0180 in groups 4..17 and P0770 in {3, 4} |
| Resource | employed in formal jobs (`n_ocupados_formais`) | V6910 = 1 and V0648 in {1, 2, 3} | P0950 = 1 and (P1000 = 1 or P0990 in {2, 4}) |
| Resource | rooms used as bedrooms (`n_dormitorios`) | V0204 | D0230 |
| Resource | exclusive bathrooms, capped at 9 (`n_banheiros`) | min(V0205, 9) | min(D0280, 9) |

Formal jobs: formal employment contract, military, or statutory civil servant.
Income: the reference dates of both censuses are 31 July. Income of 2010 is
converted to reais of July 2022 with the IPCA number index (IBGE, SIDRA table 1737):
6411.95 / 3111.05 = 2.0610.

Before estimation (function `prepare_matrices`): income of zero or less becomes
R$ 1 and income is expressed in thousands of reais; any other zero resource or
demand becomes 0.001. The CCR model requires strictly positive data.

## Categorical deprivation score

Six dimensions, each coded "higher = better" and transformed into deprivation
`(best - observed) / (best - worst)`, so each lies in [0, 1] and the score S lies in [0, 6].

| Dimension | Categories (best to worst) | 2010 | 2022 |
|---|---|---|---|
| Highest education among residents (`educ_max`) | 3 complete tertiary, 2 complete secondary, 1 complete primary, 0 none or incomplete primary | V6400 | P0770 |
| Water (`cat_agua`) | 5 general network piped; 4 well or spring piped; 3 network, well or spring not piped; 2 other source piped; 1 other source not piped | V0208, V0209 | D0260, D0300 |
| Sewage (`cat_esgoto`) | 5 network; 4 septic tank; 3 rudimentary pit; 2 ditch or other; 1 river, lake or sea; 0 no toilet | V0206, V0207 | D0250 |
| Garbage (`cat_lixo`) | 2 collected directly; 1 collected in a container; 0 other destination | V0210 | D0310 |
| External walls (`cat_parede`) | 5 masonry or rammed earth with coating; 4 masonry without coating; 3 suitable wood; 2 reused wood; 1 rammed earth without coating; 0 straw, other or none | V0202 | D0210 |
| Tenure (`cat_cond_ocup`) | 5 owned and paid; 4 owned, paying; 3 rented; 2 provided by employer; 1 other | V0201 | D0190 |

The exact code-by-code mapping is in `R/02_build_census2010.R` and `R/03_build_census2022.R`.

## Harmonization between 2010 and 2022

| Item | Decision for both years | Reason |
|---|---|---|
| Adult for education | 15 years or more | the 2022 public file has age only in five-year groups; 15 is a group limit |
| Working-age adult | 15 to 64 years | exact in both years |
| Sewage | septic tank = 4 whether or not connected to a network | 2010 does not separate the two cases |
| Walls | masonry or rammed earth with coating = 5 | 2022 does not separate the two cases |
| Bathrooms | capped at 9 | cap imposed in the 2022 public file |
| Electricity | not used | not asked in 2022 |
| Territorial unit for 2022 | UF | the 2022 public file has no municipality code |

## Eligibility

A household is eligible when all model variables and all six score dimensions are
observed. The eligibility rule also requires the number of residents without severe
disability and the occupation of the household head, two variables that were tested
during the study and are not part of the final specification. The rule is kept so
that the bases are identical to those used in the article.

In 2022, households with at least one suppressed age (P0180 = 99) are excluded from
the main estimates, because their number of working-age adults is unknown. They are
scored separately in `output/national/age_suppression_sensitivity.csv`.
