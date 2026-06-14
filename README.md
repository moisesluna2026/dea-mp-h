# DEA-MP/H

**Household-level multidimensional poverty measurement via Data Envelopment Analysis**

DEA-MP/H is part of the DEA-MP family of methods for measuring multidimensional poverty using Data Envelopment Analysis (DEA). This variant operates at the household level, applying DEA directly to household-level microdata without depending on the Alkire-Foster method.

## Method overview

The method combines two steps, applied separately to each geographic unit (e.g., municipality, region, country):

1. **Identification.** A categorical deprivation score is computed for each household from a set of dimensions (e.g., water supply, sanitation, electricity, education, housing). Each dimension is normalized to a 0-1 scale and summed, producing a total score from 0 to the number of dimensions. Comparison clusters are defined by thresholds in steps of 0.5, following an ordinal dominance restriction adapted from Banker and Morey (1986): a household is only compared with peers in an equal or worse overall deprivation position.

2. **Graduation.** Within each cluster, an input-oriented CCR model (Charnes, Cooper and Rhodes, 1978) is estimated using the [deaR](https://cran.r-project.org/package=deaR) package. Outputs represent the people to be supported by the household (e.g., household size, working-age adults); inputs represent positive household resources (income, education, employment, housing standard), expressed as absolute counts.

Each household appears in multiple clusters. Its final poverty score is the **maximum** efficiency score obtained across all clusters in which it appears, ranging from 0 to 1. A score of 1 indicates the household is on the maximum-poverty frontier within at least one cluster.

## Repository structure

```
dea-mp-h/
├── applied/
│   └── DEA-MP-H_applied_census2010.R   # Replication script for the empirical
│                                          application reported in the article
│                                          (2010 Brazilian Demographic Census,
│                                          8 municipalities)
└── template/
    └── DEA-MP-H_template.R             # Generic, configurable template for
                                           applying the method to other datasets
```

**Use `applied/`** to reproduce the results reported in the article.

**Use `template/`** to apply the method to your own data. All fields requiring adaptation are marked with `[FILL IN]` and documented in the script.

## Requirements

R packages: [deaR](https://cran.r-project.org/package=deaR), [data.table](https://cran.r-project.org/package=data.table), [openxlsx](https://cran.r-project.org/package=openxlsx)

```r
install.packages(c("deaR", "data.table", "openxlsx"))
```

## The DEA-MP family

DEA-MP (Data Envelopment Analysis for Multidimensional Poverty) is a family of methods sharing a common DEA-based approach to multidimensional poverty measurement, with variants identified by level of analysis and methodological structure:

- **DEA-MP/R-AF** - regional/territorial level, integrated with the Alkire-Foster method
- **DEA-MP/H** - household level, autonomous (this repository)

Future extensions follow the pattern DEA-MP/H-[MODEL], e.g., DEA-MP/H-BCC for variable returns to scale.

## Citation

If you use this method or code, please cite:

[Citation to be added upon publication]

## License

This code is released under the MIT License. See [LICENSE](LICENSE) for details.

## References

Alkire, S., & Foster, J. (2011). Counting and multidimensional poverty measurement. *Journal of Public Economics*, 95(7-8), 476-487.

Banker, R. D., & Morey, R. C. (1986). Efficiency analysis for exogenously fixed inputs and outputs. *Operations Research*, 34(4), 513-521.

Charnes, A., Cooper, W. W., & Rhodes, E. (1978). Measuring the efficiency of decision making units. *European Journal of Operational Research*, 2(6), 429-444.
