# Data

This directory contains the source and processed datasets used in the
ICVLF Bolivia research project.

## Raw data

The analytical source workbook is:

`data/raw/Datos RL SB.xlsm`

### Source

Autoridad de Supervisión del Sistema Financiero (ASFI), Bolivia.

### Frequency

Monthly.

### Analytical sample

January 2010 – December 2025.

### Expected observations

192 monthly observations.

### Unit of analysis

Aggregated banking-system indicators reported under the category
`Todos` in the source used by the project.

## Data integrity

Before constructing the ICVLF, the analytical pipeline verifies:

- duplicate dates;
- missing months;
- exact monthly sequence;
- numerical parsing;
- non-finite observations;
- indicator availability;
- coverage thresholds.

The model stops if the expected temporal structure is not satisfied.

## Processed datasets

The following machine-readable datasets are generated automatically:

- `icvlf_monthly.csv`
- `dimensions_monthly.csv`
- `indicators_monthly.csv`
- `proxies_monthly.csv`
- `validation_global.csv`
- `validation_subperiods.csv`
- `validation_hac_differences.csv`
- `robustness_leave_indicator.csv`
- `robustness_leave_dimension.csv`
- `structural_breaks.csv`
- `stress_scenarios.csv`

The public variable dictionary is:

`data/dictionary.csv`

Processed files should not be edited manually.

Regenerate them with:

```bash
Rscript run_all.R