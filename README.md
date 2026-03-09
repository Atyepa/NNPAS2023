# NNPAS 2023 Data Visualisation

An interactive Shiny app for exploring results from the **2023 Australian National Nutrition and Physical Activity Survey (NNPAS)**, published by the Australian Bureau of Statistics.

**Live app:** https://atyepa.shinyapps.io/NNPAS2023/

---

## What it shows

The app provides interactive bar and column charts with 95% confidence intervals for three data tables sourced directly from the ABS NNPAS 2023 data cube:

| Table | Description |
|---|---|
| **AUSNUT** | Mean nutrient intakes by food group (major, sub-major, or sub-major within major) |
| **Nutrients** | Mean daily nutrient intakes, with 2011–12 and 2023 comparison |
| **Macro** | Percent of dietary energy from macronutrients, with 2011–12 and 2023 comparison |

---

## Features

- **Sex and age group filters** — filter by Males, Females, or Persons; select any combination of age groups
- **Food group selection** — choose Major food groups, Sub-major food groups, or Sub-major foods within a selected Major group (with individual food deselection)
- **Nutrient / macronutrient selection** — pick from means, medians, energy values, and % energy measures
- **Stack / unstack** — toggle stacked column charts; when stacked, foods or macronutrients are always the stacked series (age group / sex on x-axis)
- **Swap x-axis / series group** — flip the x-axis and colour grouping variable (respects stacking constraint)
- **Error bars** — optional 95% confidence intervals on charts; automatically hidden when stacking
- **Data labels** — toggleable value labels on bars
- **Data table and download** — filtered data shown as a wide table; when error bars are enabled, a 95% CI (±) row is appended below each estimate row; downloadable as Excel (.xlsx)
- **Year comparison** — for Nutrients and Macro tables, select one or both survey years (2011–12 and 2023)

---

## Data sources

All data are read directly from the ABS NNPAS 2023 data cube and classification templates hosted on GitHub:

- ABS data cube: https://www.abs.gov.au/statistics/health/food-and-nutrition/national-nutrition-and-physical-activity-survey/2023
- AUSNUT food group classification: `AUSNUT23_class.xlsx`
- Nutrient classification: `NUT23_class.xlsx`
- Macronutrient classification: `Macro23_class.xlsx`

Confidence intervals are computed as `val ± 1.96 × (RSE/100) × val` using the relative standard errors provided in the data cube.

---

## R packages

`shiny`, `shinydashboard`, `shinyWidgets`, `shinythemes`, `highcharter`, `tidyverse`, `readxl`, `writexl`, `DT`

---

## Running locally

```r
# Install dependencies if needed
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "shinythemes",
                   "highcharter", "tidyverse", "readxl", "writexl", "DT"))

# Run the app
shiny::runApp("NNPAS data visCI.R")
```
