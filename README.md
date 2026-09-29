# ECO3 – Gradient of Anthropogenic Threat

This repository contains the data and R scripts used to construct, assess, and integrate a spatial gradient of anthropogenic threat across Mediterranean ecosystems of Central Chile.

The workflow was developed during the initial phase of FONDECYT Regular Project No. 1251095 and combines spatial indicators of anthropogenic pressure, field-based biodiversity data, and anonymous questionnaire-based assessments of local threats.

The repository is organized to document the analytical workflow from the construction of the spatial gradient to the final integration and selection of study sites.

---

## Repository structure

### `01_spatial/`

Contains the final spatial products used in the analyses:

- `Road_density_ECO3.tif` – normalized road-density layer.
- `Loss_Nature_ECO3.tif` – proportion of natural ecosystems converted to anthropogenic land cover between 2000 and 2024.
- `GHM_ECO3.tif` – continuous Global Human Modification layer.
- `gAT_ECO3.tif` – continuous gradient of anthropogenic threat obtained by integrating road density, loss of natural ecosystems, and Global Human Modification.
- `gAT_quintiles_ECO3.tif` – classification of the continuous gAT into five empirical quintiles.

All spatial products are aligned to a common 1-km grid and restricted to the ECO3 study area.

---

### `02_field/`

Contains the field data used for the biodiversity analyses and information on the plots used during the field assessment.

- `StructurePlots_Clean.csv` – cleaned vegetation structure dataset used for biodiversity analyses.
- `0_sitio.xlsx - Sheet1.csv` – record of plots visited during the field assessment.
- `parcels_fieldcheck.gpkg` – spatial location of field-assessment plots.

The biodiversity workflow considers woody individuals with DBH ≥ 3 cm and calculates diversity metrics at the site level.

---

### `03_questionnaire/`

Contains the anonymized questionnaire dataset used to characterize locally observed anthropogenic threats.

- `questionnaire_anonymized_raw.csv` – anonymized responses from the field threat assessment.

Questionnaire responses are standardized to a common 0–1 scale and subsequently aggregated by threat category, evaluator, and study site.

---

### `04_integrated_data/`

Contains the site-level dataset used as input for the final integrative analyses.

- `master_raw_FINAL.csv`

This file consolidates information generated independently by the preceding spatial, biodiversity, and questionnaire workflows into a single site-level dataset. The variables contained in this dataset can therefore be reconstructed from the corresponding upstream analyses.

---

### `05_Scripts/`

Contains the R scripts used throughout the analytical workflow.

Scripts are numbered according to the order of the workflow:

1. `01_RoadDensity_ECO3.R`
2. `02_LossNature_ECO3.R`
3. `03_GHM_ECO3.R`
4. `04_gAT_ECO3.R`
5. `05_Diversity_Analysis_ECO3.R`
6. `06_Threats_questionnaire_Analysis_ECO3.R`
7. `07_Final_integration_Analysis_ECO3.R`

---

## Analysis workflow

The analytical workflow consists of four main stages:

**Spatial indicators → Anthropogenic threat gradient → Field assessment → Final integration**

### 1. Spatial indicators

Three complementary spatial indicators are used to characterize anthropogenic pressure:

**Road density**

Road density is calculated from the Chilean national road network using a 1-km grid and a 5 × 5 km moving window. Values are normalized to a 0–1 scale using the 2nd and 98th percentiles.

See:

`05_Scripts/01_RoadDensity_ECO3.R`

**Loss of natural ecosystems**

Loss of natural ecosystems is derived from MapBiomas Chile Collection 2 by identifying pixels classified as natural in 2000 that transitioned to anthropogenic land-cover classes by 2024.

The resulting proportion of converted natural land is aggregated to approximately 1-km resolution and aligned with the road-density grid.

See:

`05_Scripts/02_LossNature_ECO3.R`

**Global Human Modification**

The continuous Global Human Modification (gHM) index is resampled and aligned to the same 1-km spatial grid while retaining its original continuous 0–1 scale.

See:

`05_Scripts/03_GHM_ECO3.R`

---

### 2. Gradient of Anthropogenic Threat (gAT)

The Gradient of Anthropogenic Threat (gAT) is calculated as the arithmetic mean of the three normalized spatial indicators:

`gAT = (Road density + Loss of natural ecosystems + Global Human Modification) / 3`

Urban areas and a 1-km surrounding buffer are excluded from the study domain.

The resulting continuous gradient is subsequently divided into five empirical quintiles representing progressively higher levels of anthropogenic pressure.

See:

`05_Scripts/04_gAT_ECO3.R`

---

### 3. Field assessment

Candidate sites distributed along the spatial gradient were evaluated using two complementary sources of field information.

#### Biodiversity

Vegetation data are used to characterize woody plant diversity at each site.

The workflow calculates Hill diversity numbers for:

- q = 0
- q = 1
- q = 2

Diversity is evaluated for the complete assemblage and separately for native and exotic species. Diversity estimates are standardized using sample coverage where applicable.

See:

`05_Scripts/05_Diversity_Analysis_ECO3.R`

#### Locally observed anthropogenic threats

Questionnaires completed during the field assessment are used to characterize locally observed anthropogenic threats.

Questionnaire variables describing threat extent, severity, and temporal status are standardized to a common 0–1 scale. These values are summarized by threat category and evaluator and subsequently integrated at the site level.

See:

`05_Scripts/06_Threats_questionnaire_Analysis_ECO3.R`

---

### 4. Final integration

The final analysis integrates:

- the spatial Gradient of Anthropogenic Threat (gAT),
- locally observed anthropogenic threats,
- biodiversity metrics, and
- native-species composition.

The integrated analyses evaluate correspondence among spatial, field-based, and biodiversity indicators and compare alternative combinations of candidate sites representing the anthropogenic gradient.

---

## External spatial datasets

Some original spatial datasets used to construct the spatial indicators are not distributed in this repository. Instead, the repository provides the resulting processed spatial layers in `01_spatial/` and the scripts documenting their processing.

The main external spatial datasets used in the workflow include:

- MapBiomas Chile Collection 2 land-cover maps (2000 and 2024).
- Global Human Modification (gHM).
- Chilean national road network (2024).
- Spatial masks defining the ECO3 study area and elevation limits.
- Urban areas used to generate the 1-km urban exclusion buffer.

---

## Software

Analyses were conducted in R 4.5.1.

The workflow primarily uses the following R packages:

- `tidyverse`
- `sf`
- `terra`
- `vegan`
- `iNEXT`
- `patchwork`
- `ggrepel`
- `scales`

---

## Project

This repository was developed as part of: 
**FONDECYT Regular Project No. 1251095** 
