# Gradient of Anthropogenic Threat

This repository contains the data and R scripts used to construct, assess, and integrate a spatial gradient of anthropogenic threat across Mediterranean ecosystems of Central Chile.

The workflow was developed during the initial phase of FONDECYT Regular Project No. 1251095 and combines spatial indicators of anthropogenic pressure, field-based biodiversity data, and anonymous questionnaire-based assessments of locally observed anthropogenic threats.

The repository is organized to document the analytical workflow from the construction of the spatial gradient to the final integration and selection of study sites.

---

## Repository structure

### `01_spatial/`

Contains the processed spatial products used to construct and analyze the Gradient of Anthropogenic Threat (gAT):

- `Road_density.tif` – normalized road-density layer.
- `Loss_Nature.tif` – proportion of natural ecosystems converted to anthropogenic land cover between 2000 and 2024.
- `GHM.tif` – continuous Global Human Modification layer.
- `gAT.tif` – continuous Gradient of Anthropogenic Threat obtained by integrating road density, loss of natural ecosystems, and Global Human Modification.
- `gAT_quintiles.tif` – classification of the continuous gAT into five empirical quintiles.
- `urbano_buffer1km.gpkg` – urban-area exclusion layer, including a 1-km buffer, used to define the final spatial domain of the gAT.

All raster products are aligned to a common 1-km grid and restricted to the study area.

Scripts 01–03 document the processing of the original external spatial datasets used to derive the three spatial indicators. Because these source datasets are not distributed in this repository, these scripts are provided as methodological support. The processed spatial layers required from script 04 onward are included in `01_spatial/`.

---

### `02_field/`

Contains the field data used for the biodiversity analyses and information on the plots visited during the field assessment:

- `StructurePlots_Clean.csv` – cleaned vegetation structure dataset used for biodiversity analyses.
- `0_sitio.xlsx - Sheet1.csv` – record of plots visited during the field assessment.
- `parcels_fieldcheck.gpkg` – spatial location of field-assessment plots.

The biodiversity workflow considers woody individuals with DBH ≥ 3 cm and calculates diversity metrics at the site level.

---

### `03_questionnaire/`

Contains the anonymized questionnaire dataset used to characterize locally observed anthropogenic threats:

- `questionnaire_anonymized_raw.csv` – anonymized responses from the field threat assessment.

Questionnaire responses are standardized to a common 0–1 scale and subsequently aggregated by threat category, evaluator, and study site.

---

### `04_integrated_data/`

Contains the site-level dataset used as input for the final integrative analysis:

- `master_raw_FINAL.csv`

This file consolidates the spatial, biodiversity, and questionnaire information required by the final integration analysis and is provided directly as the input to script 07.

Output directories generated during scripts 05–07 are created automatically when the corresponding analyses are run and are therefore not distributed as part of the repository.

---

### `05_Scripts/`

Contains the R scripts used throughout the analytical workflow:

1. `01_RoadDensity.R`
2. `02_LossNature.R`
3. `03_GHM.R`
4. `04_gAT.R`
5. `05_Diversity_Analysis.R`
6. `06_Threats_questionnaire_Analysis.R`
7. `07_Final_integration_Analysis.R`

Scripts 04–07 should be run from the repository root so that relative file paths resolve correctly.

---

## Analysis workflow

The analytical workflow consists of four main stages:

**Spatial indicators → Anthropogenic threat gradient → Field assessment → Final integration**

### 1. Spatial indicators

Three complementary spatial indicators are used to characterize anthropogenic pressure.

#### Road density

Road density is calculated from the Chilean National Road Network provided by the Dirección de Vialidad, Ministerio de Obras Públicas (MOP, 2024), using the dataset version dated 7 February 2024.

Road density is calculated using a 1-km grid and a 5 × 5 km moving window. Values are normalized to a 0–1 scale using the 2nd and 98th percentiles.

Processing is documented in:

`05_Scripts/01_RoadDensity.R`

#### Loss of natural ecosystems

Loss of natural ecosystems is derived from Collection 2 of the Annual Series of Land Cover and Land Use Maps of Chile produced by the MapBiomas Chile Project (2025), accessed through Google Earth Engine.

Pixels classified as natural in 2000 that transitioned to anthropogenic land-cover classes by 2024 are identified. The resulting proportion of converted natural land is aggregated to approximately 1-km resolution and aligned with the road-density grid.

Processing is documented in:

`05_Scripts/02_LossNature.R`

#### Global Human Modification

The Global Human Modification (gHM) dataset developed by Kennedy et al. (2019) is processed and aligned to the common 1-km spatial grid while retaining a continuous 0–1 representation of human modification.

Processing is documented in:

`05_Scripts/03_GHM.R`

---

### 2. Gradient of Anthropogenic Threat (gAT)

The Gradient of Anthropogenic Threat (gAT) is calculated as the arithmetic mean of the three normalized spatial indicators:

`gAT = (Road density + Loss of natural ecosystems + Global Human Modification) / 3`

Urban areas and a 1-km surrounding buffer are excluded from the study domain.

The resulting continuous gradient is subsequently divided into five empirical quintiles representing progressively higher levels of anthropogenic pressure.

See:

`05_Scripts/04_gAT.R`

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

`05_Scripts/05_Diversity_Analysis.R`

#### Locally observed anthropogenic threats

Questionnaires completed during the field assessment are used to characterize locally observed anthropogenic threats.

Questionnaire variables describing threat extent, severity, and temporal status are standardized to a common 0–1 scale. These values are summarized by threat category and evaluator and subsequently integrated at the site level.

See:

`05_Scripts/06_Threats_questionnaire_Analysis.R`

---

### 4. Final integration

The final analysis integrates:

- the spatial Gradient of Anthropogenic Threat (gAT),
- locally observed anthropogenic threats,
- biodiversity metrics, and
- native-species composition.

The integrated analyses evaluate correspondence among spatial, field-based, and biodiversity indicators and compare alternative combinations of candidate sites representing the anthropogenic gradient.

See:

`05_Scripts/07_Final_integration_Analysis.R`

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

## References

Kennedy, C. M., Oakleaf, J. R., Theobald, D. M., Baruch-Mordo, S., & Kiesecker, J. (2019). Managing the middle: A shift in conservation priorities based on the global human modification gradient. *Global Change Biology, 25*(3), 811–826. 

MapBiomas Chile Project. (2025). *Collection 2 of the Annual Series of Land Cover and Land Use Maps of Chile (1999–2024).* Accessed through Google Earth Engine.

Dirección de Vialidad, Ministerio de Obras Públicas (MOP). (2024). *Red Vial Nacional de Chile*. Dataset version dated 7 February 2024.

---

