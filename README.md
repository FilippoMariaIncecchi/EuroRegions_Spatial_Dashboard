# EuroRegions — NUTS-2 Spatial Dashboard

![R](https://img.shields.io/badge/R-4.x-276DC3?logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-dashboard-blue)
![License: MIT](https://img.shields.io/badge/code-MIT-green)

An interactive **R Shiny** dashboard for exploratory spatial data analysis of
**234 EU-27 regions at NUTS-2 level** — the EU's standard scale for regional
policy and cohesion funding. Built with real **Eurostat** indicators and
official **GISCO** boundaries (ESRI shapefile), shipped locally with the app:
it runs fully offline.

## Live demo

https://filippomariaincecchi.shinyapps.io/EuroRegions_Spatial_Dashboard/

## Features

- **Choropleth explorer** — 6 socio-economic indicators × multiple years,
  with region-level tooltips and rankings
- **Switchable spatial weights** — queen contiguity (with nearest-neighbour
  patch for island regions), k-nearest neighbours (k = 2–8), distance band —
  see how every statistic reacts to the choice of W
- **Spatial autocorrelation** — global Moran's I, Moran scatterplot,
  **LISA cluster maps** and **Getis-Ord G\*** hot/cold spots
- **Convergence & dynamics** — Moran's I tracked over time and σ-convergence
  on a balanced panel
- **Spatial regression** — OLS vs spatial lag (SAR) vs spatial error (SEM)
  via `spatialreg`, with AIC comparison and residual diagnostics

## Quick start

```r
# in R / RStudio, with this folder as working directory:
shiny::runApp()
```

Missing packages are installed automatically on first launch
(`shiny`, `sf`, `spdep`, `spatialreg`, `plotly`, `dplyr`, …).
Keep the `data/` folder next to the `.R` files.

## Project structure

| File | Role |
|---|---|
| `global.R` | loads local data, builds the EPSG:3035 projection and the queen-contiguity graph, defines the `build_weights()` helper used by every tab |
| `ui.R` / `server.R` | the Shiny app (7 tabs) |
| `data/NUTS_RG_20M_2024_4326_LEVL_2.*` | NUTS-2 polygons — ESRI shapefile (`.shp` `.shx` `.dbf` `.prj`), GISCO NUTS 2024, 1:20m, EPSG:4326 |
| `data/indicators_nuts2.csv` | tidy panel: region × year × 6 indicators |
| `data/regions_meta.csv` | region names and countries |

## Data

All indicators come from **Eurostat regional statistics**:

| Indicator | Dataset |
|---|---|
| GDP per capita (PPS / EUR) | `nama_10r_2gdp` |
| Disposable household income p.c. | `nama_10r_2hhinc` |
| Unemployment rate (15–74) | `lfst_r_lfu3rt` |
| Life expectancy at birth | `demo_r_mlifexp` |
| Tertiary education, 25–64 | `edat_lfse_04` |

**Scope:** EU-27 at NUTS-2 level. Non-EU countries are excluded, as are the
outermost territories (French outre-mer, Canarias, Ceuta/Melilla, Açores,
Madeira) — standard practice in EU spatial econometrics, since their
centroids would create neighbourhood links of thousands of km.

Boundaries from the
[GISCO distribution service](https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/)
(`ref-nuts-2024-20m.shp.zip`).
**© EuroGeographics for the administrative boundaries.** Eurostat data are
reusable under the [Eurostat open-data policy](https://ec.europa.eu/eurostat/about-us/policies/copyright).

## About

Developed by **Filippo Maria Incecchi** for the Spatial Data Laboratory,
M.Sc. in Analytics and Data Science for Economics and Management,
Università degli Studi di Brescia (A.Y. 2025/2026).
Stack: R · Shiny · sf · spdep · spatialreg · plotly · dplyr.
