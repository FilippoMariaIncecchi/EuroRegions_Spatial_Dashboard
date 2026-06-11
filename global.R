# =============================================================================
# GLOBAL.R — EuroRegions · NUTS-2 Spatial Dashboard
# Spatial Data Laboratory 2025/2026 · Università degli Studi di Brescia
# Prof. Nicola Pontarollo · Student Filippo Maria Incecchi
# =============================================================================
#   · ~230 NUTS-2 regions, real Eurostat data stored locally in ./data
#   · switchable spatial weights (queen / KNN / distance band)
#   · LISA + Getis-Ord G*, Moran's I over time, σ-convergence
#   · spatial regression: OLS vs SAR (lag) vs SEM (error)
# =============================================================================

# ── Packages (auto-install anything missing) ──────────────────────────────────
required_pkgs <- c("shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets",
                   "shinycssloaders", "dplyr", "tidyr", "ggplot2", "plotly", "sf",
                   "scales", "DT", "fresh", "spdep", "spatialreg")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
library(shiny);            library(shinydashboard);  library(shinydashboardPlus)
library(shinyWidgets);     library(shinycssloaders); library(dplyr)
library(tidyr);            library(ggplot2);         library(plotly)
library(sf);               library(scales);          library(DT)
library(fresh);            library(spdep);           library(spatialreg)

# ── Local data — shipped with the project in ./data ──────────────────────────
# NUTS-2 boundaries: ESRI shapefile from Eurostat GISCO (NUTS 2024, 1:20m,
# EPSG:4326). A shapefile is a set of companion files that must stay together:
#   .shp = geometries · .shx = spatial index · .dbf = attribute table · .prj = CRS
# st_read() is pointed at the .shp and silently reads the other three.
SHP_CANDIDATES <- c("data/NUTS_RG_20M_2024_4326_LEVL_2.shp",  # level-2-only file
                    "data/NUTS_RG_20M_2024_4326.shp")          # all-levels file
SHP_PATH <- SHP_CANDIDATES[file.exists(SHP_CANDIDATES)][1]
if (is.na(SHP_PATH)) {
  stop("NUTS-2 shapefile not found in ./data — download ",
       "'ref-nuts-2024-20m.shp.zip' from Eurostat GISCO, extract the inner ",
       "'NUTS_RG_20M_2024_4326_LEVL_2.shp.zip' and copy its files ",
       "(.shp, .shx, .dbf, .prj) into data/. Also make sure the app is run ",
       "from the project folder (in RStudio: Session > Set Working Directory ",
       "> To Source File Location).")
}
SHP_PARTS <- paste0(sub("\\.shp$", "", SHP_PATH), c(".shp", ".shx", ".dbf", ".prj"))
DATA_FILES <- c(SHP_PARTS, "data/indicators_nuts2.csv", "data/regions_meta.csv")
if (!all(file.exists(DATA_FILES))) {
  stop("Missing files in ./data: ",
       paste(basename(DATA_FILES[!file.exists(DATA_FILES)]), collapse = ", "),
       " — a shapefile needs all four companion files (.shp .shx .dbf .prj) ",
       "copied together, and the two CSVs must ship with the app.")
}

# EU-27 only: non-EU countries (Switzerland, Norway) are dropped everywhere —
# Eurostat does not publish these regional indicators for them anyway.
NON_EU <- c("CH", "NO")

meta  <- read.csv("data/regions_meta.csv", stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8") %>%
  filter(!country %in% NON_EU)
panel <- read.csv("data/indicators_nuts2.csv", stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8") %>%
  filter(geo %in% meta$geo)

# GISCO ships every NUTS level for EU + EFTA + candidate countries; keep only
# the NUTS-2 polygons in the project scope (the EU-27 regions in
# regions_meta.csv, outermost territories excluded), then rename the GISCO id
# column (NUTS_ID) to the Eurostat code used everywhere else in the app (geo).
# Names/countries come from regions_meta.csv, as before.
mapN2 <- st_read(SHP_PATH, quiet = TRUE) %>%
  st_make_valid() %>%
  filter(LEVL_CODE == 2, NUTS_ID %in% meta$geo) %>%
  transmute(geo = NUTS_ID) %>%
  left_join(meta, by = "geo") %>%
  arrange(geo)

N_REGIONS <- nrow(mapN2)

# ── Indicators ────────────────────────────────────────────────────────────────
INDICATORS <- c(
  "GDP per capita (PPS)"                    = "gdp_pps_hab",
  "GDP per capita (EUR)"                    = "gdp_eur_hab",
  "Disposable income per capita (PPS)"      = "income_pps_hab",
  "Unemployment rate (%)"                   = "unemp_rate",
  "Life expectancy at birth (years)"        = "life_exp",
  "Tertiary education, age 25-64 (%)"       = "tertiary_pct"
)
IND_LABEL <- setNames(names(INDICATORS), unname(INDICATORS))  # code -> label

PREDICTORS <- c(
  "Unemployment rate (%)"             = "unemp_rate",
  "Life expectancy at birth (years)"  = "life_exp",
  "Tertiary education, 25-64 (%)"     = "tertiary_pct"
)

fmt_val <- function(v, var) {
  if (var %in% c("gdp_pps_hab", "gdp_eur_hab", "income_pps_hab"))
    format(round(v), big.mark = ",")
  else sprintf("%.1f", v)
}

# Years: anchored on GDP (PPS) availability — the core variable of the app
YEARS    <- sort(unique(panel$year[!is.na(panel$gdp_pps_hab)]))
YEAR_MIN <- min(YEARS)
YEAR_MAX <- max(YEARS)

# ── Projection, centroids, queen contiguity (computed once) ───────────────────
spdep::set.ZeroPolicyOption(TRUE)

mapN2_proj   <- st_transform(mapN2, 3035)                 # ETRS89-LAEA, metres
CENTROIDS_XY <- st_coordinates(st_centroid(st_geometry(mapN2_proj)))

nb_queen_full <- poly2nb(mapN2_proj, queen = TRUE, snap = 100)

# ── Weights builder — shared by all analysis tabs ─────────────────────────────
# keep   : logical vector (length N_REGIONS) of regions entering the analysis
# type   : "queen" | "knn" | "dist"
# Queen contiguity is patched with a nearest-neighbour link for island regions
# (Cyprus, Malta, islands) so no region is silently dropped from the analysis.
build_weights <- function(keep, type = "queen", k = 4, dist_km = 500) {
  coords <- CENTROIDS_XY[keep, , drop = FALSE]
  n      <- sum(keep)
  if (type == "queen") {
    nb       <- subset(nb_queen_full, keep)
    isolated <- which(card(nb) == 0)
    if (length(isolated) > 0 && n > 2) {
      knn1 <- knn2nb(knearneigh(coords, k = 1))
      for (i in isolated) {
        j       <- as.integer(knn1[[i]][1])
        nb[[i]] <- j
        nb[[j]] <- sort(unique(c(nb[[j]][nb[[j]] != 0L], as.integer(i))))
      }
    }
    desc <- paste0("Queen contiguity",
                   if (length(isolated) > 0)
                     paste0(" + nearest link for ", length(isolated),
                            " island region(s)") else "")
  } else if (type == "knn") {
    nb   <- knn2nb(knearneigh(coords, k = k))
    desc <- paste0("k-nearest neighbours, k = ", k)
  } else {
    nb   <- dnearneigh(coords, 0, dist_km * 1000)
    desc <- paste0("Distance band 0-", dist_km, " km between centroids")
  }
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  list(nb = nb, lw = lw, n = n,
       n_isolated = sum(card(nb) == 0),
       avg_links  = round(mean(card(nb)), 1),
       desc       = desc)
}

# ── Colour palette ────────────────────────────────────────────────────────────
ACCENT  <- "#0D9488"
ACCENT2 <- "#F59E0B"
LISA_COLORS <- c(
  HH = "#B91C1C", LL = "#1D4ED8", HL = "#F59E0B", LH = "#A78BFA",
  NS = "#E2E8F0", `No data` = "#F8FAFC"
)
GSTAR_COLORS <- c(
  "Hot spot (99%)"  = "#B91C1C", "Hot spot (95%)"  = "#EF4444",
  "Not significant" = "#E2E8F0",
  "Cold spot (95%)" = "#60A5FA", "Cold spot (99%)" = "#1D4ED8",
  `No data` = "#F8FAFC"
)

euro_theme <- create_theme(
  adminlte_color(
    light_blue = ACCENT,
    green      = "#10B981",
    yellow     = "#F59E0B",
    red        = "#EF4444"
  ),
  adminlte_sidebar(
    width            = "250px",
    dark_bg          = "#0F172A",
    dark_hover_bg    = "#1E293B",
    dark_color       = "#94A3B8",
    dark_hover_color = "#F1F5F9"
  ),
  adminlte_global(
    content_bg  = "#F8FAFC",
    box_bg      = "#FFFFFF",
    info_box_bg = "#FFFFFF"
  )
)

# ── Sentinel — ui.R / server.R verify global.R has fully run ──────────────────
GLOBALS_OK <- TRUE
