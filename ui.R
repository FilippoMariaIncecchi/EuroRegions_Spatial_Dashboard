# =============================================================================
# UI.R — EuroRegions · NUTS-2 Spatial Dashboard
# =============================================================================

# Make sure global.R has fully run (guards against a stale restored workspace)
if (!isTRUE(get0("GLOBALS_OK", ifnotfound = FALSE))) source("global.R")

# ── Custom CSS ────────────────────────────────────────────────────────────────
custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=DM+Mono:wght@400;500&family=Fraunces:ital,wght@0,300;0,600;1,300&display=swap');

  :root {
    --accent:   #0D9488;
    --accent2:  #F59E0B;
    --dark:     #0F172A;
    --card:     #FFFFFF;
    --muted:    #64748B;
    --border:   #E2E8F0;
    --radius:   12px;
    --shadow:   0 1px 3px rgba(0,0,0,.06), 0 4px 16px rgba(0,0,0,.08);
  }

  body, .wrapper { background: #F1F5F9 !important; }
  * { font-family: 'DM Sans', sans-serif; }

  .main-sidebar, .left-side {
    background: var(--dark) !important;
    box-shadow: 2px 0 20px rgba(0,0,0,.25) !important;
  }
  .sidebar-menu > li > a {
    font-size: 13.5px; font-weight: 500; letter-spacing: .02em;
    padding: 11px 20px;
    border-left: 3px solid transparent !important;
    transition: all .2s ease;
  }
  .sidebar-menu > li.active > a,
  .sidebar-menu > li > a:hover {
    background: rgba(13,148,136,.18) !important;
    border-left: 3px solid var(--accent) !important;
    color: #F1F5F9 !important;
  }
  .sidebar-menu > li > a > .fa { width: 20px; margin-right: 10px; color: #94A3B8; }
  .sidebar-menu > li.active > a > .fa { color: var(--accent) !important; }

  .main-header .logo {
    background: #08101F !important;
    font-family: 'Fraunces', serif !important;
    font-weight: 600; font-size: 15px !important;
    color: #F8FAFC !important;
    border-bottom: 1px solid rgba(255,255,255,.06) !important;
  }
  .main-header .navbar { background: #0F172A !important; border-bottom: none !important; }
  .main-header .sidebar-toggle { color: #94A3B8 !important; }
  .main-header .sidebar-toggle:hover { color: #F1F5F9 !important; background: rgba(255,255,255,.08) !important; }

  .box {
    border-radius: var(--radius) !important;
    border: 1px solid var(--border) !important;
    box-shadow: var(--shadow) !important;
    background: var(--card) !important;
    border-top: none !important;
  }
  .box-header { padding: 14px 18px 10px !important; border-bottom: 1px solid var(--border) !important; }
  .box-header .box-title {
    font-size: 13px !important; font-weight: 600 !important;
    text-transform: uppercase; letter-spacing: .07em; color: var(--muted) !important;
  }
  .box-body { padding: 16px 18px !important; }

  .small-box { border-radius: var(--radius) !important; box-shadow: var(--shadow) !important; overflow: hidden; }
  .small-box h3 { font-family: 'Fraunces', serif !important; font-size: 26px !important; font-weight: 600 !important; margin: 0 !important; }
  .small-box p { font-size: 12px !important; font-weight: 500 !important; opacity: .85; }
  .small-box .icon { font-size: 60px !important; top: 10px !important; }
  .small-box:hover { transform: translateY(-2px); transition: transform .2s ease; }

  .selectize-input {
    border-radius: 8px !important; border: 1.5px solid var(--border) !important;
    font-size: 13.5px !important; box-shadow: none !important;
  }
  .selectize-input.focus { border-color: var(--accent) !important; }
  .selectize-dropdown { border-radius: 8px !important; border: 1.5px solid var(--border) !important; box-shadow: var(--shadow) !important; }
  .selectize-dropdown-content .option:hover { background: rgba(13,148,136,.08) !important; color: var(--accent) !important; }
  .selectize-dropdown-content .active { background: var(--accent) !important; }

  label { font-size: 12px !important; font-weight: 600 !important; color: var(--muted) !important; text-transform: uppercase; letter-spacing: .05em; margin-bottom: 6px !important; }

  .irs--shiny .irs-bar { background: var(--accent) !important; }
  .irs--shiny .irs-handle { border-color: var(--accent) !important; background: var(--accent) !important; }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: var(--accent) !important; }

  .content-wrapper { background: #F1F5F9 !important; }
  .content { padding: 20px 24px !important; }

  .section-title { font-family: 'Fraunces', serif; font-size: 22px; font-weight: 600; color: #0F172A; margin-bottom: 6px; }
  .section-sub   { font-size: 13px; color: var(--muted); margin-bottom: 20px; }

  .dataTables_wrapper { font-size: 13px; }
  table.dataTable thead th {
    font-weight: 600; font-size: 11px; text-transform: uppercase;
    letter-spacing: .06em; color: var(--muted); border-bottom: 2px solid var(--border) !important;
  }
  table.dataTable tbody tr:hover td { background: rgba(13,148,136,.04) !important; }

  ::-webkit-scrollbar { width: 6px; height: 6px; }
  ::-webkit-scrollbar-track { background: transparent; }
  ::-webkit-scrollbar-thumb { background: #CBD5E1; border-radius: 99px; }

  .sidebar-custom-footer {
    position: absolute; bottom: 0; width: 100%;
    padding: 12px 20px; font-size: 11px; color: #475569;
    border-top: 1px solid rgba(255,255,255,.06);
  }
"

# ── Helpers ───────────────────────────────────────────────────────────────────
section_header <- function(title, subtitle = "") {
  tagList(
    tags$p(class = "section-title", title),
    if (nchar(subtitle) > 0) tags$p(class = "section-sub", subtitle)
  )
}

# =============================================================================
# UI definition
# =============================================================================

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-map", style = "margin-right:8px; color:#0D9488;"),
      "EuroRegions"
    ),
    titleWidth = 250
  ),

  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Home",                  tabName = "home",  icon = icon("graduation-cap")),
      menuItem("Map Explorer",          tabName = "map",   icon = icon("earth-europe")),
      menuItem("Spatial Analysis",      tabName = "esda",  icon = icon("map-location-dot")),
      menuItem("Convergence & Dynamics",tabName = "conv",  icon = icon("chart-line")),
      menuItem("Spatial Regression",    tabName = "reg",   icon = icon("calculator")),
      menuItem("Data Table",            tabName = "data",  icon = icon("table")),
      menuItem("About",                 tabName = "about", icon = icon("circle-info"))
    ),
    tags$div(
      class = "sidebar-custom-footer",
      tags$span("Data: Eurostat · GISCO (local files)"),
      tags$br(),
      tags$span(paste0(N_REGIONS, " NUTS-2 regions · ", YEAR_MIN, "-", YEAR_MAX))
    )
  ),

  dashboardBody(
    use_theme(euro_theme),
    tags$head(
      tags$style(HTML(custom_css)),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
    ),

    tabItems(

      # ════════════════════════════════════════════════════════════════════════
      # 0. HOME
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "home",

        fluidRow(
          box(
            width = 12, solidHeader = FALSE,
            tags$div(style = "text-align:center; padding:30px 24px 24px;",
              tags$i(class = "fa fa-map", style = "font-size:44px; color:#0D9488;"),
              tags$h1(style = "font-family:'Fraunces',serif; font-weight:600; font-size:30px;
                               color:#0F172A; margin:14px 0 6px;",
                "EuroRegions Spatial Dashboard"),
              tags$p(style = "font-size:15px; color:#475569; max-width:760px; margin:0 auto 22px;",
                paste0("Exploratory spatial data analysis and spatial econometrics on ",
                       N_REGIONS, " European NUTS-2 regions, with real Eurostat data
                       ")),
              tags$div(style = "display:inline-block; background:#F0FDFA; border:1px solid #99F6E4;
                                border-radius:12px; padding:18px 32px;",
                tags$p(style = "font-size:13px; font-weight:700; color:#115E59; margin:0 0 8px;
                                text-transform:uppercase; letter-spacing:.06em;",
                  icon("graduation-cap"), " Spatial Data Laboratory · A.Y. 2025/2026"),
                tags$p(style = "font-size:13.5px; color:#334155; margin:0 0 3px;",
                  "M.Sc. in Analytics and Data Science for Economics and Management"),
                tags$p(style = "font-size:13.5px; color:#334155; margin:0 0 3px;",
                  tags$strong("Università degli Studi di Brescia")),
                tags$p(style = "font-size:13.5px; color:#334155; margin:0;",
                  "Course instructor: Prof. Nicola Pontarollo · Author: Filippo Maria Incecchi")
              )
            )
          )
        ),

        fluidRow(
          box(
            title = "Why the Regional Scale", width = 6, solidHeader = FALSE,
            tags$p(style = "font-size:13.5px; line-height:1.8; color:#334155;",
              "The unit of analysis is the ", tags$strong("NUTS-2 region"),
              " — the EU's standard level for regional policy and cohesion funding (",
              N_REGIONS, " regions in this dataset)."),
            tags$p(style = "font-size:13.5px; line-height:1.8; color:#334155;",
              "Working at this scale matters: country-level analyses (n ≈ 30) give
               Moran's I little statistical power, and each observation hides huge
               internal differences — Milano and Calabria disappear inside a single
               'Italy' value. With ~250 regions, spatial clusters, regional
               convergence, and core-periphery patterns become measurable."),
            tags$p(style = "font-size:12.5px; line-height:1.7; color:#64748B;",
              icon("database"),
              " All data are stored locally in the ", tags$code("data/"), " folder:
                Eurostat regional indicators (CSV) and GISCO NUTS-2 boundaries
                (ESRI shapefile). The app runs fully offline.")
          ),
          box(
            title = "What This Dashboard Offers", width = 6, solidHeader = FALSE,
            tags$ul(style = "font-size:13px; color:#475569; line-height:2; padding-left:18px; margin:0;",
              tags$li(tags$strong("NUTS-2 scale"), " — ", N_REGIONS, " EU-27 regions,
                      the standard level for EU regional policy and cohesion funding"),
              tags$li(tags$strong("Switchable spatial weights"), " — queen contiguity,
                      k-nearest neighbours (k = 2-8), distance band — see how Moran's I
                      reacts to the W matrix"),
              tags$li(tags$strong("Getis-Ord G*"), " hot-spot analysis next to LISA"),
              tags$li(tags$strong("Convergence & Dynamics"), " — Moran's I and
                      σ-convergence (coefficient of variation) tracked over time"),
              tags$li(tags$strong("Spatial regression"), " — OLS vs. spatial lag (SAR)
                      vs. spatial error (SEM) models, via spatialreg"),
              tags$li(tags$strong("Local data files"), " — boundaries (shapefile) and
                      indicators (CSV) ship with the project")
            )
          )
        ),

        fluidRow(
          box(
            title = "How to Navigate", width = 12, solidHeader = FALSE,
            fluidRow(
              column(6,
                tags$ul(style = "font-size:13px; color:#475569; line-height:2; padding-left:18px; margin:0;",
                  tags$li(tags$strong("Map Explorer"), " — choropleth of any indicator and
                          year, regional rankings, single-region trends"),
                  tags$li(tags$strong("Spatial Analysis"), " — global Moran's I, Moran
                          scatter plot, LISA clusters and G* hot spots, with your choice
                          of spatial weights"),
                  tags$li(tags$strong("Convergence & Dynamics"), " — is Europe's regional
                          inequality shrinking, and is clustering getting stronger?")
                )
              ),
              column(6,
                tags$ul(style = "font-size:13px; color:#475569; line-height:2; padding-left:18px; margin:0;",
                  tags$li(tags$strong("Spatial Regression"), " — does GDP per capita
                          depend on its neighbours? OLS / SAR / SEM side by side"),
                  tags$li(tags$strong("Data Table"), " — full panel, exportable to CSV/Excel"),
                  tags$li(tags$strong("About"), " — data sources, dataset codes,
                          and methodology")
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # 1. MAP EXPLORER
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "map",
        section_header("Regional Map Explorer",
                       paste0("Eurostat indicators across ", N_REGIONS,
                              " NUTS-2 regions · ", YEAR_MIN, "-", YEAR_MAX)),

        fluidRow(
          valueBoxOutput("kpi_mean",   width = 3),
          valueBoxOutput("kpi_top",    width = 3),
          valueBoxOutput("kpi_bottom", width = 3),
          valueBoxOutput("kpi_cv",     width = 3)
        ),

        fluidRow(
          box(
            title = "Choropleth Map", width = 8, solidHeader = FALSE,
            withSpinner(plotlyOutput("map_main", height = "480px"), color = "#0D9488")
          ),
          box(
            title = "Controls", width = 4, solidHeader = FALSE,
            pickerInput("map_var", "Indicator",
                        choices = INDICATORS, selected = "gdp_pps_hab"),
            sliderInput("map_year", "Year",
                        min = YEAR_MIN, max = YEAR_MAX, value = YEAR_MAX,
                        step = 1, sep = "", ticks = FALSE),
            hr(style = "border-color:#E2E8F0;"),
            pickerInput("map_region", "Region trend (chart below)",
                        choices  = setNames(mapN2$geo,
                                            paste0(mapN2$region_name, " (", mapN2$geo, ")")),
                        selected = "ITC4",
                        options  = list(`live-search` = TRUE)),
            tags$p(style = "font-size:12px; color:#64748B;",
              icon("circle-info"),
              " Grey regions have no data for the selected indicator-year.")
          )
        ),

        fluidRow(
          box(
            title = "Top & Bottom 15 Regions", width = 6, solidHeader = FALSE,
            withSpinner(plotlyOutput("bar_topbottom", height = "420px"), color = "#0D9488")
          ),
          box(
            title = "Region vs. European Mean Over Time", width = 6, solidHeader = FALSE,
            withSpinner(plotlyOutput("line_region_trend", height = "420px"), color = "#0D9488")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # 2. SPATIAL ANALYSIS
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "esda",
        section_header("Spatial Autocorrelation — ESDA",
                       "Global Moran's I, LISA clusters and Getis-Ord G* hot spots,
                        with switchable spatial weight matrices"),

        fluidRow(
          box(title = "Variable & Year", width = 4, solidHeader = FALSE,
            pickerInput("sa_var", NULL, choices = INDICATORS, selected = "gdp_pps_hab"),
            sliderInput("sa_year", "Year",
                        min = YEAR_MIN, max = YEAR_MAX, value = YEAR_MAX,
                        step = 1, sep = "", ticks = FALSE)
          ),
          box(title = "Spatial Weights (W)", width = 8, solidHeader = FALSE,
            radioGroupButtons(
              inputId = "w_type", label = NULL,
              choices = c("Queen contiguity" = "queen",
                          "K-nearest neighbours" = "knn",
                          "Distance band" = "dist"),
              selected = "queen", status = "primary", size = "sm", justified = TRUE
            ),
            conditionalPanel(
              condition = "input.w_type == 'knn'",
              sliderInput("w_k", "k (neighbours per region)",
                          min = 2, max = 8, value = 4, step = 1, ticks = FALSE)
            ),
            conditionalPanel(
              condition = "input.w_type == 'dist'",
              sliderInput("w_dist", "Distance threshold (km)",
                          min = 200, max = 1000, value = 500, step = 50, ticks = FALSE)
            ),
            htmlOutput("w_note")
          )
        ),

        fluidRow(
          box(title = "Global Moran's I", width = 12, solidHeader = FALSE,
            withSpinner(uiOutput("moran_kpi_ui"), color = "#0D9488")
          )
        ),

        fluidRow(
          box(title = "Moran Scatter Plot", width = 5, solidHeader = FALSE,
            withSpinner(plotlyOutput("moran_scatter", height = "420px"), color = "#0D9488"),
            tags$p(style = "font-size:11px; color:#94A3B8; margin-top:6px;",
              "Each point is a region; the dashed slope equals Moran's I.
               Colours match the LISA quadrants.")
          ),
          box(title = "Local Clusters", width = 7, solidHeader = FALSE,
            radioGroupButtons(
              inputId = "cluster_type", label = NULL,
              choices = c("LISA clusters" = "lisa", "Getis-Ord G* hot spots" = "gstar"),
              selected = "lisa", status = "primary", size = "sm"
            ),
            withSpinner(plotlyOutput("cluster_map", height = "400px"), color = "#0D9488")
          )
        ),

        fluidRow(
          box(title = "Interpretation", width = 12, solidHeader = FALSE,
            withSpinner(uiOutput("sa_interp_ui"), color = "#0D9488")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # 3. CONVERGENCE & DYNAMICS
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "conv",
        section_header("Convergence & Dynamics",
                       "Is regional Europe converging? Dispersion (σ-convergence) and
                        spatial clustering (Moran's I) tracked over time"),

        fluidRow(
          box(title = "Settings", width = 4, solidHeader = FALSE,
            pickerInput("conv_var", "Indicator",
                        choices = INDICATORS, selected = "gdp_pps_hab"),
            radioGroupButtons(
              inputId = "conv_wtype", label = "Spatial weights",
              choices = c("Queen" = "queen", "KNN (k=4)" = "knn"),
              selected = "queen", status = "primary", size = "sm", justified = TRUE
            )
          ),
          box(title = "Balanced Panel", width = 8, solidHeader = FALSE,
            withSpinner(uiOutput("conv_note_ui"), color = "#0D9488")
          )
        ),

        fluidRow(
          box(title = "Global Moran's I Over Time", width = 6, solidHeader = FALSE,
            withSpinner(plotlyOutput("conv_moran_line", height = "340px"), color = "#0D9488")
          ),
          box(title = "Sigma-Convergence: Coefficient of Variation", width = 6, solidHeader = FALSE,
            withSpinner(plotlyOutput("conv_cv_line", height = "340px"), color = "#0D9488")
          )
        ),

        fluidRow(
          box(title = "Reading the Two Curves Together", width = 12, solidHeader = FALSE,
            withSpinner(uiOutput("conv_interp_ui"), color = "#0D9488")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # 4. SPATIAL REGRESSION
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "reg",
        section_header("Spatial Regression — OLS vs. SAR vs. SEM",
                       "Does regional GDP per capita depend on neighbouring regions?
                        Spatial lag and spatial error models via spatialreg"),

        fluidRow(
          box(title = "Model Setup", width = 4, solidHeader = FALSE,
            sliderInput("reg_year", "Year",
                        min = YEAR_MIN, max = YEAR_MAX, value = YEAR_MAX - 1,
                        step = 1, sep = "", ticks = FALSE),
            checkboxGroupInput("reg_predictors", "Predictors",
                               choices = PREDICTORS,
                               selected = unname(PREDICTORS)),
            radioGroupButtons(
              inputId = "reg_wtype", label = "Spatial weights",
              choices = c("Queen" = "queen", "KNN (k=4)" = "knn"),
              selected = "queen", status = "primary", size = "sm", justified = TRUE
            )
          ),
          box(title = "Specification", width = 8, solidHeader = FALSE,
            tags$p(style = "font-size:13.5px; line-height:1.8; color:#334155;",
              "Dependent variable: ", tags$strong("log GDP per capita (PPS)"), "."),
            tags$p(style = "font-size:13px; line-height:1.8; color:#475569;",
              tags$strong("OLS"), ": y = Xβ + ε — ignores space entirely.", tags$br(),
              tags$strong("SAR (spatial lag)"), ": y = ρWy + Xβ + ε — a region's GDP
              depends directly on its neighbours' GDP (spillovers).", tags$br(),
              tags$strong("SEM (spatial error)"), ": y = Xβ + u, u = λWu + ε — spatial
              dependence lives in omitted factors captured by the error term."),
            tags$p(style = "font-size:12px; color:#94A3B8;",
              icon("circle-info"),
              " If OLS residuals show significant Moran's I, plain OLS is misspecified
                and a spatial model is warranted. Compare AICs: lower is better.")
          )
        ),

        fluidRow(
          box(title = "Model Comparison", width = 12, solidHeader = FALSE,
            withSpinner(DT::dataTableOutput("reg_table"), color = "#0D9488")
          )
        ),

        fluidRow(
          box(title = "Interpretation", width = 12, solidHeader = FALSE,
            withSpinner(uiOutput("reg_interp_ui"), color = "#0D9488")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # 5. DATA TABLE
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "data",
        section_header("Full Dataset",
                       "All indicators for every NUTS-2 region · export to CSV / Excel"),

        fluidRow(
          box(width = 12, solidHeader = FALSE,
            sliderInput("data_year", "Year",
                        min = YEAR_MIN, max = YEAR_MAX, value = YEAR_MAX,
                        step = 1, sep = "", ticks = FALSE),
            DT::dataTableOutput("full_table")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # 6. ABOUT
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "about",
        section_header("About this Dashboard"),

        fluidRow(
          box(
            title = "Data Sources (all local, in ./data)", width = 6, solidHeader = FALSE,
            tags$p(style = "font-size:13.5px; line-height:1.75; color:#334155;",
              "All indicators come from ", tags$strong("Eurostat regional statistics"),
              ", stored as CSV in the project's ", tags$code("data/"), " folder:"),
            tags$ul(style = "font-size:13px; color:#475569; line-height:2;",
              tags$li(tags$code("nama_10r_2gdp"), " — GDP per capita (PPS_EU27_2020_HAB and EUR_HAB)"),
              tags$li(tags$code("nama_10r_2hhinc"), " — net disposable household income (B6N, balance, PPS/hab)"),
              tags$li(tags$code("lfst_r_lfu3rt"), " — unemployment rate (15-74, total, all ISCED levels)"),
              tags$li(tags$code("demo_r_mlifexp"), " — life expectancy at birth (total)"),
              tags$li(tags$code("edat_lfse_04"), " — tertiary educational attainment, age 25-64")
            ),
            tags$p(style = "font-size:13px; line-height:1.75; color:#475569;",
              "Boundaries: ", tags$strong("Eurostat GISCO"), " NUTS 2024 polygons,
               1:20m resolution, EPSG:4326, saved as an ESRI shapefile ",
              tags$code("data/NUTS_RG_20M_2024_4326_LEVL_2.*"),
              " — four companion files (", tags$code(".shp"), " geometries, ",
              tags$code(".shx"), " index, ", tags$code(".dbf"), " attributes, ",
              tags$code(".prj"), " CRS) read together by ",
              tags$code("sf::st_read()"), "."),
            tags$p(style = "font-size:11.5px; color:#94A3B8;",
              "© EuroGeographics for the administrative boundaries.")
          ),
          box(
            title = "Scope & Methodology", width = 6, solidHeader = FALSE,
            tags$p(style = "font-size:13.5px; line-height:1.75; color:#334155;",
              tags$strong("Scope: "), "EU-27 at NUTS-2 level — non-EU countries
              (e.g. Switzerland, Norway) are excluded, since Eurostat does not
              publish these regional indicators for them.
              Outermost territories (French outre-mer, Canarias,
              Ceuta/Melilla, Açores, Madeira, Svalbard) are excluded, as is
              standard in EU spatial econometrics — their centroids would create
              neighbourhood links of thousands of km."),
            tags$p(style = "font-size:13.5px; line-height:1.75; color:#334155;",
              tags$strong("Weights: "), "computed on region centroids in the ETRS89-LAEA
              projection (EPSG:3035). Queen contiguity is patched with a
              nearest-neighbour link for island regions (e.g. Cyprus, Malta) so no
              region is silently excluded. KNN and distance-band alternatives are
              available in every analysis tab."),
            tags$p(style = "font-size:13.5px; line-height:1.75; color:#334155;",
              tags$strong("Methods: "), "global Moran's I (randomisation inference),
              local Moran (LISA, p < 0.05), Getis-Ord G* (z-score classes),
              σ-convergence (coefficient of variation), and ML-estimated spatial lag /
              spatial error models (spatialreg)."),
            tags$p(style = "font-size:13px; line-height:1.75; color:#475569;",
              tags$strong("Deploy: "), "install the ", tags$code("rsconnect"),
              " package, create a free account on shinyapps.io, then ",
              tags$code("rsconnect::deployApp()"), " from this folder — the data/
              folder is uploaded with the app, so it works online too.")
          )
        )
      )

    ) # end tabItems
  )   # end dashboardBody
)
