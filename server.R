# =============================================================================
# SERVER.R — EuroRegions · NUTS-2 Spatial Dashboard
# =============================================================================

# Make sure global.R has fully run (guards against a stale restored workspace)
if (!isTRUE(get0("GLOBALS_OK", ifnotfound = FALSE))) source("global.R")

server <- function(input, output, session) {

  unit_suffix <- function(var) {
    switch(var,
      gdp_pps_hab    = " PPS",
      gdp_eur_hab    = " €",
      income_pps_hab = " PPS",
      unemp_rate     = "%",
      life_exp       = " yrs",
      tertiary_pct   = "%")
  }

  plotly_base <- function(p) {
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(family = "DM Sans, sans-serif", color = "#334155")
    )
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # 1. MAP EXPLORER
  # ═══════════════════════════════════════════════════════════════════════════

  map_df <- reactive({
    req(input$map_var, input$map_year)
    vals <- panel %>%
      filter(year == input$map_year) %>%
      select(geo, value = all_of(input$map_var))
    mapN2 %>% left_join(vals, by = "geo")
  })

  output$kpi_mean <- renderValueBox({
    d <- map_df()
    valueBox(
      value    = paste0(fmt_val(mean(d$value, na.rm = TRUE), input$map_var),
                        unit_suffix(input$map_var)),
      subtitle = "European mean (unweighted)",
      icon     = icon("calculator"),
      color    = "light-blue"
    )
  })

  output$kpi_top <- renderValueBox({
    d   <- map_df() %>% st_drop_geometry() %>% filter(!is.na(value))
    top <- d[which.max(d$value), ]
    valueBox(
      value    = top$region_name,
      subtitle = paste0("Highest · ", fmt_val(top$value, input$map_var),
                        unit_suffix(input$map_var)),
      icon     = icon("trophy"),
      color    = "green"
    )
  })

  output$kpi_bottom <- renderValueBox({
    d   <- map_df() %>% st_drop_geometry() %>% filter(!is.na(value))
    bot <- d[which.min(d$value), ]
    valueBox(
      value    = bot$region_name,
      subtitle = paste0("Lowest · ", fmt_val(bot$value, input$map_var),
                        unit_suffix(input$map_var)),
      icon     = icon("arrow-trend-down"),
      color    = "red"
    )
  })

  output$kpi_cv <- renderValueBox({
    d  <- map_df()$value
    cv <- sd(d, na.rm = TRUE) / mean(d, na.rm = TRUE) * 100
    valueBox(
      value    = paste0(sprintf("%.1f", cv), "%"),
      subtitle = "Regional dispersion (coeff. of variation)",
      icon     = icon("arrows-left-right"),
      color    = "yellow"
    )
  })

  output$map_main <- renderPlotly({
    d   <- map_df()
    lab <- IND_LABEL[input$map_var]

    d <- d %>%
      mutate(tip = paste0(region_name, " (", geo, ") · ", country_name, "\n",
                          lab, ": ",
                          ifelse(is.na(value), "no data",
                                 paste0(fmt_val(value, input$map_var),
                                        unit_suffix(input$map_var)))))

    p <- ggplot(d) +
      geom_sf(aes(fill = value, text = tip), colour = "white", linewidth = 0.08) +
      scale_fill_viridis_c(name = lab, na.value = "#E2E8F0", labels = scales::comma) +
      coord_sf(xlim = c(-11, 32), ylim = c(34, 71)) +
      theme_void() +
      theme(legend.position = "right",
            legend.title    = element_text(size = 9, colour = "#64748B"),
            legend.text     = element_text(size = 8))

    ggplotly(p, tooltip = "text") %>%
      plotly_base() %>%
      layout(margin = list(t = 0, b = 0, l = 0, r = 0))
  })

  output$bar_topbottom <- renderPlotly({
    d <- map_df() %>% st_drop_geometry() %>%
      filter(!is.na(value)) %>%
      arrange(desc(value))
    req(nrow(d) > 30)
    top    <- head(d, 15) %>% mutate(grp = "Top 15")
    bottom <- tail(d, 15) %>% mutate(grp = "Bottom 15")
    dd     <- bind_rows(top, bottom) %>%
      mutate(lab = paste0(region_name, " (", geo, ")"))

    plot_ly(
      x           = dd$value,
      y           = reorder(dd$lab, dd$value),
      type        = "bar",
      orientation = "h",
      marker      = list(color = ifelse(dd$grp == "Top 15", "#0D9488", "#EF4444")),
      hovertemplate = paste0("<b>%{y}</b><br>",
                             IND_LABEL[input$map_var], ": %{x:,.1f}<extra></extra>")
    ) %>%
      plotly_base() %>%
      layout(
        xaxis  = list(title = IND_LABEL[input$map_var], gridcolor = "#E2E8F0"),
        yaxis  = list(title = "", tickfont = list(size = 9)),
        margin = list(t = 10, b = 50, l = 180, r = 10),
        bargap = 0.3
      )
  })

  output$line_region_trend <- renderPlotly({
    req(input$map_region, input$map_var)
    var <- input$map_var

    reg_series <- panel %>%
      filter(geo == input$map_region, year %in% YEARS) %>%
      select(year, value = all_of(var)) %>%
      arrange(year)

    eu_series <- panel %>%
      filter(year %in% YEARS) %>%
      group_by(year) %>%
      summarise(value = mean(.data[[var]], na.rm = TRUE), .groups = "drop")

    reg_name <- mapN2$region_name[mapN2$geo == input$map_region][1]

    plot_ly() %>%
      add_trace(
        data = eu_series, x = ~year, y = ~value,
        type = "scatter", mode = "lines",
        name = "European mean",
        line = list(color = "#94A3B8", width = 2, dash = "dot"),
        hovertemplate = "European mean<br>%{x}: %{y:,.1f}<extra></extra>"
      ) %>%
      add_trace(
        data = reg_series, x = ~year, y = ~value,
        type = "scatter", mode = "lines+markers",
        name = reg_name,
        line = list(color = "#0D9488", width = 3),
        marker = list(color = "#0D9488", size = 6),
        hovertemplate = paste0(reg_name, "<br>%{x}: %{y:,.1f}<extra></extra>")
      ) %>%
      plotly_base() %>%
      layout(
        xaxis  = list(title = "Year", dtick = 2, gridcolor = "#E2E8F0"),
        yaxis  = list(title = IND_LABEL[var], gridcolor = "#E2E8F0"),
        legend = list(orientation = "h", y = -0.2),
        margin = list(t = 10, b = 60, l = 70, r = 10)
      )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # 2. SPATIAL ANALYSIS — ESDA
  # ═══════════════════════════════════════════════════════════════════════════

  esda_set <- reactive({
    req(input$sa_var, input$sa_year, input$w_type)
    vals <- panel %>%
      filter(year == input$sa_year) %>%
      select(geo, value = all_of(input$sa_var))
    sfd  <- mapN2 %>% left_join(vals, by = "geo")
    keep <- !is.na(sfd$value)
    validate(need(sum(keep) >= 30,
      "Fewer than 30 regions have data for this indicator-year — choose another year or indicator."))
    W <- build_weights(keep,
                       type    = input$w_type,
                       k       = ifelse(is.null(input$w_k), 4, input$w_k),
                       dist_km = ifelse(is.null(input$w_dist), 500, input$w_dist))
    list(sf = sfd, keep = keep, W = W, vec = sfd$value[keep])
  })

  moran_global <- reactive({
    es <- esda_set()
    moran.test(es$vec, es$W$lw, randomisation = TRUE,
               alternative = "two.sided", zero.policy = TRUE)
  })

  output$w_note <- renderUI({
    es <- esda_set()
    tags$p(style = "font-size:12px; color:#64748B; margin-top:8px;",
      icon("circle-info"),
      sprintf(" %s · %d regions with data · avg. %.1f links per region",
              es$W$desc, es$W$n, es$W$avg_links),
      if (es$W$n_isolated > 0)
        tags$span(style = "color:#EF4444;",
          sprintf(" · %d region(s) without neighbours (excluded from lag)",
                  es$W$n_isolated))
    )
  })

  output$moran_kpi_ui <- renderUI({
    mt  <- moran_global()
    I   <- round(unname(mt$estimate["Moran I statistic"]), 3)
    pv  <- mt$p.value
    ev  <- round(unname(mt$estimate["Expectation"]), 4)
    sdv <- round(sqrt(unname(mt$estimate["Variance"])), 4)

    sig_col   <- if (pv < 0.05) "#10B981" else "#EF4444"
    sig_label <- if (pv < 0.001) "p < 0.001 ✓"
                 else if (pv < 0.05) paste0("p = ", signif(pv, 2), " ✓")
                 else paste0("p = ", signif(pv, 2), " ✗")

    patt <- if (I > 0.2 && pv < 0.05) "Positive clustering"
            else if (I < -0.2 && pv < 0.05) "Dispersion"
            else if (pv >= 0.05) "No significant pattern"
            else "Weak / inconclusive"

    kpi_box <- function(value, label, bg) {
      column(3,
        tags$div(style = paste0("padding:16px; border-radius:10px; background:", bg, ";"),
          tags$div(style = "font-size:26px; font-weight:600; font-family:'Fraunces',serif; color:#fff;", value),
          tags$p(style = "font-size:12px; color:rgba(255,255,255,.85); margin:0;", label)
        )
      )
    }

    fluidRow(
      kpi_box(I,                       "Global Moran's I",  "#0D9488"),
      kpi_box(sig_label,               "Significance",      sig_col),
      kpi_box(paste0(ev, " ± ", sdv),  "E[I] ± sd under H0","#64748B"),
      kpi_box(patt,                    "Pattern",           "#0F172A")
    )
  })

  output$moran_scatter <- renderPlotly({
    es  <- esda_set()
    vec <- es$vec

    z     <- as.numeric(scale(vec))
    lag_z <- as.numeric(scale(lag.listw(es$W$lw, vec, zero.policy = TRUE)))

    quad_col <- dplyr::case_when(
      z > 0 & lag_z > 0 ~ "#B91C1C",
      z < 0 & lag_z < 0 ~ "#1D4ED8",
      z > 0 & lag_z < 0 ~ "#F59E0B",
      z < 0 & lag_z > 0 ~ "#A78BFA",
      TRUE              ~ "#CBD5E1"
    )
    quad_lab <- dplyr::case_when(
      z > 0 & lag_z > 0 ~ "HH",
      z < 0 & lag_z < 0 ~ "LL",
      z > 0 & lag_z < 0 ~ "HL",
      z < 0 & lag_z > 0 ~ "LH",
      TRUE              ~ "-"
    )

    rng  <- max(abs(c(z, lag_z)), na.rm = TRUE) * 1.15
    fit  <- lm(lag_z ~ z)
    xseq <- seq(-rng, rng, length.out = 60)
    yfit <- predict(fit, newdata = data.frame(z = xseq))

    names_keep <- es$sf$region_name[es$keep]
    geo_keep   <- es$sf$geo[es$keep]

    plot_ly() %>%
      add_trace(x = xseq, y = yfit, type = "scatter", mode = "lines",
                line = list(color = "#0D9488", width = 2, dash = "dot"),
                showlegend = FALSE, hoverinfo = "skip") %>%
      add_segments(x = -rng, xend = rng, y = 0, yend = 0,
                   line = list(color = "#E2E8F0", width = 1),
                   showlegend = FALSE, hoverinfo = "skip") %>%
      add_segments(x = 0, xend = 0, y = -rng, yend = rng,
                   line = list(color = "#E2E8F0", width = 1),
                   showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(
        x = z, y = lag_z, type = "scatter", mode = "markers",
        marker = list(size = 7, color = quad_col, opacity = 0.85,
                      line = list(color = "white", width = 0.8)),
        hovertemplate = paste0("<b>%{customdata[0]}</b> (%{customdata[1]})<br>",
                               "z: %{x:.2f} · lag: %{y:.2f}<br>",
                               "Quadrant: %{customdata[2]}<extra></extra>"),
        customdata = cbind(names_keep, geo_keep, quad_lab),
        showlegend = FALSE
      ) %>%
      plotly_base() %>%
      layout(
        xaxis  = list(title = "Standardised value (z)", gridcolor = "#E2E8F0",
                      range = c(-rng, rng), zeroline = FALSE),
        yaxis  = list(title = "Spatial lag (z)", gridcolor = "#E2E8F0",
                      range = c(-rng, rng), zeroline = FALSE),
        margin = list(t = 10, b = 60, l = 70, r = 10)
      )
  })

  output$cluster_map <- renderPlotly({
    es  <- esda_set()
    vec <- es$vec
    n   <- length(vec)

    cluster_full <- rep("No data", nrow(es$sf))

    if (input$cluster_type == "lisa") {
      z      <- as.numeric(scale(vec))
      lag_z  <- as.numeric(scale(lag.listw(es$W$lw, vec, zero.policy = TRUE)))
      lm_mat <- localmoran(vec, es$W$lw, zero.policy = TRUE)
      pcol   <- grep("^Pr", colnames(lm_mat))[1]
      pvals  <- lm_mat[, pcol]

      cl <- dplyr::case_when(
        is.na(pvals) | pvals >= 0.05 ~ "NS",
        z > 0 & lag_z > 0            ~ "HH",
        z < 0 & lag_z < 0            ~ "LL",
        z > 0 & lag_z < 0            ~ "HL",
        z < 0 & lag_z > 0            ~ "LH",
        TRUE                         ~ "NS"
      )
      cluster_full[es$keep] <- cl
      colors <- LISA_COLORS
      breaks <- c("HH", "LL", "HL", "LH", "NS", "No data")
      labels <- c("High-High", "Low-Low", "High-Low", "Low-High",
                  "Not significant", "No data")
      legend_name <- "LISA cluster"
    } else {
      lw_self <- nb2listw(include.self(es$W$nb), style = "W", zero.policy = TRUE)
      gz      <- as.numeric(localG(vec, lw_self))
      cl <- cut(gz,
                breaks = c(-Inf, -2.576, -1.96, 1.96, 2.576, Inf),
                labels = c("Cold spot (99%)", "Cold spot (95%)", "Not significant",
                           "Hot spot (95%)", "Hot spot (99%)"))
      cluster_full[es$keep] <- as.character(cl)
      colors <- GSTAR_COLORS
      breaks <- names(GSTAR_COLORS)
      labels <- names(GSTAR_COLORS)
      legend_name <- "G* z-score class"
    }

    d <- es$sf %>%
      mutate(
        cluster = cluster_full,
        tip     = paste0(region_name, " (", geo, ")\n",
                         "Cluster: ", cluster_full,
                         "\nValue: ", ifelse(is.na(value), "no data",
                                             fmt_val(value, input$sa_var)))
      )

    p <- ggplot(d) +
      geom_sf(aes(fill = cluster, text = tip), colour = "white", linewidth = 0.08) +
      scale_fill_manual(values = colors, name = legend_name,
                        breaks = breaks, labels = labels, na.value = "#F8FAFC") +
      coord_sf(xlim = c(-11, 32), ylim = c(34, 71)) +
      theme_void() +
      theme(legend.position = "right",
            legend.title    = element_text(size = 9, colour = "#64748B"),
            legend.text     = element_text(size = 8))

    ggplotly(p, tooltip = "text") %>%
      plotly_base() %>%
      layout(margin = list(t = 0, b = 0, l = 0, r = 0))
  })

  output$sa_interp_ui <- renderUI({
    mt <- moran_global()
    es <- esda_set()
    I  <- round(unname(mt$estimate["Moran I statistic"]), 3)
    pv <- mt$p.value

    strength <- if (abs(I) > 0.5) "strong" else if (abs(I) > 0.3) "moderate"
                else if (abs(I) > 0.1) "weak" else "negligible"
    direction <- if (I > 0) "positive spatial autocorrelation — similar values cluster together"
                 else "negative spatial autocorrelation — dissimilar values are neighbours"
    sig_text <- if (pv < 0.001) "highly significant (p < 0.001)"
                else if (pv < 0.05) paste0("significant (p = ", signif(pv, 2), ")")
                else paste0("not statistically significant (p = ", signif(pv, 2), ")")

    tags$div(style = "font-size:13px; color:#475569; line-height:1.9;",
      tags$p(
        tags$strong("Global result: "),
        sprintf("Moran's I = %s for %s (%d): %s %s, %s.",
                I, IND_LABEL[input$sa_var], input$sa_year,
                strength, direction, sig_text)
      ),
      tags$p(
        tags$strong("Weights sensitivity: "),
        "switch the W matrix above (queen / KNN / distance band) and watch Moran's I
         change — a robust spatial pattern should remain significant under all
         reasonable specifications. Current: ", es$W$desc, "."
      ),
      tags$p(
        tags$strong("Local view: "),
        "LISA decomposes the global statistic — High-High (red) regions are rich
         cores with rich neighbours; Low-Low (blue) are lagging clusters; High-Low /
         Low-High are spatial outliers. Getis-Ord G* answers a related question
         (where are concentrations of high or low values?) without distinguishing
         outliers; significant hot spots and LISA HH clusters usually overlap."
      ),
      tags$p(style = "font-size:11px; color:#94A3B8;",
        icon("circle-info"),
        " LISA p-values are unadjusted for multiple comparisons (",
        N_REGIONS, " simultaneous tests) — interpret isolated significant regions
         with caution; coherent multi-region clusters are more reliable."
      )
    )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # 3. CONVERGENCE & DYNAMICS
  # ═══════════════════════════════════════════════════════════════════════════

  conv_set <- reactive({
    req(input$conv_var, input$conv_wtype)
    var <- input$conv_var

    # Years with at least 70% regional coverage for this indicator
    cov <- panel %>%
      filter(year %in% YEARS) %>%
      group_by(year) %>%
      summarise(p = mean(!is.na(.data[[var]])), .groups = "drop")
    yrs <- cov$year[cov$p >= 0.7]
    validate(need(length(yrs) >= 5,
      "This indicator has good regional coverage in fewer than 5 years — choose another indicator."))

    # Balanced panel: regions observed in every retained year
    wide <- panel %>%
      filter(year %in% yrs) %>%
      select(geo, year, value = all_of(var)) %>%
      pivot_wider(names_from = year, values_from = value) %>%
      filter(if_all(-geo, ~ !is.na(.)))

    keep <- mapN2$geo %in% wide$geo
    validate(need(sum(keep) >= 30,
      "The balanced panel has fewer than 30 regions — choose another indicator."))
    geo_keep <- mapN2$geo[keep]
    M <- as.matrix(wide[match(geo_keep, wide$geo), as.character(sort(yrs))])

    W <- build_weights(keep,
                       type = ifelse(input$conv_wtype == "queen", "queen", "knn"),
                       k = 4)

    stats <- lapply(seq_along(sort(yrs)), function(j) {
      v  <- M[, j]
      mt <- moran.test(v, W$lw, randomisation = TRUE,
                       alternative = "two.sided", zero.policy = TRUE)
      data.frame(
        year = sort(yrs)[j],
        I    = unname(mt$estimate["Moran I statistic"]),
        E    = unname(mt$estimate["Expectation"]),
        p    = mt$p.value,
        cv   = sd(v) / mean(v) * 100
      )
    })
    list(df = do.call(rbind, stats), n = sum(keep), yrs = sort(yrs), W = W)
  })

  output$conv_note_ui <- renderUI({
    cs <- conv_set()
    tags$div(style = "font-size:13px; color:#475569; line-height:1.8;",
      tags$p(
        tags$strong("Balanced panel: "), cs$n, " regions observed in every year from ",
        min(cs$yrs), " to ", max(cs$yrs),
        " (regions or years with missing data are excluded so that Moran's I is
         computed on the same spatial system in every period — otherwise changes
         in I could simply reflect changes in the sample)."
      ),
      tags$p(style = "font-size:12px; color:#64748B;",
        icon("circle-info"), " Weights: ", cs$W$desc,
        " · avg. ", cs$W$avg_links, " links per region."
      )
    )
  })

  output$conv_moran_line <- renderPlotly({
    d <- conv_set()$df
    plot_ly() %>%
      add_trace(x = d$year, y = d$E, type = "scatter", mode = "lines",
                name = "E[I] under H0",
                line = list(color = "#94A3B8", width = 1.5, dash = "dot"),
                hoverinfo = "skip") %>%
      add_trace(
        x = d$year, y = d$I, type = "scatter", mode = "lines+markers",
        name = "Moran's I",
        line = list(color = "#0D9488", width = 3),
        marker = list(size = 9,
                      color = ifelse(d$p < 0.05, "#0D9488", "#CBD5E1"),
                      line = list(color = "white", width = 1.5)),
        hovertemplate = paste0("Year %{x}<br>Moran's I: %{y:.3f}<br>",
                               "p-value: %{customdata:.4f}<extra></extra>"),
        customdata = d$p
      ) %>%
      plotly_base() %>%
      layout(
        xaxis  = list(title = "Year", dtick = 2, gridcolor = "#E2E8F0"),
        yaxis  = list(title = "Global Moran's I", gridcolor = "#E2E8F0"),
        legend = list(orientation = "h", y = -0.25),
        margin = list(t = 10, b = 70, l = 70, r = 10)
      )
  })

  output$conv_cv_line <- renderPlotly({
    d <- conv_set()$df
    plot_ly(
      x = d$year, y = d$cv, type = "scatter", mode = "lines+markers",
      line = list(color = "#F59E0B", width = 3),
      marker = list(size = 8, color = "#F59E0B",
                    line = list(color = "white", width = 1.5)),
      hovertemplate = "Year %{x}<br>CV: %{y:.1f}%<extra></extra>"
    ) %>%
      plotly_base() %>%
      layout(
        xaxis  = list(title = "Year", dtick = 2, gridcolor = "#E2E8F0"),
        yaxis  = list(title = "Coefficient of variation (%)", gridcolor = "#E2E8F0"),
        margin = list(t = 10, b = 50, l = 70, r = 10)
      )
  })

  output$conv_interp_ui <- renderUI({
    d  <- conv_set()$df
    dI  <- d$I[nrow(d)]  - d$I[1]
    dCV <- d$cv[nrow(d)] - d$cv[1]

    cv_text <- if (dCV < -1)
      "regional dispersion has fallen — evidence of σ-convergence (poorer regions catching up)"
    else if (dCV > 1)
      "regional dispersion has risen — σ-divergence (gaps between regions are widening)"
    else
      "regional dispersion is roughly stable"

    I_text <- if (dI > 0.05)
      "spatial clustering has strengthened: similar regions are increasingly found next to each other"
    else if (dI < -0.05)
      "spatial clustering has weakened"
    else
      "the strength of spatial clustering is roughly stable"

    tags$div(style = "font-size:13px; color:#475569; line-height:1.9;",
      tags$p(
        tags$strong("Between ", min(d$year), " and ", max(d$year), ": "),
        sprintf("the CV moved from %.1f%% to %.1f%% (%+.1f), i.e. %s. ",
                d$cv[1], d$cv[nrow(d)], dCV, cv_text),
        sprintf("Moran's I moved from %.3f to %.3f (%+.3f): %s.",
                d$I[1], d$I[nrow(d)], dI, I_text)
      ),
      tags$p(
        tags$strong("Why read them together: "),
        "convergence (falling CV) can coexist with rising Moran's I — regions can
         become more equal overall while geography becomes more organised into
         contiguous rich and poor macro-areas. That combination is exactly what much
         of the EU cohesion literature documents: national gaps shrink while
         east-west and core-periphery clusters persist."
      )
    )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # 4. SPATIAL REGRESSION
  # ═══════════════════════════════════════════════════════════════════════════

  reg_set <- reactive({
    req(input$reg_year, length(input$reg_predictors) >= 1)
    vars <- c("gdp_pps_hab", input$reg_predictors)
    df <- panel %>%
      filter(year == input$reg_year) %>%
      select(geo, all_of(vars))
    df <- df[complete.cases(df), ]
    df <- df[df$gdp_pps_hab > 0, ]

    keep <- mapN2$geo %in% df$geo
    validate(need(sum(keep) >= 30,
      "Fewer than 30 regions have complete data for this year and predictor set — try another year."))
    geo_keep <- mapN2$geo[keep]
    df <- df[match(geo_keep, df$geo), ]

    W <- build_weights(keep,
                       type = ifelse(input$reg_wtype == "queen", "queen", "knn"),
                       k = 4)
    f <- as.formula(paste("log(gdp_pps_hab) ~",
                          paste(input$reg_predictors, collapse = " + ")))
    list(df = df, W = W, f = f, n = nrow(df))
  })

  reg_models <- reactive({
    rs <- reg_set()
    ols <- lm(rs$f, data = rs$df)
    sar <- tryCatch(
      lagsarlm(rs$f, data = rs$df, listw = rs$W$lw, zero.policy = TRUE),
      error = function(e) NULL)
    sem <- tryCatch(
      errorsarlm(rs$f, data = rs$df, listw = rs$W$lw, zero.policy = TRUE),
      error = function(e) NULL)
    list(ols = ols, sar = sar, sem = sem, rs = rs)
  })

  star_str <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) "***" else if (p < 0.01) "**"
    else if (p < 0.05) "*" else if (p < 0.1) "." else ""
  }

  output$reg_table <- DT::renderDataTable({
    m  <- reg_models()
    rs <- m$rs
    terms <- c("(Intercept)", input$reg_predictors)
    PRED_LABEL <- setNames(names(PREDICTORS), unname(PREDICTORS))
    term_labels <- c("Intercept", unname(PRED_LABEL[input$reg_predictors]))

    co_ols <- summary(m$ols)$coefficients
    col_ols <- vapply(terms, function(t)
      if (t %in% rownames(co_ols))
        sprintf("%.4f%s", co_ols[t, 1], star_str(co_ols[t, 4])) else "-",
      character(1))

    get_sp_col <- function(mod) {
      if (is.null(mod)) return(rep("model failed", length(terms)))
      sc <- summary(mod)$Coef
      vapply(terms, function(t)
        if (t %in% rownames(sc))
          sprintf("%.4f%s", sc[t, 1], star_str(sc[t, 4])) else "-",
        character(1))
    }
    col_sar <- get_sp_col(m$sar)
    col_sem <- get_sp_col(m$sem)

    res_moran <- function(mod) {
      if (is.null(mod)) return("-")
      mt <- moran.test(residuals(mod), rs$W$lw, zero.policy = TRUE)
      sprintf("%.3f (p = %s)",
              unname(mt$estimate["Moran I statistic"]), signif(mt$p.value, 2))
    }

    rho_str <- if (!is.null(m$sar))
      sprintf("%.4f%s", m$sar$rho, star_str(summary(m$sar)$LR1$p.value)) else "-"
    lam_str <- if (!is.null(m$sem))
      sprintf("%.4f%s", m$sem$lambda, star_str(summary(m$sem)$LR1$p.value)) else "-"

    tab <- data.frame(
      Term          = c(term_labels,
                        "rho (spatial lag of y)", "lambda (spatial error)",
                        "AIC", "Residual Moran's I", "Observations"),
      OLS           = c(col_ols, "-", "-",
                        sprintf("%.1f", AIC(m$ols)), res_moran(m$ols), rs$n),
      `SAR (lag)`   = c(col_sar, rho_str, "-",
                        if (!is.null(m$sar)) sprintf("%.1f", AIC(m$sar)) else "-",
                        res_moran(m$sar), rs$n),
      `SEM (error)` = c(col_sem, "-", lam_str,
                        if (!is.null(m$sem)) sprintf("%.1f", AIC(m$sem)) else "-",
                        res_moran(m$sem), rs$n),
      check.names = FALSE
    )

    DT::datatable(
      tab,
      options  = list(dom = "t", ordering = FALSE, pageLength = nrow(tab)),
      rownames = FALSE,
      class    = "stripe hover compact",
      caption  = htmltools::tags$caption(
        style = "caption-side: bottom; text-align: left; font-size: 11px; color: #94A3B8;",
        "Dependent variable: log GDP per capita (PPS). Significance: *** p<0.001,
         ** p<0.01, * p<0.05, . p<0.1. Spatial models estimated by maximum
         likelihood; rho / lambda significance from likelihood-ratio tests."
      )
    )
  })

  output$reg_interp_ui <- renderUI({
    m  <- reg_models()
    rs <- m$rs

    mt_ols <- moran.test(residuals(m$ols), rs$W$lw, zero.policy = TRUE)
    ols_dep <- mt_ols$p.value < 0.05

    aics <- c(OLS = AIC(m$ols),
              `SAR (lag)`   = if (!is.null(m$sar)) AIC(m$sar) else NA,
              `SEM (error)` = if (!is.null(m$sem)) AIC(m$sem) else NA)
    best <- names(which.min(aics))

    rho_sig <- !is.null(m$sar) && summary(m$sar)$LR1$p.value < 0.05

    tags$div(style = "font-size:13px; color:#475569; line-height:1.9;",
      tags$p(
        tags$strong("Diagnostics: "),
        if (ols_dep)
          sprintf("OLS residuals are spatially autocorrelated (Moran's I = %.3f,
                   p = %s) — plain OLS is misspecified here, and a spatial model
                   is warranted. ",
                  unname(mt_ols$estimate["Moran I statistic"]),
                  signif(mt_ols$p.value, 2))
        else
          "OLS residuals show no significant spatial autocorrelation — spatial
           models may not be strictly necessary for this specification. ",
        sprintf("By AIC the best-fitting model is the %s.", best)
      ),
      if (rho_sig) tags$p(
        tags$strong("Spillovers: "),
        sprintf("the spatial lag parameter rho = %.3f is significant: a region's
                 GDP per capita co-moves with its neighbours' GDP even after
                 controlling for its own characteristics. Note that in a SAR model
                 coefficients are not marginal effects — total impacts include
                 feedback through the neighbourhood system.", m$sar$rho)
      ),
      tags$p(style = "font-size:11px; color:#94A3B8;",
        icon("triangle-exclamation"),
        " Caveats: cross-sectional models — no causal claims; results depend on the
          chosen W (try queen vs. KNN); predictors are themselves spatially
          clustered and possibly endogenous. For the exam, present this as
          a specification exercise, not as causal evidence."
      )
    )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # 5. DATA TABLE
  # ═══════════════════════════════════════════════════════════════════════════

  output$full_table <- DT::renderDataTable({
    req(input$data_year)
    d <- panel %>%
      filter(year == input$data_year) %>%
      left_join(meta, by = "geo") %>%
      transmute(
        Code                    = geo,
        Region                  = region_name,
        Country                 = country_name,
        `GDP pc (PPS)`          = gdp_pps_hab,
        `GDP pc (EUR)`          = gdp_eur_hab,
        `Income pc (PPS)`       = income_pps_hab,
        `Unemployment (%)`      = unemp_rate,
        `Life expectancy`       = life_exp,
        `Tertiary educ. (%)`    = tertiary_pct
      ) %>%
      arrange(desc(`GDP pc (PPS)`))

    DT::datatable(
      d,
      extensions = "Buttons",
      options = list(
        dom        = "Bfrtip",
        buttons    = list("csv", "excel"),
        pageLength = 15,
        scrollX    = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class    = "stripe hover compact"
    ) %>%
      DT::formatRound(c("GDP pc (PPS)", "GDP pc (EUR)", "Income pc (PPS)"),
                      digits = 0, mark = ",") %>%
      DT::formatRound(c("Unemployment (%)", "Life expectancy", "Tertiary educ. (%)"),
                      digits = 1) %>%
      DT::formatStyle(
        "GDP pc (PPS)",
        background         = DT::styleColorBar(range(d$`GDP pc (PPS)`, na.rm = TRUE), "#99F6E4"),
        backgroundSize     = "100% 80%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })

}
