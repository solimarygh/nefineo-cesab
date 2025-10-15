# app.R — WP2 visual analysis dashboard
# Solimary (Soly) — starter app to explore WP2_dataset_allvars interactively
# ---------------------------------------------------------------
# Packages
suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(plotly)
  library(DT)
  library(leaflet)
  library(sf)
})

# ---------------------------------------------------------------
# 1) Load data
# Option A: if you already have WP2_dataset_allvars in your R session, set use_global <- TRUE
# Option B: provide a CSV path below
use_global <- exists("WP2_dataset_allvars", inherits = TRUE)

if (!use_global) {
  # <- edit the path as needed
  csv_path <- "WP2_dataset_allvars.csv"
  validate <- function(x) if (!file.exists(x)) stop("CSV not found. Update csv_path or load data into environment as WP2_dataset_allvars.")
  validate(csv_path)
  WP2_dataset_allvars <- readr::read_csv(csv_path, show_col_types = FALSE)
}

df <- WP2_dataset_allvars %>%
  mutate(
    year_of_sampling = as.integer(year_of_sampling),
    Biome = as.factor(Biome),
    Biome_detail = as.factor(Biome_detail),
    Holdridge_class = as.factor(Holdridge_class),
    country = as.factor(country)
  )

# ---------------------------------------------------------------
# 2) Helpers

# A sober, modern palette (fallback if paletta_Obia not available)
paletta_Obia <- c("#0f172a", "#1e293b", "#334155", "#475569", "#64748b", "#94a3b8", "#cbd5e1")

wrap_title <- function(txt, width = 40) {
  paste(strwrap(txt, width = width), collapse = "<br>")
}

h_legend <- function(p) {
  p %>% layout(legend = list(orientation = "h", x = 0, y = 1.12))
}

num_cols <- df %>% select(where(is.numeric)) %>% names()

# monthly climate columns for climographs
month_levels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
col_month <- function(prefix) paste0(prefix, month_levels)
cols_tavg <- paste0("climenv_monthavg_tavg_m_", month_levels)
cols_tmin <- paste0("climenv_monthavg_tmin_m_", month_levels)
cols_tmax <- paste0("climenv_monthavg_tmax_m_", month_levels)
cols_prec <- paste0("climenv_monthavg_prec_m_", month_levels)

# Site-level consistency check (are per-sample values constant within new.ID?)
constancy_check <- function(data, by = "new.ID", cols = c("MAT","MAP","nitrogen","sand","cec","soc","phh2o","silt","Plant_richness_res7774","Plant_richness_res23322","PET_annual","TAP_annual","PER")) {
  cols <- intersect(cols, names(data))
  data %>%
    group_by(.data[[by]]) %>%
    summarize(across(all_of(cols), ~ dplyr::n_distinct(.x, na.rm = TRUE), .names = "nlevels_{.col}"), .groups = "drop") %>%
    mutate(across(starts_with("nlevels_"), ~ .x <= 1))
}

# Build long monthly data for a selected site
get_climograph <- function(data, new_id) {
  row <- data %>% filter(new.ID == new_id) %>% slice(1)
  if (nrow(row) == 0) return(tibble())
  tibble(
    month = factor(month_levels, levels = month_levels),
    Tmean = as.numeric(row[1, cols_tavg]),
    Tmin  = as.numeric(row[1, cols_tmin]),
    Tmax  = as.numeric(row[1, cols_tmax]),
    Prec  = as.numeric(row[1, cols_prec])
  )
}

# Leaflet palette factory
leaf_pal <- function(values) colorFactor(palette = paletta_Obia, domain = values, na.color = "#9ca3af")

# ---------------------------------------------------------------
# 3) UI
ui <- navbarPage(
  title = "WP2 — Visual Analysis",
  id = "tabs",
  theme = bslib::bs_theme(version = 5, base_font = bslib::font_google("Inter")),

  tabPanel("Overview",
    fluidPage(
      br(),
      fluidRow(
        column(4,
          selectInput("var_hist", "Numeric variable (histogram)", choices = num_cols, selected = "MAT"),
          plotlyOutput("hist_plot", height = 300)
        ),
        column(4,
          selectInput("var_box", "Numeric by Biome (box)", choices = num_cols, selected = "MAT"),
          plotlyOutput("box_biome", height = 300)
        ),
        column(4,
          selectInput("var_box2", "Numeric by Holdridge (box)", choices = num_cols, selected = "MAP"),
          plotlyOutput("box_holdridge", height = 300)
        )
      ),
      br(),
      fluidRow(
        column(6,
          plotlyOutput("year_count", height = 320)
        ),
        column(6,
          DTOutput("const_table")
        )
      )
    )
  ),

  tabPanel("Map",
    fluidPage(
      br(),
      fluidRow(
        column(3,
          selectInput("color_map", "Color by:", choices = c("Holdridge_class","Biome","country","morrone_biogeoregions_Provincias","wwf_ecoregions_BIOME"), selected = "Holdridge_class"),
          checkboxInput("cluster_map", "Cluster markers", value = TRUE),
          helpText("Tip: zoom in to inspect dense areas.")
        ),
        column(9,
          leafletOutput("map", height = 580)
        )
      )
    )
  ),

  tabPanel("Bivariate",
    fluidPage(
      br(),
      fluidRow(
        column(3,
          selectInput("xvar", "X variable", choices = num_cols, selected = "MAT"),
          selectInput("yvar", "Y variable", choices = num_cols, selected = "MAP"),
          selectInput("colorby", "Color by", choices = c("Holdridge_class","Biome","country"), selected = "Holdridge_class"),
          checkboxInput("hex", "Hexbin (density)", value = FALSE)
        ),
        column(9, plotlyOutput("xy_plot", height = 520))
      )
    )
  ),

  tabPanel("Correlation",
    fluidPage(
      br(),
      fluidRow(
        column(3,
          selectizeInput("corr_vars", "Variables for correlation", choices = num_cols, selected = c("MAT","MAP","nitrogen","sand","cec","soc","phh2o","silt"), multiple = TRUE)
        ),
        column(9,
          plotlyOutput("corr_heat", height = 540)
        )
      )
    )
  ),

  tabPanel("Climograph",
    fluidPage(
      br(),
      fluidRow(
        column(3,
          selectInput("site", "Site (new.ID)", choices = sort(unique(df$new.ID)), selected = sort(unique(df$new.ID))[1])
        ),
        column(9,
          plotlyOutput("climo", height = 520)
        )
      )
    )
  ),

  tabPanel("Bioregions",
    fluidPage(
      br(),
      fluidRow(
        column(6, plotlyOutput("morrone_bar", height = 400)),
        column(6, plotlyOutput("wwf_bar", height = 400))
      ),
      br(),
      fluidRow(
        column(12, DTOutput("summary_table"))
      )
    )
  )
)

# ---------------------------------------------------------------
# 4) Server
server <- function(input, output, session) {

  # ---- Overview
  output$hist_plot <- renderPlotly({
    v <- sym(input$var_hist)
    p <- ggplot(df, aes(x = !!v)) +
      geom_histogram(bins = 30, fill = paletta_Obia[5]) +
      labs(title = wrap_title(paste0("Distribution of ", input$var_hist)), x = input$var_hist, y = "Count") +
      theme_minimal()
    ggplotly(p) %>% h_legend()
  })

  output$box_biome <- renderPlotly({
    v <- sym(input$var_box)
    p <- ggplot(df, aes(x = Biome, y = !!v, fill = Biome)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.8) +
      coord_flip() +
      scale_fill_manual(values = rep(paletta_Obia, length.out = nlevels(df$Biome))) +
      labs(title = wrap_title(paste0(input$var_box, " by Biome")), x = "Biome", y = input$var_box) +
      theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })

  output$box_holdridge <- renderPlotly({
    v <- sym(input$var_box2)
    p <- ggplot(df, aes(x = Holdridge_class, y = !!v, fill = Holdridge_class)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.8) +
      coord_flip() +
      scale_fill_manual(values = rep(paletta_Obia, length.out = nlevels(df$Holdridge_class))) +
      labs(title = wrap_title(paste0(input$var_box2, " by Holdridge class")), x = "Holdridge", y = input$var_box2) +
      theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })

  output$year_count <- renderPlotly({
    p <- df %>%
      count(year_of_sampling) %>%
      arrange(year_of_sampling) %>%
      ggplot(aes(x = year_of_sampling, y = n)) +
      geom_col(fill = paletta_Obia[4]) +
      labs(title = wrap_title("Sampling effort by year"), x = "Year", y = "Samples") +
      theme_minimal()
    ggplotly(p)
  })

  output$const_table <- renderDT({
    constancy_check(df) %>%
      datatable(options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  # ---- Map
  output$map <- renderLeaflet({
    req(input$color_map)
    col_field <- df[[input$color_map]]
    pal <- leaf_pal(col_field)

    leaflet(df, options = leafletOptions(attributionControl = FALSE)) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        color = ~pal(df[[input$color_map]]),
        radius = 5, stroke = FALSE, fillOpacity = 0.85,
        popup = ~paste0("<b>", new.ID, "</b><br>",
                        "Biome: ", Biome, "<br>",
                        "Holdridge: ", Holdridge_class, "<br>",
                        "Country: ", country)
      ) %>%
      addLegend("bottomright", pal = pal, values = col_field, title = input$color_map)
  })

  observeEvent(input$cluster_map, {
    map_proxy <- leafletProxy("map")
    if (isTRUE(input$cluster_map)) {
      map_proxy %>% clearMarkers() %>% addMarkers(
        lng = df$longitude, lat = df$latitude,
        clusterOptions = markerClusterOptions(),
        popup = paste0("<b>", df$new.ID, "</b><br>Biome: ", df$Biome,
                       "<br>Holdridge: ", df$Holdridge_class, "<br>Country: ", df$country)
      )
    } else {
      map_proxy %>% clearMarkers() %>% addCircleMarkers(
        lng = df$longitude, lat = df$latitude, radius = 5, stroke = FALSE,
        fillOpacity = 0.85, color = leaf_pal(df[[input$color_map]])(df[[input$color_map]]),
        popup = paste0("<b>", df$new.ID, "</b><br>Biome: ", df$Biome,
                       "<br>Holdridge: ", df$Holdridge_class, "<br>Country: ", df$country)
      )
    }
  })

  # ---- Bivariate
  output$xy_plot <- renderPlotly({
    req(input$xvar, input$yvar)

    if (isTRUE(input$hex)) {
      p <- ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
        geom_hex(bins = 30) +
        labs(title = wrap_title(paste0(input$yvar, " vs ", input$xvar)), x = input$xvar, y = input$yvar) +
        theme_minimal()
      ggplotly(p)
    } else {
      p <- ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]], color = .data[[input$colorby]])) +
        geom_point(alpha = 0.8) +
        scale_color_manual(values = rep(paletta_Obia, length.out = nlevels(as.factor(df[[input$colorby]])))) +
        labs(title = wrap_title(paste0(input$yvar, " vs ", input$xvar)), x = input$xvar, y = input$yvar, color = input$colorby) +
        theme_minimal()
      ggplotly(p) %>% h_legend()
    }
  })

  # ---- Correlation heatmap
  output$corr_heat <- renderPlotly({
    vars <- input$corr_vars
    req(length(vars) >= 2)
    m <- df %>% select(all_of(vars)) %>% mutate(across(everything(), as.numeric)) %>% cor(use = "pairwise.complete.obs")
    m_long <- as.data.frame(as.table(m))
    names(m_long) <- c("Var1","Var2","Corr")
    p <- ggplot(m_long, aes(x = Var1, y = Var2, fill = Corr, text = sprintf("%s–%s: %.2f", Var1, Var2, Corr))) +
      geom_tile() +
      scale_fill_gradient2(low = "#2563eb", mid = "#e5e7eb", high = "#ef4444", midpoint = 0) +
      coord_fixed() +
      labs(title = wrap_title("Correlation matrix"), x = NULL, y = NULL, fill = "r") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  # ---- Climograph (per site)
  output$climo <- renderPlotly({
    dat <- get_climograph(df, input$site)
    req(nrow(dat) > 0)

    p1 <- plot_ly(dat, x = ~month, y = ~Tmean, type = 'scatter', mode = 'lines+markers', name = 'Tmean') %>%
      add_trace(y = ~Tmin, name = 'Tmin', mode = 'lines+markers') %>%
      add_trace(y = ~Tmax, name = 'Tmax', mode = 'lines+markers') %>%
      add_bars(y = ~Prec, name = 'Precipitation', yaxis = 'y2', opacity = 0.5) %>%
      layout(
        title = list(text = wrap_title(paste0("Climograph — ", input$site))),
        yaxis = list(title = "Temperature (°C)"),
        yaxis2 = list(overlaying = 'y', side = 'right', title = 'Precipitation (mm)')
      ) %>% h_legend()
    p1
  })

  # ---- Bioregions
  output$morrone_bar <- renderPlotly({
    p <- df %>%
      count(morrone_biogeoregions_Provincias, sort = TRUE) %>%
      slice_head(n = 20) %>%
      ggplot(aes(x = reorder(morrone_biogeoregions_Provincias, n), y = n, fill = n)) +
      geom_col() + coord_flip() +
      scale_fill_gradient(low = paletta_Obia[4], high = paletta_Obia[1]) +
      labs(title = wrap_title("Top 20 Morrone Provinces by sample count"), x = "Province", y = "Samples") +
      theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })

  output$wwf_bar <- renderPlotly({
    p <- df %>%
      count(wwf_ecoregions_BIOME, sort = TRUE) %>%
      ggplot(aes(x = reorder(wwf_ecoregions_BIOME, n), y = n, fill = n)) +
      geom_col() + coord_flip() +
      scale_fill_gradient(low = paletta_Obia[4], high = paletta_Obia[1]) +
      labs(title = wrap_title("WWF biomes by sample count"), x = "WWF biome", y = "Samples") +
      theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })

  output$summary_table <- renderDT({
    df %>%
      transmute(
        new.ID, country, Biome, Holdridge_class,
        MAT, MAP, PET_annual, TAP_annual, PER,
        nitrogen, sand, cec, soc, phh2o, silt
      ) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
}

# ---------------------------------------------------------------
shinyApp(ui, server)

