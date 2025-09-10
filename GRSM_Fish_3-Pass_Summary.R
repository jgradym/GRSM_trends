library(readxl)
library(tidyverse)
library(readxl)
library(patchwork)
library(lubridate)
library(mgcv)
library(grid)    
library(gtable)  
library(gridExtra)

fish_pop <- read_xlsx(
  "/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Old/GRSM File Transfers/GRSM_Fish_3-Pass_Summary.xlsx",
  sheet = "Summary"
)
str(fish_pop)
# ===== Community metrics — FAST nested RE (Stream + Site-within-Stream) with mgcv::bam =====
# Produces multi-panel PDF (4 per page) with a single black population-level smooth per plot.
# Estimand (black line) defaults to: expected value for a *typical site* (exclude both REs).
# -------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(lubridate)
library(mgcv)
library(gridExtra)
library(rlang)

###### Mixed model, with random effect is stream:site


# ---------------- CONFIG ----------------
SITE_COL   <- "Site"     # <-- change if your site column is named differently
OUT_FILE   <- "~/Downloads/community_metrics_nestedRE_fast.pdf"

# Smoothing / speed
K_SMOOTH   <- 10         # basis dimension for s(Year) (try 7–10)
YEAR_BY    <- 0.1        # prediction grid step in years (smaller => smoother black line)
GAMMA_PEN  <- 1.0        # >1 adds extra smoothness penalty (e.g., 1.1–1.2); 1.0 = off
PT_SIZE    <- 0.8        # point size
N_THREADS  <- max(1, parallel::detectCores() - 1)

# Target for black line ("avg_site" | "avg_stream_sum" | "network_sum")
TARGET     <- "avg_site"

# Families per metric
fam_nb     <- mgcv::nb()                 # counts
fam_gamma  <- Gamma(link = "log")        # positive continuous

# ---------------- HELPERS ----------------

# Stream–year totals (for colored lines/points; unchanged)
make_stream_year <- function(df, var) {
  df %>%
    dplyr::select(Stream, Date, {{ var }}) %>%
    mutate(Year = lubridate::year(Date)) %>%
    group_by(Stream, Year) %>%
    summarize(Total = sum({{ var }}, na.rm = TRUE), .groups = "drop") %>%
    filter(Total > 0)
}

# Site×Stream×Year totals for modeling; build nested SiteID
make_site_stream_year <- function(df, var, site_col = SITE_COL) {
  site_sym <- rlang::sym(site_col)
  df %>%
    dplyr::select(Stream, !!site_sym, Date, {{ var }}) %>%
    mutate(
      Year   = lubridate::year(Date),
      SiteID = interaction(Stream, !!site_sym, drop = TRUE, lex.order = TRUE)
    ) %>%
    group_by(Stream, SiteID, Year) %>%
    summarize(Total = sum({{ var }}, na.rm = TRUE), .groups = "drop") %>%
    filter(Total > 0)
}

# FAST nested-RE fit with bam() and marginal smooth prediction
fit_comm_bam_nestedRE <- function(sy_site, family, k = K_SMOOTH, year_by = YEAR_BY,
                                  gamma_pen = GAMMA_PEN, target = TARGET,
                                  nthreads = N_THREADS) {
  sy_site <- sy_site %>%
    mutate(
      Year   = as.numeric(Year),
      Stream = as.factor(Stream),
      SiteID = as.factor(SiteID)
    )
  if (n_distinct(sy_site$Year) < 3 || nrow(sy_site) < 3) return(NULL)
  k_eff <- max(3, min(k, n_distinct(sy_site$Year) - 1))
  
  # Primary fit on original scale with log-link family (NB/Gamma)
  fit_try <- try(
    mgcv::bam(
      Total ~ s(Year, k = k_eff, bs = "cr") +
        s(Stream, bs = "re") +
        s(SiteID, bs = "re"),
      family   = family,
      data     = sy_site,
      method   = "fREML",
      discrete = TRUE,
      nthreads = nthreads,
      gamma    = gamma_pen
    ),
    silent = TRUE
  )
  
  use_link_exp <- FALSE
  if (inherits(fit_try, "try-error")) {
    # Fallback: Gaussian on log(Total) (very fast & robust)
    fit_try <- try(
      mgcv::bam(
        log(Total) ~ s(Year, k = k_eff, bs = "cr") +
          s(Stream, bs = "re") +
          s(SiteID, bs = "re"),
        family   = gaussian(),
        data     = sy_site,
        method   = "fREML",
        discrete = TRUE,
        nthreads = nthreads,
        gamma    = gamma_pen
      ),
      silent = TRUE
    )
    if (inherits(fit_try, "try-error")) return(NULL)
    use_link_exp <- TRUE
  }
  
  yrs <- seq(min(sy_site$Year), max(sy_site$Year), by = year_by)
  ref_stream <- levels(sy_site$Stream)[1]
  ref_site   <- levels(sy_site$SiteID)[1]
  
  nd <- data.frame(
    Year   = yrs,
    Stream = factor(ref_stream, levels = levels(sy_site$Stream)),
    SiteID = factor(ref_site,   levels = levels(sy_site$SiteID))
  )
  
  # Exclude REs to get a "typical site" expectation on the link scale
  if (use_link_exp) {
    eta  <- predict(fit_try, newdata = nd, type = "link",
                    exclude = c("s(Stream)", "s(SiteID)"))
    mu_site <- exp(eta)
  } else {
    mu_site <- predict(fit_try, newdata = nd, type = "response",
                       exclude = c("s(Stream)", "s(SiteID)"))
  }
  
  # Aggregate to chosen estimand on ORIGINAL scale
  site_counts <- sy_site %>%
    group_by(Stream, Year) %>% summarise(n_sites = n_distinct(SiteID), .groups = "drop") %>%
    group_by(Stream) %>% summarise(n_ref = stats::median(n_sites), .groups = "drop")
  mean_sites_per_stream <- if (nrow(site_counts)) mean(site_counts$n_ref) else 1
  total_sites_network   <- if (nrow(site_counts)) sum(site_counts$n_ref)  else 1
  
  yhat <- switch(
    target,
    "avg_site"       = mu_site,
    "avg_stream_sum" = mu_site * mean_sites_per_stream,
    "network_sum"    = mu_site * total_sites_network,
    mu_site
  )
  
  tibble::tibble(Year = yrs, yhat = yhat)
}

# Plot factory (same look; black line from bam nested-RE)
plot_metric_comm_nestedRE <- function(df, var, ylab, title_txt, family,
                                      k = K_SMOOTH, year_by = YEAR_BY,
                                      site_col = SITE_COL, target = TARGET,
                                      gamma_pen = GAMMA_PEN, nthreads = N_THREADS) {
  sy_plot <- make_stream_year(df, {{ var }})
  sy_site <- make_site_stream_year(df, {{ var }}, site_col)
  
  if (nrow(sy_plot) == 0 || nrow(sy_site) == 0) {
    return(ggplot() + theme_void() + ggtitle(paste0(title_txt, " — (no positive data)")))
  }
  
  smooth_df <- if (n_distinct(sy_site$Year) >= 3)
    fit_comm_bam_nestedRE(sy_site, family = family, k = k, year_by = year_by,
                          gamma_pen = gamma_pen, target = target, nthreads = nthreads) else NULL
  
  sy_line <- sy_plot %>% group_by(Stream) %>% filter(dplyr::n() >= 2) %>% ungroup()
  
  ggplot(sy_plot, aes(Year, Total, color = Stream, group = Stream)) +
    geom_line(data = sy_line) +
    geom_point(size = PT_SIZE) +
    scale_y_log10() +
    { if (!is.null(smooth_df))
      geom_line(data = smooth_df, aes(Year, yhat),
                inherit.aes = FALSE, color = "black", linewidth = 1.2) else NULL } +
    labs(x = "Year", y = ylab, title = title_txt) +
    theme_bw() +
    scale_color_viridis_d() +
    theme(legend.position = "none")
}

# ---------------- BUILD PLOTS ----------------
plots <- list(
  # Counts -> Negative Binomial
  plot_metric_comm_nestedRE(fish_pop, TOTpop,  "Total abundance (count)",         "# Total Abundance",         family = fam_nb),
  plot_metric_comm_nestedRE(fish_pop, YOYpop,  "Young-of-Year abundance (count)", "# Young-of-Year Abundance", family = fam_nb),
  plot_metric_comm_nestedRE(fish_pop, ADTpop,  "Adult abundance (count)",         "# Adult Abundance",         family = fam_nb),
  
  # Positive continuous -> Gamma(log)
  plot_metric_comm_nestedRE(fish_pop, TOTDens, "Total density",                   "# Total Density",           family = fam_gamma),
  plot_metric_comm_nestedRE(fish_pop, YOYDens, "Young-of-Year density",           "# Young-of-Year Density",   family = fam_gamma),
  plot_metric_comm_nestedRE(fish_pop, ADTDens, "Adult density",                   "# Adult Density",           family = fam_gamma),
  
  plot_metric_comm_nestedRE(fish_pop, TOTBiom, "Total biomass",                   "# Total Biomass",           family = fam_gamma),
  plot_metric_comm_nestedRE(fish_pop, YOYBiom, "Young-of-Year biomass",           "# Young-of-Year Biomass",   family = fam_gamma),
  plot_metric_comm_nestedRE(fish_pop, ADTBiom, "Adult biomass",                   "# Adult Biomass",           family = fam_gamma),
  
  plot_metric_comm_nestedRE(fish_pop, TOTmnwt, "Mean weight (all, g)",            "# Mean Weight (All)",       family = fam_gamma),
  plot_metric_comm_nestedRE(fish_pop, YOYmnwt, "Mean weight (Young-of-Year, g)",  "# Mean Weight (Young-of-Year)", family = fam_gamma),
  plot_metric_comm_nestedRE(fish_pop, ADTmnwt, "Mean weight (Adult, g)",          "# Mean Weight (Adult)",     family = fam_gamma)
)

# ---------------- PAGINATE & WRITE ----------------
idx_groups <- split(seq_along(plots), ceiling(seq_along(plots) / 4))
pages <- lapply(idx_groups, function(idx) gridExtra::arrangeGrob(grobs = plots[idx], ncol = 2))

while (!is.null(dev.list())) dev.off()
pdf(OUT_FILE, width = 11, height = 8.5)  # landscape
for (k in seq_along(pages)) {
  if (k > 1) grid::grid.newpage()
  grid::grid.draw(pages[[k]])
}
dev.off()
message("Wrote: ", OUT_FILE)




# Plot species with mixed model fit

# ===== ALL SPECIES → one PDF; one page per species; SINGLE bottom legend (no duplicates) =====
# Requires: fish_pop, species_lookup, dplyr, lubridate, ggplot2, mgcv, patchwork, grid, gtable



# --- plotting constants ---
PANEL_AR <- 0.62
pal12 <- c("#a6cee3","#1f78b4","#b2df8a","#33a02c",
           "#fb9a99","#e31a1c","#fdbf6f","#ff7f00",
           "#cab2d6","#6a3d9a","#ffff99","#b15928")

# --- normalize lookup ONCE; in-memory only ---
species_lu_norm <- species_lookup %>%
  mutate(
    Species    = toupper(trimws(.data$Species)),
    CommonName = trimws(.data$CommonName),
    Scientific = trimws(.data$Scientific)
  ) %>%
  distinct(Species, .keep_all = TRUE)

title_for_species <- function(code) {
  code_norm <- toupper(trimws(code))
  hit <- dplyr::filter(species_lu_norm, .data$Species == code_norm)
  if (nrow(hit) >= 1 && nzchar(hit$CommonName[1]) && nzchar(hit$Scientific[1])) {
    paste0(hit$CommonName[1], " - ", hit$Scientific[1], " (", code_norm, ")")
  } else paste0("Species ", code_norm)
}

# --- helpers (same compute path) ---
make_stream_year_species <- function(df, species_code, var) {
  df %>%
    filter(.data$Species == species_code) %>%
    select(Stream, Date, {{ var }}) %>%
    mutate(Year = lubridate::year(.data$Date)) %>%
    group_by(Stream, Year) %>%
    summarize(Total = sum({{ var }}, na.rm = TRUE), .groups = "drop") %>%
    filter(.data$Total > 0)
}

fit_global_gamm <- function(sy, k = 10) {
  sy <- sy %>% mutate(Year = as.numeric(.data$Year), Stream = as.factor(.data$Stream))
  n_years <- dplyr::n_distinct(sy$Year)
  if (n_years < 3 || nrow(sy) < 3) return(NULL)
  k_eff <- max(3, min(k, n_years - 1))
  new_years <- sort(unique(sy$Year))
  ref_level <- levels(sy$Stream)[1]
  nd <- data.frame(Year = new_years, Stream = factor(ref_level, levels = levels(sy$Stream)))
  fit_try <- try(
    mgcv::gam(log(Total) ~ s(Year, k = k_eff, bs = "cr") + s(Stream, bs = "re"),
              data = sy, method = "REML", select = TRUE),
    silent = TRUE
  )
  if (!inherits(fit_try, "try-error")) {
    excl <- grep("^s\\(Stream\\)", sapply(fit_try$smooth, function(s) s$label), value = TRUE)
    if (length(excl) == 0) excl <- "s(Stream)"
    pred <- predict(fit_try, newdata = nd, type = "link", exclude = excl)
    return(tibble::tibble(Year = new_years, yhat = exp(pred)))
  }
  fit_lin <- try(stats::lm(log(Total) ~ Year, data = sy), silent = TRUE)
  if (!inherits(fit_lin, "try-error")) {
    pred <- predict(fit_lin, newdata = data.frame(Year = new_years), type = "response")
    return(tibble::tibble(Year = new_years, yhat = exp(pred)))
  }
  NULL
}

# --- labels (same as yours) ---
metric_titles <- c(
  TOTpop  = "# Total Abundance", YOYpop  = "# Young-of-Year Abundance",
  ADTpop  = "# Adult Abundance", TOTDens = "# Total Density",
  YOYDens = "# Young-of-Year Density", ADTDens = "# Adult Density",
  TOTBiom = "# Total Biomass", YOYBiom = "# Young-of-Year Biomass",
  ADTBiom = "# Adult Biomass", TOTmnwt = "# Mean Weight (All)",
  YOYmnwt = "# Mean Weight (Young-of-Year)", ADTmnwt = "# Mean Weight (Adult)"
)
ylabs <- c(
  TOTpop="Total abundance (count)", YOYpop="Young-of-Year abundance (count)", ADTpop="Adult abundance (count)",
  TOTDens="Total density", YOYDens="Young-of-Year density", ADTDens="Adult density",
  TOTBiom="Total biomass", YOYBiom="Young-of-Year biomass", ADTBiom="Adult biomass",
  TOTmnwt="Mean weight (all, g)", YOYmnwt="Mean weight (Young-of-Year, g)", ADTmnwt="Mean weight (Adult, g)"
)

# --- panel + legend layout helpers ---
pick_ncol <- function(n) { if (n <= 2) 1 else if (n <= 4) 2 else if (n <= 6) 3 else 4 }
pick_legend_ncol <- function(n_streams) { max(2, min(6, ceiling(n_streams / 3))) }
legend_height_frac <- function(n_streams, ncol) {
  rows <- ceiling(n_streams / ncol)
  min(0.18, 0.08 + 0.03 * (rows - 1))
}

# --- core panel builder (no legends inside; avoids 1-point line warnings) ---
plot_metric_core <- function(df_metric, main_title, ylab, scale_panel) {
  df_line <- df_metric %>% group_by(Stream) %>% filter(n() >= 2) %>% ungroup()
  smooth_df <- fit_global_gamm(df_metric, k = 10)
  ggplot(df_metric, aes(Year, Total, color = Stream, group = Stream)) +
    geom_line(data = df_line, show.legend = FALSE) +
    geom_point(size = 1.5) +                              # << size tweak
    scale_y_log10() +
    scale_panel +                                         # legend suppressed in panels
    labs(x = "Year", y = ylab, title = main_title) +
    theme_bw() +
    theme(aspect.ratio = PANEL_AR) +
    (if (!is.null(smooth_df)) geom_line(
      data = smooth_df, aes(Year, yhat),
      inherit.aes = FALSE, color = "black", linewidth = 1.2, show.legend = FALSE
    ) else NULL)
}

# --- output (lazy-open device to avoid blank first page) ---
out_file <- "~/Downloads/species_metrics_ALL_species.pdf"
while (!is.null(dev.list())) dev.off()

pdf_open <- FALSE  # open device only when first valid page is ready

all_species <- sort(unique(fish_pop$Species))

for (sp in all_species) {
  message("Rendering: ", sp)
  
  # per-metric data (NOTE capitalization: ADTDens)
  metric_dfs <- list(
    TOTpop  = make_stream_year_species(fish_pop, sp, TOTpop),
    YOYpop  = make_stream_year_species(fish_pop, sp, YOYpop),
    ADTpop  = make_stream_year_species(fish_pop, sp, ADTpop),
    TOTDens = make_stream_year_species(fish_pop, sp, TOTDens),
    YOYDens = make_stream_year_species(fish_pop, sp, YOYDens),
    ADTDens = make_stream_year_species(fish_pop, sp, ADTDens),
    TOTBiom = make_stream_year_species(fish_pop, sp, TOTBiom),
    YOYBiom = make_stream_year_species(fish_pop, sp, YOYBiom),
    ADTBiom = make_stream_year_species(fish_pop, sp, ADTBiom),
    TOTmnwt = make_stream_year_species(fish_pop, sp, TOTmnwt),
    YOYmnwt = make_stream_year_species(fish_pop, sp, YOYmnwt),
    ADTmnwt = make_stream_year_species(fish_pop, sp, ADTmnwt)
  )
  keep <- vapply(metric_dfs, function(df) (!is.null(df)) && nrow(df) >= 3 && dplyr::n_distinct(df$Year) >= 3, logical(1))
  metric_dfs <- metric_dfs[keep]
  
  # skip species with no usable metrics
  if (length(metric_dfs) == 0) next
  
  # legend mapping from union of streams across kept panels
  union_streams <- sort(unique(unlist(lapply(metric_dfs, function(df) unique(df$Stream)))))
  if (length(union_streams) == 0) next
  
  pal_union <- setNames(rep_len(pal12, length(union_streams)), union_streams)
  
  # panel scale: same mapping everywhere; NO legend in panels
  scale_panel <- ggplot2::scale_color_manual(
    values = pal_union, breaks = union_streams, limits = union_streams, drop = FALSE, name = "Stream",
    guide = "none"
  )
  
  # build all panels
  metric_titles_sub <- metric_titles[names(metric_dfs)]
  ylabs_sub <- ylabs[names(metric_dfs)]
  plots <- Map(function(name, df)
    plot_metric_core(df, metric_titles_sub[[name]], ylabs_sub[[name]], scale_panel),
    names(metric_dfs), metric_dfs)
  
  # arrange panels
  ncol <- pick_ncol(length(plots))
  panels <- patchwork::wrap_plots(plots, ncol = ncol)
  
  # build ONE legend from a tiny dummy plot with the same scale
  legend_ncol <- pick_legend_ncol(length(union_streams))
  legend_plot <- ggplot(
    data.frame(Stream = factor(union_streams, levels = union_streams), x = 1, y = 1),
    aes(x, y, color = Stream)
  ) +
    geom_point(size = 1) +   # << match panel size in legend
    ggplot2::scale_color_manual(
      values = pal_union, breaks = union_streams, limits = union_streams, drop = FALSE, name = "Stream"
    ) +
    guides(color = guide_legend(ncol = legend_ncol, byrow = TRUE,
                                override.aes = list(linetype = 0, size = 1.5))) +  # << size tweak
    theme_void(base_size = 10) +
    theme(
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.justification = "center",
      legend.text       = element_text(size = 8),
      legend.key.height = grid::unit(3, "mm"),
      legend.key.width  = grid::unit(6, "mm"),
      legend.margin     = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  
  g <- ggplotGrob(legend_plot)
  guide_idx <- which(sapply(g$grobs, function(x) grepl("guide-box", x$name)))
  if (length(guide_idx) < 1) next
  legend_box <- g$grobs[[guide_idx[1]]]
  
  # combine panels + legend on a single page (title via plot_annotation)
  h_legend <- legend_height_frac(length(union_streams), legend_ncol)
  page <- (panels / patchwork::wrap_elements(legend_box)) +
    patchwork::plot_layout(heights = c(1, h_legend)) +
    patchwork::plot_annotation(
      title = title_for_species(sp),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
    )
  
  # --- open PDF only when first printable page is ready ---
  if (!pdf_open) {
    grDevices::pdf(out_file, width = 14, height = 10)
    pdf_open <- TRUE
  }
  
  print(page)  # one PDF page per species
}

if (pdf_open) dev.off()
message("Wrote: ", out_file)



#---------Richness across streams -----

# ===================== SPECIES RICHNESS (log-fit, log-plot, colors, filtering) =====================
# Requires: ggplot2, dplyr, lubridate, mgcv, tidyr, grid, gridExtra (and optionally colorspace)

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(lubridate); library(mgcv)
  library(tidyr); library(grid); library(gridExtra)
})

# ---------- CONFIG ----------
SITE_COL  <- if (exists("SITE_COL")) SITE_COL else "Site"
RICH_OUT_FILE <- "~/Downloads/richness_stream_time.pdf"

# Effort normalization for the black line
EFFORT_MODE <- "typical"     # "typical" | "year_specific" | "fixed"
FIXED_SITES_PER_STREAM <- 5  # only when EFFORT_MODE == "fixed"

# Target across streams for the black line
TARGET <- "stream_gmean"     # "stream_gmean" (default) | "stream_mean" | "network_sum"

# Log transform & plotting eps (handles zeros cleanly)
LOG_EPS_RICH  <- 0.5         # used IN the fit: log(Richness + LOG_EPS_RICH)
PLOT_EPS_RICH <- LOG_EPS_RICH

# Smoothing / model options
K_SMOOTH  <- 10
YEAR_BY   <- 0.1
GAMMA_PEN <- 1.0
N_THREADS <- if (requireNamespace("parallel", quietly = TRUE)) max(1, parallel::detectCores()-1) else 1
PT_SIZE   <- 0.8
DEBUG     <- TRUE


# ---- Smoothing / model options ----

# K_SMOOTH: upper bound on wiggliness for s(Year) in mgcv.
#   • Typical: 7–12. Model chooses edf ≤ K_SMOOTH-1.
#   • If the curve is too flat and summary shows edf ≈ K_SMOOTH-1 → raise K_SMOOTH.
#   • If the curve looks too wiggly → lower K_SMOOTH or increase GAMMA_PEN.
K_SMOOTH  <- 10

# YEAR_BY: spacing of the prediction grid in years (for drawing the black line).
#   • Smaller → smoother-looking line, larger files; does NOT change the fit.
#   • Larger → coarser-looking line. Try 0.1–0.25 for annual data; use 1 for 1 point/year.
YEAR_BY   <- 0.1

# GAMMA_PEN: mgcv's extra smoothing penalty (gamma). Values > 1 favor smoother curves.
#   • 1.0 = standard penalty. 1.1–1.3 = safer/less wiggly. (<1 rarely useful.)
#   • First try bumping to 1.1–1.2 before changing K_SMOOTH if the fit is too wiggly.
GAMMA_PEN <- 1.0

# N_THREADS: threads used by bam(..., discrete = TRUE).
#   • Default: all cores minus one (fast). Set to 1 for strict reproducibility.
N_THREADS <- if (requireNamespace("parallel", quietly = TRUE))
  max(1, parallel::detectCores() - 1) else 1

# PT_SIZE: point size for the per-stream scatter.
#   • 0.6–1.2 is a reasonable range; smaller reduces clutter with many points.
PT_SIZE   <- 0.8

# DEBUG: print status messages during fit/predict (family used, year range, etc.).
#   • TRUE helps diagnose missing lines; set FALSE for quiet plotting.
DEBUG     <- TRUE

# Quick tuning cheatsheet:
#   Too wiggly  → increase GAMMA_PEN (e.g., 1.2) or lower K_SMOOTH.
#   Too flat    → raise K_SMOOTH a bit (e.g., 12).
#   Slow/large  → increase YEAR_BY (e.g., 0.25–0.5).
#   Non-deterministic runtimes → set N_THREADS <- 1.

# ---------- NEW: FILTERS ----------
MIN_YEARS_PER_STREAM <- 5          # keep streams with >= this many years of data
DROP_LOW_SAMPLED_YEARS <- FALSE    # set TRUE to drop stream-years with too few sites
MIN_SITES_PER_STREAMYEAR <- 5      # only applies if DROP_LOW_SAMPLED_YEARS = TRUE

# ---------- PALETTE ----------
.make_stream_palette <- function(levels_vec) {
  n <- length(levels_vec)
  if (requireNamespace("colorspace", quietly = TRUE)) {
    cols <- colorspace::qualitative_hcl(n, palette = "Dark 3")
  } else {
    cols <- grDevices::hcl(h = seq(15, 375, length.out = n + 1)[-1], c = 80, l = 60)
  }
  stats::setNames(cols, levels_vec)
}

# ---------- 1) Build Stream×Year Richness (+ effort) ----------
# presence_var: which column defines presence (>0 means species observed). e.g., TOTpop
make_richness_stream_year <- function(df, presence_var, site_col = SITE_COL) {
  site_sym <- rlang::sym(site_col)
  
  df_base <- df %>%
    dplyr::select(Stream, !!site_sym, Date, Species, {{ presence_var }}) %>%
    dplyr::mutate(
      Year    = lubridate::year(.data$Date),
      present = {{ presence_var }} > 0
    )
  
  # Effort = # distinct sites sampled in each Stream-Year
  effort_sy <- df_base %>%
    dplyr::distinct(Stream, !!site_sym, Year) %>%
    dplyr::count(Stream, Year, name = "n_sites")
  
  # Richness = # unique species present in Stream-Year (union across sites)
  rich_sy <- df_base %>%
    dplyr::group_by(Stream, Year, Species) %>%
    dplyr::summarise(present = any(present, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::summarise(Richness = sum(present, na.rm = TRUE), .groups = "drop")
  
  dplyr::left_join(rich_sy, effort_sy, by = c("Stream","Year")) %>%
    dplyr::mutate(
      Stream    = factor(Stream),
      Year      = as.numeric(Year),
      n_sites   = pmax(n_sites, 1L),
      log_sites = log(pmax(n_sites, 1))
    ) %>% droplevels()
}

# ---------- 2) Fit & Predict (Gaussian on log(Richness + eps)) ----------
fit_richness_log_bam <- function(sy_rich,
                                 k = K_SMOOTH, year_by = YEAR_BY,
                                 gamma_pen = GAMMA_PEN, nthreads = N_THREADS,
                                 target = TARGET) {
  if (nrow(sy_rich) < 3 || dplyr::n_distinct(sy_rich$Year) < 3) {
    if (DEBUG) message("Richness: insufficient data to fit."); return(NULL)
  }
  k_eff <- max(3, min(k, dplyr::n_distinct(sy_rich$Year) - 1))
  
  sy_fit <- sy_rich %>% dplyr::mutate(Rich_log = log(Richness + LOG_EPS_RICH))
  
  fit_try <- try(
    mgcv::bam(
      Rich_log ~ s(Year, k = k_eff, bs = "cr") +
        s(Stream, bs = "re") +                 # stream intercepts
        s(log_sites, k = 5, bs = "cr"),        # effort effect
      family   = gaussian(),                   # GAUSSIAN on log scale
      data     = sy_fit,
      method   = "fREML",
      discrete = TRUE,
      nthreads = nthreads,
      gamma    = gamma_pen
    ),
    silent = TRUE
  )
  if (inherits(fit_try, "try-error")) { if (DEBUG) message("Richness log-bam() failed."); return(NULL) }
  if (DEBUG) message("bam() fit OK (richness, log-Gaussian).")
  
  yrs <- seq(min(sy_fit$Year), max(sy_fit$Year), by = year_by)
  
  # ---- Effort level for prediction ----
  if (EFFORT_MODE == "year_specific") {
    n_eff_tbl <- sy_fit %>% dplyr::select(Stream, Year, n_sites)
  } else if (EFFORT_MODE == "fixed") {
    n_eff_tbl <- tidyr::expand_grid(Stream = levels(sy_fit$Stream), Year = yrs) %>%
      dplyr::mutate(n_sites = FIXED_SITES_PER_STREAM)
  } else { # "typical"
    med_sites <- sy_fit %>% dplyr::group_by(Stream) %>%
      dplyr::summarise(n_sites = stats::median(n_sites), .groups = "drop")
    n_eff_tbl <- tidyr::expand_grid(Stream = levels(sy_fit$Stream), Year = yrs) %>%
      dplyr::left_join(med_sites, by = "Stream")
  }
  
  nd <- n_eff_tbl %>%
    dplyr::mutate(
      Stream    = factor(Stream, levels = levels(sy_fit$Stream)),
      Year      = as.numeric(Year),
      n_sites   = pmax(n_sites, 1),
      log_sites = log(n_sites)
    )
  
  # Predict per stream on LOG scale
  mu_log <- try(predict(fit_try, newdata = nd, type = "response"), silent = TRUE)
  if (inherits(mu_log, "try-error")) { if (DEBUG) message("Richness log predict failed."); return(NULL) }
  nd$mu_log_stream <- as.numeric(mu_log)
  
  # ---- Aggregate across streams ----
  out <- switch(
    tolower(target),
    "stream_mean" = {
      nd %>% dplyr::mutate(mu_stream = pmax(exp(mu_log_stream) - LOG_EPS_RICH, PLOT_EPS_RICH)) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(yhat = mean(mu_stream, na.rm = TRUE), .groups = "drop")
    },
    "network_sum" = {
      nd %>% dplyr::mutate(mu_stream = pmax(exp(mu_log_stream) - LOG_EPS_RICH, PLOT_EPS_RICH)) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(yhat = sum(mu_stream, na.rm = TRUE), .groups = "drop")
    },
    { # default: geometric mean across streams
      nd %>% dplyr::group_by(Year) %>%
        dplyr::summarise(yhat = pmax(exp(mean(mu_log_stream, na.rm = TRUE)) - LOG_EPS_RICH,
                                     PLOT_EPS_RICH),
                         .groups = "drop")
    }
  )
  
  if (DEBUG) message("Richness Pred rows: ", nrow(out), "; Year range: ",
                     sprintf("%.1f–%.1f", min(out$Year), max(out$Year)))
  out
}

# ---------- 3) Plot builder (adds filtering) ----------
plot_richness <- function(df, presence_var,
                          ylab = "Species richness (# unique species)",
                          title_txt = "# Species Richness") {
  sy_rich  <- make_richness_stream_year(df, {{ presence_var }}, SITE_COL)
  if (nrow(sy_rich) == 0) {
    return(ggplot() + theme_void() + ggtitle(paste0(title_txt, " — (no data)")))
  }
  
  # ---- OPTIONAL: drop poorly sampled stream-years (< MIN_SITES_PER_STREAMYEAR) ----
  if (DROP_LOW_SAMPLED_YEARS) {
    before_n <- nrow(sy_rich)
    sy_rich <- sy_rich %>% dplyr::filter(n_sites >= MIN_SITES_PER_STREAMYEAR)
    if (DEBUG) message("Dropped ", before_n - nrow(sy_rich),
                       " stream-years with n_sites < ", MIN_SITES_PER_STREAMYEAR, ".")
  }
  
  # ---- Filter to streams with at least MIN_YEARS_PER_STREAM years ----
  keep_streams <- sy_rich %>%
    dplyr::group_by(Stream) %>%
    dplyr::summarise(n_years = dplyr::n_distinct(Year), .groups = "drop") %>%
    dplyr::filter(n_years >= MIN_YEARS_PER_STREAM)
  sy_rich <- sy_rich %>%
    dplyr::semi_join(keep_streams, by = "Stream") %>%
    droplevels()
  
  if (nrow(sy_rich) == 0) {
    return(ggplot() + theme_void() +
             ggtitle(paste0(title_txt, " — (no streams with ≥ ", MIN_YEARS_PER_STREAM, " years)")))
  }
  
  # for log plotting, add a tiny pseudocount to zeros
  sy_rich <- sy_rich %>% mutate(Richness_plot = pmax(Richness, PLOT_EPS_RICH))
  
  smooth_df <- fit_richness_log_bam(sy_rich)
  
  sy_line <- sy_rich %>%
    group_by(Stream) %>% filter(n() >= 2) %>% ungroup()
  
  # palette mapped to filtered stream levels
  stream_levels <- levels(sy_rich$Stream)
  pal_streams   <- .make_stream_palette(stream_levels)
  
  n_streams   <- length(stream_levels)
  legend_ncol <- max(2, min(6, ceiling(n_streams / 3)))
  
  ggplot(sy_rich, aes(Year, Richness_plot, color = Stream, group = Stream)) +
    geom_line(data = sy_line, linewidth = 0.6, alpha = 0.9) +
    geom_point(size = PT_SIZE, alpha = 0.9) +
    scale_y_log10() +
    scale_color_manual(
      values = pal_streams, breaks = stream_levels, limits = stream_levels, drop = FALSE, name = "Stream"
    ) +
    { if (!is.null(smooth_df) && nrow(smooth_df) > 1)
      geom_line(
        data = smooth_df, aes(Year, yhat),
        inherit.aes = FALSE, color = "black", linewidth = 1.2
      ) else NULL } +
    labs(x = "Year", y = ylab, title = title_txt) +
    theme_bw() +
    scale_color_viridis_d() +
    guides(color = guide_legend(
      ncol = legend_ncol, byrow = TRUE, override.aes = list(size = 2, alpha = 1)
    )) +
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.title     = element_text(size = 9),
      legend.text      = element_text(size = 8)
    )
}

# ---------- 4) MAKE & SAVE THE PLOT ----------
# Presence defined by TOTpop > 0; change column if you prefer a different presence definition
rich_plot <- plot_richness(fish_pop, TOTpop)
print(rich_plot)

# Write a one-page PDF
while (!is.null(grDevices::dev.list())) grDevices::dev.off()
grDevices::pdf(RICH_OUT_FILE, width = 14, height = 10)  # bigger to fit the legend
grid::grid.newpage(); grid::grid.draw(gridExtra::arrangeGrob(rich_plot))
grDevices::dev.off()
message("Wrote richness plot to: ", RICH_OUT_FILE)


