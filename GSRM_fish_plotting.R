# GRSM species code → common/scientific (from NPS Fish Checklist)
species_lookup <- tibble::tibble(
  code = c(
    "BEC","BRH","GFD","GID","NHS","RIC","RLD","ROB","SMB","STR","TES","TNS","TSD","WPS","WTS","BAD",
    "RBT","WHS","BND","WND","CKC","SMT","SPC","CIT","BNT","RSD","SAS","FHM","LND","FTD","TND","BKT",
    "MOS","BAS","RRH","GSD","MIS","SVS","GRS","MBL","SPB","TUC","OLD","TAD","BLG","ABL","SWD","SHR",
    "GRH"
  ),
  common = c(
    "Bigeye Chub",              # BEC
    "Black Redhorse",           # BRH
    "Greenfin Darter",          # GFD
    "Gilt Darter",              # GID
    "Northern Hog Sucker",      # NHS
    "River Chub",               # RIC
    "Redline Darter",           # RLD
    "Rock Bass",                # ROB
    "Smallmouth Bass",          # SMB
    "Striped Shiner",           # STR
    "Telescope Shiner",         # TES
    "Tennessee Shiner",         # TNS
    NA,                           # TSD = ??? (not on NPS list; please confirm)
    "Warpaint Shiner",          # WPS
    "Whitetail Shiner",         # WTS
    "Banded Darter",            # BAD
    "Rainbow Trout",            # RBT
    "White Sucker",             # WHS
    "Blacknose Dace",           # BND = ??? (ambiguous; not clear on list)
    "Wounded Darter",           # WND
    "Creek Chub",               # CKC
    "Smoky Madtom",             # SMT
    NA,                         # SPC = ??? (ambiguous; not obvious on list)
    "Citico Darter",            # CIT
    "Brown Trout",              # BNT
    "Rosyside Dace",            # RSD
    "Saffron Shiner",           # SAS
    "Fathead Minnow",           # FHM
    "Longnose Dace",            # LND
    "Fantail Darter",           # FTD
    "Tennessee Dace",           # TND
    "Brook Trout",              # BKT
    "Mosquitofish",             # MOS
    "Banded Sculpin",           # BAS
    "River Redhorse",           # RRH
    "Gizzard Shad",             # GSD
    "Mirror Shiner",            # MIS
    "Silver Shiner",            # SVS
    "Greenside Darter",         # GRS
    "Mountain Brook Lamprey",   # MBL
    "Spotted Bass",             # SPB
    "Tuckasegee Darter",        # TUC
    "Olive Darter",             # OLD
    "Tangerine Darter",         # TAD
    "Bluegill",                 # BLG
    "American Brook Lamprey",   # ABL
    "Swannanoa Darter",         # SWD
    "Shorthead Redhorse",       # SHR
    "Golden Redhorse"           # GRH
  ),
  scientific = c(
    "Hybopsis amblops",                 # Bigeye Chub
    "Moxostoma duquesnei",              # Black Redhorse
    "Etheostoma chlorobranchium",       # Greenfin Darter
    "Percina evides",                   # Gilt Darter
    "Hypentelium nigricans",            # Northern Hog Sucker
    "Nocomis micropogon",               # River Chub
    "Etheostoma rufilineatum",          # Redline Darter
    "Ambloplites rupestris",            # Rock Bass
    "Micropterus dolomieu",             # Smallmouth Bass
    "Luxilus chrysocephalus",           # Striped Shiner
    "Notropis telescopus",              # Telescope Shiner
    "Notropis leuciodus",               # Tennessee Shiner
    NA,                                # TSD = ??? (please confirm)
    "Luxilus coccogenis",               # Warpaint Shiner
    "Cyprinella galactura",             # Whitetail Shiner
    "Etheostoma zonale",                # Banded Darter
    "Oncorhynchus mykiss",              # Rainbow Trout
    "Catostomus commersonii",           # White Sucker
    "Rhinichthys atratulus",            # BND = ??? (please confirm)
    "Etheostoma vulneratum",            # Wounded Darter
    "Semotilus atromaculatus",          # Creek Chub
    "Noturus baileyi",                  # Smoky Madtom
    NA,                                 # SPC = ??? (please confirm)
    "Etheostoma sitikuense",            # Citico Darter
    "Salmo trutta",                     # Brown Trout
    "Clinostomus funduloides",          # Rosyside Dace
    "Notropis rubricroceus",            # Saffron Shiner
    "Pimephales promelas",              # Fathead Minnow
    "Rhinichthys cataractae",           # Longnose Dace
    "Etheostoma flabellare",            # Fantail Darter
    "Chrosomus tennesseensis",          # Tennessee Dace
    "Salvelinus fontinalis",            # Brook Trout
    "Gambusia affinis",                 # Western Mosquitofish
    "Cottus carolinae",                 # Banded Sculpin
    "Moxostoma carinatum",              # River Redhorse
    "Dorosoma cepedianum",              # Gizzard Shad
    "Notropis spectrunculus",           # Mirror Shiner
    "Notropis photogenis",              # Silver Shiner
    "Etheostoma blennioides",           # Greenside Darter
    "Ichthyomyzon greeleyi",            # Mountain Brook Lamprey
    "Micropterus punctulatus",          # Spotted Bass
    "Etheostoma gutselli",              # Tuckasegee Darter
    "Percina squamata",                 # Olive Darter
    "Percina aurantiaca",               # Tangerine Darter
    "Lepomis macrochirus",              # Bluegill
    "Lethenteron appendix",             # American Brook Lamprey
    "Etheostoma swannanoa",             # Swannanoa Darter
    "Moxostoma macrolepidotum",         # Shorthead Redhorse
    "Moxostoma erythrurum"              # Golden Redhorse
  )
)
# Make case insensitive
resolve_species_terms <- function(terms) {
  terms_lc <- tolower(trimws(terms))
  
  # Direct code match
  direct_code <- toupper(terms_lc[terms_lc %in% tolower(species_lookup$code)])
  
  # Match against common names
  common_match <- species_lookup$code[
    match(terms_lc, tolower(species_lookup$common))
  ]
  
  # Match against scientific names
  sci_match <- species_lookup$code[
    match(terms_lc, tolower(species_lookup$scientific))
  ]
  
  # Collapse
  out <- unique(na.omit(c(direct_code, common_match, sci_match)))
  
  if (length(out) == 0) {
    warning("No species matched: ", paste(terms, collapse = ", "))
  }
  out
}

species_labeler <- function(codes) {
  meta <- dplyr::filter(species_lookup, code %in% codes)
  setNames(paste0(meta$common, " (", meta$code, ")"), meta$code)
}


# ====================== COMPLETE DROP-IN SCRIPT ======================
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(mgcv)
  library(parallel)
  library(scales)
  library(RColorBrewer)
})

# ---------- Custom theme ----------
theme_plot <- function(legend = TRUE) {
  ggplot2::theme(
    panel.grid        = element_blank(),
    aspect.ratio      = .75,
    axis.text         = element_text(size = 18, color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.title        = element_text(size = 18),
    axis.title.y      = element_text(margin = margin(r = 10)),
    axis.title.x      = element_text(margin = margin(t = 10)),
    axis.title.x.top  = element_text(margin = margin(b = 5)),
    plot.title        = element_text(size = 18, face = "plain", hjust = 0.5),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.background  = element_blank(),
    strip.background  = element_blank(),
    legend.text       = element_text(size = 15),
    legend.title      = element_text(size = 18),
    legend.position   = if (legend) "right" else "none",
    text              = element_text(family = "Helvetica")
  )
}

# ---------- Plain powers-of-ten labels (0.001, 0.01, 0.1, 1, 10, 100, 1,000, …) ----------
label_powers_plain <- function(x) {
  out <- prettyNum(x, big.mark = ",", scientific = FALSE, drop0trailing = TRUE)
  sub("\\.$", "", out)
}

# ---------- Aggregate to stream × year totals ----------
make_stream_year_totals <- function(df, var) {
  df %>%
    dplyr::select(Stream, Date, {{ var }}) %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::group_by(Stream, Year) %>%
    dplyr::summarise(Total = sum({{ var }}, na.rm = TRUE), .groups = "drop")
}

# ---------- Species-aware totals (Stream × Year × Species) ----------
make_stream_year_totals_by_species <- function(df, var) {
  df %>%
    dplyr::select(Species, Stream, Date, {{ var }}) %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::group_by(Species, Stream, Year) %>%
    dplyr::summarise(Total = sum({{ var }}, na.rm = TRUE), .groups = "drop")
}

# ---------- Fit function: Gaussian on log(y) (drops zeros) OR NegBin (keeps zeros) ----------
fit_stream_trend <- function(sy_stream,
                             k = 10, year_by = 0.1, gamma_pen = 1.0,
                             nthreads = max(1, parallel::detectCores() - 1),
                             fit_type = c("gaussian","nb"),
                             target = c("avg_stream_sum","network_sum")) {
  fit_type <- match.arg(fit_type)
  target   <- match.arg(target)
  
  dat <- sy_stream %>%
    dplyr::mutate(Stream = factor(Stream),
                  Year   = as.numeric(Year))
  
  if (fit_type == "gaussian") {
    dat <- dplyr::filter(dat, Total > 0 & is.finite(Total)) %>% droplevels()
    if (nrow(dat) < 3 || dplyr::n_distinct(dat$Year) < 3) return(NULL)
    k_eff <- max(3, min(k, dplyr::n_distinct(dat$Year) - 1))
    
    fit <- mgcv::bam(
      log(Total) ~ s(Year, k = k_eff, bs = "cr") + s(Stream, bs = "re"),
      family   = gaussian(),
      data     = dat,
      method   = "fREML",
      discrete = TRUE,
      nthreads = nthreads,
      gamma    = gamma_pen
    )
    
    yrs <- seq(min(dat$Year), max(dat$Year), by = year_by)
    nd  <- tidyr::expand_grid(Year = yrs, Stream = levels(dat$Stream))
    eta <- mgcv::predict.bam(fit, newdata = nd, type = "link")
    nd$yhat_stream <- exp(eta)
    
  } else { # NegBin
    if (nrow(dat) < 3 || dplyr::n_distinct(dat$Year) < 3) return(NULL)
    k_eff <- max(3, min(k, dplyr::n_distinct(dat$Year) - 1))
    
    fit <- mgcv::bam(
      Total ~ s(Year, k = k_eff, bs = "cr") + s(Stream, bs = "re"),
      family   = mgcv::nb(),
      data     = dat,
      method   = "fREML",
      discrete = TRUE,
      nthreads = nthreads,
      gamma    = gamma_pen
    )
    
    yrs <- seq(min(dat$Year), max(dat$Year), by = year_by)
    nd  <- tidyr::expand_grid(Year = yrs, Stream = levels(dat$Stream))
    eta <- mgcv::predict.bam(fit, newdata = nd, type = "link")
    nd$yhat_stream <- fit$family$linkinv(eta)
  }
  
  out <- if (identical(target, "network_sum")) {
    nd %>% dplyr::group_by(Year) %>%
      dplyr::summarise(yhat = sum(yhat_stream), .groups = "drop")
  } else { # avg_stream_sum (geometric mean across streams)
    nd %>% dplyr::group_by(Year) %>%
      dplyr::summarise(yhat = exp(mean(log(yhat_stream), na.rm = TRUE)), .groups = "drop")
  }
  
  list(smooth = out, per_stream = nd, fit = fit, fit_type = fit_type)
}

# ---------- Build species→stream color families (Greens=BKT, Blues=RBT, Reds=BNT) ----------
.build_species_stream_palette <- function(sy_plot, species_levels = NULL) {
  if (is.null(species_levels)) species_levels <- sort(unique(sy_plot$Species))
  species_palettes <- c(BKT = "Greens", RBT = "Blues", BNT = "Reds")
  ramp_from_brewer <- function(n, palette) {
    n_base <- min(9, max(3, n))
    base <- RColorBrewer::brewer.pal(n_base, palette)
    grDevices::colorRampPalette(base)(n)
  }
  
  cols_named <- c()
  for (sp in species_levels) {
    sub <- sy_plot %>% dplyr::filter(Species == sp)
    if (!nrow(sub)) next
    streams <- sort(unique(sub$Stream))
    n <- length(streams)
    pal_name <- species_palettes[[sp]]
    if (is.null(pal_name)) pal_name <- "Greys"
    shades <- ramp_from_brewer(n, pal_name)
    names(shades) <- paste(sp, streams, sep=":")  # Species:Stream key
    cols_named <- c(cols_named, shades)
    
    # species smooth color (a darker shade)
    smooth_col <- shades[max(1, min(n, ceiling(0.75 * n)))]
    names(smooth_col) <- paste(sp, "__smooth", sep=":")
    cols_named <- c(cols_named, smooth_col)
  }
  cols_named
}

# ---------- Plot wrapper ----------
# species_mode = "pooled"  -> pool all selected species (single black fit line)
# species_mode = "separate"-> fit/overlay one smooth per species; shades per stream
plot_stream_totals_with_fit <- function(df, var,
                                        ylab, title_txt,
                                        fit_type = c("gaussian","nb"),
                                        species = NULL,
                                        streams = NULL,
                                        species_mode = c("pooled","separate"),
                                        show_legend = FALSE,
                                        log10_labels = TRUE,
                                        k = 10, year_by = 0.1,
                                        gamma_pen = 1.0,
                                        nthreads = max(1, parallel::detectCores() - 1),
                                        target = c("avg_stream_sum","network_sum")) {
  fit_type     <- match.arg(fit_type)
  species_mode <- match.arg(species_mode)
  target       <- match.arg(target)
  
  # --- normalize species input (codes/common/scientific) -> codes
  if (!is.null(species)) {
    species <- resolve_species_terms(species)
    if (!length(species)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() +
               ggplot2::ggtitle("No species matched your input"))
    }
  }
  
  # --- filter
  if (!is.null(species)) df <- dplyr::filter(df, Species %in% species)
  if (!is.null(streams)) df <- dplyr::filter(df, Stream  %in% streams)
  
  if (species_mode == "pooled") {
    # ---------- pooled (one fit over the selected species) ----------
    sy <- make_stream_year_totals(df, {{ var }})
    
    sm <- fit_stream_trend(sy, k = k, year_by = year_by, gamma_pen = gamma_pen,
                           nthreads = nthreads, fit_type = fit_type, target = target)
    if (is.null(sm)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() +
               ggplot2::ggtitle(paste0(title_txt, " — (insufficient data)")))
    }
    
    sy_plot <- if (fit_type == "gaussian") dplyr::filter(sy, Total > 0) else sy
    
    g <- ggplot2::ggplot(sy_plot, ggplot2::aes(Year, Total, color = Stream, group = Stream)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 0.8) +
      ggplot2::geom_line(data = sm$smooth, ggplot2::aes(Year, yhat),
                         inherit.aes = FALSE, linewidth = 1.2, color = "black") +
      ggplot2::labs(x = "Year", y = ylab, title = title_txt) +
      theme_plot(legend = show_legend) +
      ggplot2::scale_color_viridis_d()
    
    g <- g + ggplot2::scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = if (log10_labels) label_powers_plain else NULL
    )
    return(g)
  }
  
  # ---------- separate (one fit per species; stream shades; colored corner labels) ----------
  sy_sp <- make_stream_year_totals_by_species(df, {{ var }})
  if (!nrow(sy_sp)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::ggtitle(paste0(title_txt, " — (no data)")))
  }
  
  # fit per species
  smooth_list <- list()
  for (sp in sort(unique(sy_sp$Species))) {
    sy_one <- sy_sp %>% dplyr::filter(Species == sp) %>% dplyr::select(Stream, Year, Total)
    sm1 <- fit_stream_trend(sy_one, k = k, year_by = year_by, gamma_pen = gamma_pen,
                            nthreads = nthreads, fit_type = fit_type, target = target)
    if (!is.null(sm1)) { sm1$smooth$Species <- sp; smooth_list[[sp]] <- sm1$smooth }
  }
  smooths <- dplyr::bind_rows(smooth_list)
  if (!nrow(smooths)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::ggtitle(paste0(title_txt, " — (insufficient data after fit)")))
  }
  
  # data for plotting (drop zeros if gaussian because log scale)
  sy_plot <- if (fit_type == "gaussian") dplyr::filter(sy_sp, Total > 0) else sy_sp
  sy_plot <- sy_plot %>% dplyr::mutate(col_key = paste(Species, Stream, sep=":"))
  smooths <- smooths  %>% dplyr::mutate(col_key = paste(Species, "__smooth", sep=":"))
  
  cols_named <- .build_species_stream_palette(sy_plot, species_levels = sort(unique(sy_plot$Species)))
  
  g <- ggplot2::ggplot() +
    ggplot2::geom_line(data = sy_plot,
                       ggplot2::aes(Year, Total, color = col_key,
                                    group = interaction(Species, Stream))) +
    ggplot2::geom_point(data = sy_plot,
                        ggplot2::aes(Year, Total, color = col_key), size = 0.8) +
    ggplot2::geom_line(data = smooths,
                       ggplot2::aes(Year, yhat, color = col_key), linewidth = 1.2) +
    ggplot2::scale_color_manual(values = cols_named, guide = if (show_legend) "legend" else "none") +
    ggplot2::labs(x = "Year", y = ylab, title = title_txt) +
    theme_plot(legend = show_legend)
  
  # corner labels (Common (CODE)), evenly spaced down from top-right
  y_max <- max(c(sy_plot$Total, smooths$yhat), na.rm = TRUE)
  sp_levels <- sort(unique(smooths$Species))
  n_lab <- length(sp_levels)
  # spread labels across top 30% of y-range; increase 0.7 -> 0.4 to spread more
  y_positions <- seq(from = y_max, to = y_max * 0.1, length.out = n_lab)
  
  labels_df <- tibble::tibble(
    Species = sp_levels,
    x = Inf,
    y = y_positions,
    col_key = paste(sp_levels, "__smooth", sep=":"),
    label = unname(species_labeler(sp_levels))  # "Common (CODE)"
  )
  
  g <- g + ggplot2::geom_text(
    data = labels_df,
    ggplot2::aes(x = x, y = y, label = label, color = col_key),
    hjust = 1.2, vjust = 1.05, size = 5,
    inherit.aes = FALSE
  )
  
  g <- g + ggplot2::scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = if (log10_labels) label_powers_plain else NULL
  )
  
  g
}



# ---------- helper for dyadic fractions ----------
.label_frac_dyadic <- function(x, tol = 1e-9) {
  if (!is.finite(x) || x <= 0) return(NA_character_)
  if (x >= 1) {
    return(prettyNum(x, big.mark = ",", scientific = FALSE, drop0trailing = TRUE))
  }
  k <- round(log2(1/x))
  if (abs(x - 2^(-k)) <= tol * 2^(-k)) {
    return(paste0("1/", 2^k))   # always plain fraction text
  }
  prettyNum(x, big.mark = ",", scientific = FALSE, drop0trailing = TRUE)
}

label_log_auto <- function(xs) vapply(xs, .label_frac_dyadic, character(1))
scale_y_log_auto <- function(yvals) {
  yvals <- yvals[is.finite(yvals) & yvals > 0]
  if (!length(yvals)) return(ggplot2::scale_y_continuous(labels = label_log_auto))
  spread <- max(yvals, na.rm = TRUE) / max(min(yvals[yvals > 0], na.rm = TRUE), .Machine$double.xmin)
  if (is.finite(spread) && spread >= 1e3) {
    ggplot2::scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = label_log_auto
    )
  } else {
    ggplot2::scale_y_continuous(
      trans  = scales::log_trans(base = 2),
      breaks = scales::trans_breaks("log2", function(x) 2^x),
      labels = label_log_auto
    )
  }
}

# ---------- replacement plotting function ----------
plot_stream_totals_with_fit <- function(df, var,
                                        ylab, title_txt,
                                        fit_type = c("gaussian","nb"),
                                        species = NULL,
                                        streams = NULL,
                                        species_mode = c("pooled","separate"),
                                        show_legend = FALSE,
                                        k = 10, year_by = 0.1,
                                        gamma_pen = 1.0,
                                        nthreads = max(1, parallel::detectCores() - 1),
                                        target = c("avg_stream_sum","network_sum")) {
  fit_type     <- match.arg(fit_type)
  species_mode <- match.arg(species_mode)
  target       <- match.arg(target)
  
  # --- normalize species input (codes/common/scientific) -> codes
  if (!is.null(species)) {
    species <- resolve_species_terms(species)
    if (!length(species)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() +
               ggplot2::ggtitle("No species matched your input"))
    }
  }
  
  # --- filter
  if (!is.null(species)) df <- dplyr::filter(df, Species %in% species)
  if (!is.null(streams)) df <- dplyr::filter(df, Stream  %in% streams)
  
  if (species_mode == "pooled") {
    # ---------- pooled (one fit over the selected species) ----------
    sy <- make_stream_year_totals(df, {{ var }})
    sm <- fit_stream_trend(sy, k = k, year_by = year_by, gamma_pen = gamma_pen,
                           nthreads = nthreads, fit_type = fit_type, target = target)
    if (is.null(sm)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() +
               ggplot2::ggtitle(paste0(title_txt, " — (insufficient data)")))
    }
    sy_plot <- if (fit_type == "gaussian") dplyr::filter(sy, Total > 0) else sy
    g <- ggplot2::ggplot(sy_plot, ggplot2::aes(Year, Total, color = Stream, group = Stream)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 0.8) +
      ggplot2::geom_line(data = sm$smooth, ggplot2::aes(Year, yhat),
                         inherit.aes = FALSE, linewidth = 1.2, color = "black") +
      ggplot2::labs(x = "Year", y = ylab, title = title_txt) +
      theme_plot(legend = show_legend) +
      ggplot2::scale_color_viridis_d()
    g <- g + scale_y_log_auto(c(sy_plot$Total, sm$smooth$yhat))
    return(g)
  }
  
  # ---------- separate (one fit per species; stream shades; corner labels) ----------
  sy_sp <- make_stream_year_totals_by_species(df, {{ var }})
  if (!nrow(sy_sp)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::ggtitle(paste0(title_txt, " — (no data)")))
  }
  smooth_list <- list()
  for (sp in sort(unique(sy_sp$Species))) {
    sy_one <- sy_sp %>% dplyr::filter(Species == sp) %>% dplyr::select(Stream, Year, Total)
    sm1 <- fit_stream_trend(sy_one, k = k, year_by = year_by, gamma_pen = gamma_pen,
                            nthreads = nthreads, fit_type = fit_type, target = target)
    if (!is.null(sm1)) { sm1$smooth$Species <- sp; smooth_list[[sp]] <- sm1$smooth }
  }
  smooths <- dplyr::bind_rows(smooth_list)
  if (!nrow(smooths)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::ggtitle(paste0(title_txt, " — (insufficient data after fit)")))
  }
  sy_plot <- if (fit_type == "gaussian") dplyr::filter(sy_sp, Total > 0) else sy_sp
  sy_plot <- sy_plot %>% dplyr::mutate(col_key = paste(Species, Stream, sep=":"))
  smooths <- smooths %>% dplyr::mutate(col_key = paste(Species, "__smooth", sep=":"))
  cols_named <- .build_species_stream_palette(sy_plot, species_levels = sort(unique(sy_plot$Species)))
  g <- ggplot2::ggplot() +
    ggplot2::geom_line(data = sy_plot,
                       ggplot2::aes(Year, Total, color = col_key,
                                    group = interaction(Species, Stream))) +
    ggplot2::geom_point(data = sy_plot,
                        ggplot2::aes(Year, Total, color = col_key), size = 0.8) +
    ggplot2::geom_line(data = smooths,
                       ggplot2::aes(Year, yhat, color = col_key), linewidth = 1.2) +
    ggplot2::scale_color_manual(values = cols_named, guide = if (show_legend) "legend" else "none") +
    ggplot2::labs(x = "Year", y = ylab, title = title_txt) +
    theme_plot(legend = show_legend)
  
  # ----- species corner labels: upper-right band, log-spaced, plain font -----
  y_all <- c(sy_plot$Total, smooths$yhat)
  y_all <- y_all[is.finite(y_all) & y_all > 0]
  if (!length(y_all)) y_all <- 1
  
  y_max <- max(y_all); y_min <- min(y_all)
  sp_levels <- sort(unique(smooths$Species))
  n_lab <- length(sp_levels)
  
  # top-band height (fraction of log-range): tweak 0.25 as needed
  band_prop <- 0.1
  log_y_max <- log(y_max)
  log_y_min <- log(y_min)
  log_lower <- log_y_max - band_prop * (log_y_max - log_y_min)
  
  y_positions <- if (n_lab <= 1) {
    y_max * 0.92
  } else {
    exp(seq(log_y_max, log_lower, length.out = n_lab))
  }
  
  labels_df <- tibble::tibble(
    Species = sp_levels,
    x = Inf,
    y = y_positions,
    col_key = paste(sp_levels, "__smooth", sep=":"),
    label = unname(species_labeler(sp_levels))
  )
  
  g <- g +
    ggplot2::geom_text(
      data = labels_df,
      ggplot2::aes(x = x, y = y, label = label, color = col_key),
      hjust = 1.1, vjust = 1.05, size = 5, fontface = "plain",
      inherit.aes = FALSE
    )
  
  g <- g + scale_y_log_auto(c(sy_plot$Total, smooths$yhat))
  return(g)
}

# ====================== EXAMPLES ======================
# 1) Pooled (e.g., trout only, but combined), Gaussian (drop zeros), pretty log labels
# p_pooled <- plot_stream_totals_with_fit(
#   three_pass, ADTDens,
#   ylab = "Adult density",
#   title_txt = "# Adult Density — Trout pooled",
#   species = c("BKT","RBT","BNT"),
#   species_mode = "pooled",
#   fit_type = "gaussian",
#   show_legend = FALSE,
#   log10_labels = TRUE
# )
# print(p_pooled)


# 3) Single species, selected streams
# p_one <- plot_stream_totals_with_fit(
#   three_pass, ADTDens,
#   ylab = "Adult density",
#   title_txt = "# Brook Trout — Abrams & West Prong",
#   species = "BKT",
#   streams = c("Abrams","West Prong"),
#   species_mode = "pooled",
#   fit_type = "gaussian",
#   show_legend = TRUE
# )
# print(p_one)

# ---- Export: one PDF per species (single page each) ----
export_species_pdfs <- function(
    df,
    var,                                 # unquoted column, e.g., ADTDens
    out_dir = "species_plots",           # folder to write PDFs
    species_codes = NULL,                # default = all species in df & lookup
    streams = NULL,                      # optional: c("Abrams Creek","West Prong Little Pigeon")
    fit_type = c("nb","gaussian"),
    target   = c("avg_stream_sum","network_sum"),
    width = 9, height = 6, pointsize = 12,
    title_prefix = "Adult density — "    # used before "Common (CODE)"
) {
  fit_type <- match.arg(fit_type)
  target   <- match.arg(target)
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Determine species set (allow common/scientific/codes)
  all_in_data <- sort(unique(df$Species))
  default_set <- sort(intersect(all_in_data, species_lookup$code))
  if (is.null(species_codes)) {
    species_codes <- default_set
  } else {
    species_codes <- resolve_species_terms(species_codes)
    species_codes <- intersect(species_codes, all_in_data)
  }
  if (!length(species_codes)) stop("No valid species to plot after matching to data and lookup.")
  
  # filename helper
  sanitize <- function(x) {
    x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
    gsub("_+", "_", x)
  }
  
  for (sp in species_codes) {
    # Title like "Adult density — Brook Trout (BKT)"
    title_txt <- paste0(title_prefix, unname(species_labeler(sp)))
    
    # Filename like "BKT_Brook_Trout.pdf"
    label   <- unname(species_labeler(sp))                 # "Brook Trout (BKT)"
    code    <- sub(".*\\(([^)]+)\\).*", "\\1", label)      # "BKT"
    common  <- trimws(sub(" \\([^)]*\\)$", "", label))     # "Brook Trout"
    pdf_path <- file.path(out_dir, paste0(code, "_", sanitize(common), ".pdf"))
    
    # Build the plot (single species; streams overlaid)
    p <- try(
      plot_stream_totals_with_fit(
        df, {{ var }},
        ylab = "Adult density",
        title_txt = title_txt,
        species = sp,
        streams = streams,                 # keep NULL to include all streams
        species_mode = "separate",         # per-species, multi-stream overlay
        fit_type = fit_type,
        show_legend = FALSE,
        target = target
      ),
      silent = TRUE
    )
    if (inherits(p, "try-error") || is.null(p)) {
      message("Skipping ", sp, " (insufficient data or fit error).")
      next
    }
    
    # Write a single-page PDF for this species
    grDevices::pdf(pdf_path, width = width, height = height, pointsize = pointsize)
    print(p)
    grDevices::dev.off()
  }
  
  invisible(out_dir)
}





export_species_metrics_pdf_onefn <- function(
    df,
    species_lookup,                 # tibble with columns: code, common, scientific
    out_file,
    metrics = c("TOTpop","YOYpop","ADTpop",
                "TOTDens","YOYDens","ADTDens",
                "TOTBiom","YOYBiom","ADTBiom",
                "TOTmnwt","YOYmnwt","ADTmnwt"),
    species_set   = NULL,           # optional subset of codes (e.g., c("BKT","RBT"))
    fit_type      = c("gaussian","nb"),
    width         = 14,
    height        = 10,
    pointsize     = 12,
    min_points_pos = 12,            # minimum positive datapoints (plotted on log axis)
    min_years_pos  = 6,             # minimum distinct years among positive points
    palette       = c("#a6cee3","#1f78b4","#b2df8a","#33a02c",
                      "#fb9a99","#e31a1c","#fdbf6f","#ff7f00",
                      "#cab2d6","#6a3d9a","#ffff99","#b15928"),
    panel_ar      = 0.62
) {
  fit_type <- match.arg(fit_type)
  
  # ---- sanity checks ----
  needed_cols <- c("Species","Stream","Date")
  miss_base <- setdiff(needed_cols, names(df))
  if (length(miss_base)) stop("Missing columns in df: ", paste(miss_base, collapse = ", "))
  
  metrics <- intersect(metrics, names(df))
  if (!length(metrics)) stop("None of the requested metrics are present in df.")
  
  if (!all(c("code","common","scientific") %in% names(species_lookup))) {
    stop("species_lookup must have columns: code, common, scientific")
  }
  
  # normalize lookup
  species_lu_norm <- species_lookup |>
    dplyr::transmute(
      Species    = toupper(trimws(.data$code)),
      CommonName = trimws(.data$common),
      Scientific = trimws(.data$scientific)
    ) |>
    dplyr::distinct(Species, .keep_all = TRUE)
  
  # species set
  all_in_data <- sort(unique(df$Species))
  valid_codes <- intersect(all_in_data, species_lu_norm$Species)
  if (is.null(species_set)) {
    species_set <- valid_codes
  } else {
    species_set <- toupper(trimws(species_set))
    species_set <- intersect(species_set, all_in_data)
  }
  if (!length(species_set)) stop("No valid species to plot after matching to data & lookup.")
  
  # ensure output dir exists
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  # ---- local helpers (scoped inside) ----
  title_for_species <- function(code) {
    code_norm <- toupper(trimws(code))
    hit <- dplyr::filter(species_lu_norm, .data$Species == code_norm)
    if (nrow(hit) >= 1 && nzchar(hit$CommonName[1]) && nzchar(hit$Scientific[1])) {
      paste0(hit$CommonName[1], " - ", hit$Scientific[1], " (", code_norm, ")")
    } else paste0("Species ", code_norm)
  }
  
  make_sy_gauss <- function(df, sp, var_name) {
    df |>
      dplyr::filter(.data$Species == sp) |>
      dplyr::select(Stream, Date, !!var_name := .data[[var_name]]) |>
      dplyr::mutate(Year = lubridate::year(.data$Date)) |>
      dplyr::group_by(Stream, Year) |>
      dplyr::summarise(Total = sum(.data[[var_name]], na.rm = TRUE), .groups = "drop") |>
      dplyr::filter(Total > 0)
  }
  
  make_sy_nb <- function(df, sp, var_name) {
    df |>
      dplyr::filter(.data$Species == sp) |>
      dplyr::select(Stream, Date, !!var_name := .data[[var_name]]) |>
      dplyr::mutate(Year = lubridate::year(.data$Date)) |>
      dplyr::group_by(Stream, Year) |>
      dplyr::summarise(Total = sum(.data[[var_name]], na.rm = TRUE), .groups = "drop")
  }
  
  fit_global_gamm <- function(sy, k = 10, fit_type = c("gaussian","nb")) {
    fit_type <- match.arg(fit_type)
    sy <- sy |>
      dplyr::mutate(Year = as.numeric(.data$Year),
                    Stream = as.factor(.data$Stream))
    n_years <- dplyr::n_distinct(sy$Year)
    if (n_years < 3 || nrow(sy) < 3) return(NULL)
    k_eff <- max(3, min(k, n_years - 1))
    new_years <- sort(unique(sy$Year))
    ref_level <- levels(sy$Stream)[1]
    nd <- data.frame(Year = new_years, Stream = factor(ref_level, levels = levels(sy$Stream)))
    
    if (fit_type == "gaussian") {
      fit <- try(
        mgcv::gam(log(Total) ~ s(Year, k = k_eff, bs = "cr") + s(Stream, bs = "re"),
                  data = sy, family = stats::gaussian(), method = "REML", select = TRUE),
        silent = TRUE
      )
      if (!inherits(fit, "try-error")) {
        excl <- grep("^s\\(Stream\\)", sapply(fit$smooth, `[[`, "label"), value = TRUE)
        if (!length(excl)) excl <- "s(Stream)"
        pred <- mgcv::predict.gam(fit, newdata = nd, type = "link", exclude = excl)
        return(tibble::tibble(Year = new_years, yhat = exp(pred)))
      }
      fit_lin <- try(stats::lm(log(Total) ~ Year, data = sy), silent = TRUE)
      if (!inherits(fit_lin, "try-error")) {
        pred <- stats::predict(fit_lin, newdata = data.frame(Year = new_years), type = "response")
        return(tibble::tibble(Year = new_years, yhat = exp(pred)))
      }
    } else {
      fit <- try(
        mgcv::gam(Total ~ s(Year, k = k_eff, bs = "cr") + s(Stream, bs = "re"),
                  data = sy, family = mgcv::nb(), method = "REML", select = TRUE),
        silent = TRUE
      )
      if (!inherits(fit, "try-error")) {
        excl <- grep("^s\\(Stream\\)", sapply(fit$smooth, `[[`, "label"), value = TRUE)
        if (!length(excl)) excl <- "s(Stream)"
        eta <- mgcv::predict.gam(fit, newdata = nd, type = "link", exclude = excl)
        return(tibble::tibble(Year = new_years, yhat = fit$family$linkinv(eta)))
      }
      fit_lin <- try(stats::glm(Total ~ Year, data = sy, family = stats::poisson()), silent = TRUE)
      if (!inherits(fit_lin, "try-error")) {
        eta <- stats::predict(fit_lin, newdata = data.frame(Year = new_years), type = "link")
        return(tibble::tibble(Year = new_years, yhat = exp(eta)))
      }
    }
    NULL
  }
  
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
  
  pick_ncol <- function(n) { if (n <= 2) 1 else if (n <= 4) 2 else if (n <= 6) 3 else 4 }
  pick_legend_ncol <- function(n_streams) { max(2, min(6, ceiling(n_streams / 3))) }
  legend_height_frac <- function(n_streams, ncol) {
    rows <- ceiling(n_streams / ncol)
    min(0.18, 0.08 + 0.03 * (rows - 1))
  }
  
  plot_metric_core <- function(df_metric, main_title, ylab_txt, scale_panel) {
    smooth_df <- fit_global_gamm(df_metric, k = 10, fit_type = fit_type)
    df_plot <- dplyr::filter(df_metric, Total > 0)
    df_line <- df_plot |>
      dplyr::group_by(Stream) |>
      dplyr::filter(dplyr::n() >= 2) |>
      dplyr::ungroup()
    
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(Year, Total, color = Stream, group = Stream)) +
      ggplot2::geom_line(data = df_line, show.legend = FALSE) +
      ggplot2::geom_point(size = 1.5) +
      scale_panel +
      ggplot2::labs(x = "Year", y = ylab_txt, title = main_title) +
      ggplot2::theme_bw() +
      ggplot2::theme(aspect.ratio = panel_ar) +
      ggplot2::scale_y_log10()
    
    if (!is.null(smooth_df)) {
      p <- p + ggplot2::geom_line(
        data = smooth_df, ggplot2::aes(Year, yhat),
        inherit.aes = FALSE, color = "black", linewidth = 1.2, show.legend = FALSE
      )
    }
    p
  }
  
  # ---- main loop ----
  pdf_open <- FALSE
  pages_written <- 0L
  
  for (sp in species_set) {
    # per-metric data (different aggregators for fit_type)
    metric_dfs <- lapply(metrics, function(mn) {
      if (fit_type == "gaussian") make_sy_gauss(df, sp, mn) else make_sy_nb(df, sp, mn)
    })
    names(metric_dfs) <- metrics
    
    # require sufficient POSITIVE support (what we’ll plot on log axis)
    keep <- vapply(
      metric_dfs,
      function(dd) {
        if (is.null(dd) || !nrow(dd)) return(FALSE)
        dd_pos <- dplyr::filter(dd, Total > 0)
        (nrow(dd_pos) >= min_points_pos) && (dplyr::n_distinct(dd_pos$Year) >= min_years_pos)
      },
      logical(1)
    )
    metric_dfs <- metric_dfs[keep]
    if (!length(metric_dfs)) next
    
    # union of streams for legend
    union_streams <- sort(unique(unlist(lapply(metric_dfs, function(dd) unique(dd$Stream)))))
    if (!length(union_streams)) next
    
    pal_union <- stats::setNames(rep_len(palette, length(union_streams)), union_streams)
    scale_panel <- ggplot2::scale_color_manual(
      values = pal_union, breaks = union_streams, limits = union_streams, drop = FALSE, name = "Stream",
      guide = "none"
    )
    
    metric_titles_sub <- metric_titles[names(metric_dfs)]
    ylabs_sub         <- ylabs[names(metric_dfs)]
    
    plots <- Map(function(name, dd)
      plot_metric_core(dd, metric_titles_sub[[name]], ylabs_sub[[name]], scale_panel),
      names(metric_dfs), metric_dfs)
    
    ncol   <- pick_ncol(length(plots))
    panels <- patchwork::wrap_plots(plots, ncol = ncol)
    
    # shared legend
    legend_ncol <- pick_legend_ncol(length(union_streams))
    legend_plot <- ggplot2::ggplot(
      data.frame(Stream = factor(union_streams, levels = union_streams), x = 1, y = 1),
      ggplot2::aes(x, y, color = Stream)
    ) +
      ggplot2::geom_point(size = 1) +
      ggplot2::scale_color_manual(
        values = pal_union, breaks = union_streams, limits = union_streams, drop = FALSE, name = "Stream"
      ) +
      ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_ncol, byrow = TRUE,
                                                    override.aes = list(linetype = 0, size = 1.5))) +
      ggplot2::theme_void(base_size = 10) +
      ggplot2::theme(
        legend.position   = "bottom",
        legend.direction  = "horizontal",
        legend.justification = "center",
        legend.text       = ggplot2::element_text(size = 8),
        legend.key.height = grid::unit(3, "mm"),
        legend.key.width  = grid::unit(6, "mm"),
        legend.margin     = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
      )
    
    g <- ggplot2::ggplotGrob(legend_plot)
    guide_idx <- which(sapply(g$grobs, function(x) grepl("guide-box", x$name)))
    if (!length(guide_idx)) next
    legend_box <- g$grobs[[guide_idx[1]]]
    
    h_legend <- legend_height_frac(length(union_streams), legend_ncol)
    page <- (panels / patchwork::wrap_elements(legend_box)) +
      patchwork::plot_layout(heights = c(1, h_legend)) +
      patchwork::plot_annotation(
        title = title_for_species(sp),
        theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16))
      )
    
    if (!pdf_open) {
      grDevices::pdf(out_file, width = width, height = height, pointsize = pointsize)
      pdf_open <- TRUE
    }
    print(page)
    pages_written <- pages_written + 1L
  }
  
  if (pdf_open) grDevices::dev.off()
  if (pages_written == 0L) message("No species/pages written (not enough data).")
}

# ---- Example calls ----


# Negative Binomial (fit uses zeros; panels still log of positives)
#export_species_metrics_pdf_onefn(
#  df = three_pass,
#  species_lookup = species_lookup,
#  out_file = "~/Downloads/species_metrics_ALL_species_nb.pdf",
#  metrics = intersect(
#    c("TOTpop","YOYpop","ADTpop","TOTDens","YOYDens","ADTDens",
#      "TOTBiom","YOYBiom","ADTBiom","TOTmnwt","YOYmnwt","ADTmnwt"),
#    names(three_pass)
#  ),
#  species_set   = NULL,
#  fit_type      = "nb", #Negative binomial; also try "guassian"
#  min_points_pos = 12,
#  min_years_pos  = 6
#)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(patchwork)
})

# --- Chao1 estimator ---
chao1_abund <- function(x) {
  x <- x[x > 0]
  S_obs <- length(x)
  f1 <- sum(x == 1L)
  f2 <- sum(x == 2L)
  if (f1 == 0) return(S_obs)
  if (f2 > 0) S_obs + (f1 * f1) / (2 * f2) else S_obs + (f1 * (f1 - 1)) / 2
}

# --- Integer breaks helper (ensures ≥3 ticks) ---
integer_breaks_min <- function(min_breaks = 3, max_breaks = 6) {
  force(min_breaks); force(max_breaks)
  function(lims) {
    lo <- 0
    hi <- ceiling(max(lims, na.rm = TRUE))
    hi <- max(hi, min_breaks - 1)   # ensure room for 0..(min_breaks-1)
    step <- max(1, ceiling((hi - lo) / (max_breaks - 1)))
    br <- seq(lo, hi, by = step)
    if (length(br) < min_breaks) br <- seq(lo, lo + (min_breaks - 1), by = 1)
    br
  }
}
# --- Exact individuals-based rarefaction (expected richness at m individuals) ---
.rarefaction_expected_S <- function(abund_vec, m_vals) {
  abund_vec <- as.numeric(abund_vec[abund_vec > 0])
  N <- sum(abund_vec)
  m_vals <- m_vals[m_vals >= 1 & m_vals <= N]
  if (!length(m_vals)) return(data.frame(m = integer(0), ESm = numeric(0)))
  
  # Use log-choose for stability: E[S_m] = sum_i (1 - C(N - n_i, m) / C(N, m))
  lC_N_m <- lgamma(N + 1) - (lgamma(m_vals + 1) + lgamma(N - m_vals + 1))  # vector
  ESm <- vapply(m_vals, function(m) {
    lC_Nm_m <- lgamma(N - abund_vec + 1) - (lgamma(m + 1) + lgamma(N - abund_vec - m + 1))
    # 1 - exp(lC(N - n_i, m) - lC(N, m)), summed over species
    sum(1 - exp(lC_Nm_m - (lgamma(N + 1) - (lgamma(m + 1) + lgamma(N - m + 1)))))
  }, numeric(1))
  data.frame(m = m_vals, ESm = ESm)
}

# --- All-streams plot with optional rarefaction curve ---
plot_all_streams_cumulative <- function(df,
                                        species_col    = "Species",
                                        count_col      = "TOTpop",
                                        date_col       = "Date",
                                        log_x          = TRUE,
                                        add_rarefaction= FALSE,
                                        raref_n_points = 400,
                                        title          = "All streams combined",
                                        theme_fn       = theme_plot) {
  dat <- df %>%
    mutate(Year = lubridate::year(.data[[date_col]])) %>%
    group_by(Year, .data[[species_col]]) %>%
    summarise(n = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop")
  if (nrow(dat) == 0L) return(NULL)
  
  wide <- dat %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(species_col),
                       values_from = n, values_fill = 0) %>%
    arrange(Year)
  
  mat0 <- as.matrix(wide[, -match("Year", names(wide)), drop = FALSE])
  storage.mode(mat0) <- "double"
  
  # drop years with zero individuals
  keep_rows <- rowSums(mat0) > 0
  wide <- wide[keep_rows, , drop = FALSE]
  if (nrow(wide) == 0L) return(NULL)
  
  years <- wide$Year
  mat   <- as.matrix(wide[, -match("Year", names(wide)), drop = FALSE])
  storage.mode(mat) <- "double"
  
  # cumulative individuals & cumulative richness
  mat_cum_by_species <- apply(mat, 2, cumsum)
  S_obs_cum <- rowSums(mat_cum_by_species > 0)
  n_per_year <- rowSums(mat)
  n_cum <- cumsum(n_per_year)
  
  # final Chao1 on totals across all years
  x_final <- colSums(mat)
  S_chao  <- chao1_abund(x_final)
  
  x_min <- min(n_cum, na.rm = TRUE)
  x_max <- max(n_cum, na.rm = TRUE)
  y_min <- 0
  y_max <- ceiling(max(S_obs_cum, S_chao, na.rm = TRUE))
  
  # optional rarefaction curve from pooled abundances
  raref_df <- NULL
  if (add_rarefaction) {
    N <- sum(x_final)
    # choose m values spaced on log-scale if log_x, else linear; always unique, within [1, N]
    if (N <= raref_n_points) {
      m_vals <- seq_len(N)
    } else if (log_x) {
      m_vals <- unique(floor(exp(seq(log(1), log(N), length.out = raref_n_points))))
      m_vals[m_vals < 1] <- 1L
      m_vals[m_vals > N] <- N
    } else {
      m_vals <- unique(round(seq(1, N, length.out = raref_n_points)))
    }
    raref_df <- .rarefaction_expected_S(x_final, m_vals)
  }
  
  p <- ggplot2::ggplot(
    data.frame(Year = years, n_cum = n_cum, S_obs_cum = S_obs_cum),
    ggplot2::aes(x = n_cum, y = S_obs_cum)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.6)
  
  if (!is.null(raref_df) && nrow(raref_df)) {
    # plot rarefaction as a dashed line; no explicit color to avoid palette changes
    p <- p + ggplot2::geom_line(
      data = raref_df,
      ggplot2::aes(x = m, y = ESm),
      linetype = "dashed"
    )
  }
  
  p <- p +
    ggplot2::geom_segment(
      ggplot2::aes(x = x_min, xend = x_max, y = S_chao, yend = S_chao),
      linewidth = 0.9, color = "firebrick2"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Cumulative richness vs cumulative individuals",
      x = "Cumulative individuals",
      y = "Cumulative observed richness"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = integer_breaks_min(min_breaks = 3, max_breaks = 6),
      minor_breaks = NULL,
      labels = function(x) x
    ) +
    theme_fn()
  
  if (log_x) {
    p <- p + ggplot2::scale_x_log10(labels = scales::label_number(big.mark = ","))
  } else {
    p <- p + ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","))
  }
  
  p
}

# --- Example ---
# p_all <- plot_all_streams_cumulative(
#   three_pass, log_x = TRUE, add_rarefaction = TRUE, theme_fn = theme_bw
# )
# p_all


# --- Single stream plot ---
plot_stream_cumulative <- function(df, stream_name,
                                   species_col = "Species",
                                   stream_col  = "Stream",
                                   count_col   = "TOTpop",
                                   date_col    = "Date",
                                   log_x       = TRUE,
                                   theme_fn    = theme_plot) {
  
  dat_stream <- df %>%
    filter(.data[[stream_col]] == stream_name) %>%
    mutate(Year = year(.data[[date_col]])) %>%
    group_by(Year, .data[[species_col]]) %>%
    summarise(n = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop")
  
  if (nrow(dat_stream) == 0L) return(NULL)
  
  wide <- dat_stream %>%
    pivot_wider(names_from = all_of(species_col),
                values_from = n, values_fill = 0) %>%
    arrange(Year)
  
  mat0 <- as.matrix(wide[, -match("Year", names(wide)), drop = FALSE])
  storage.mode(mat0) <- "double"
  
  # drop years with zero individuals
  keep_rows <- rowSums(mat0) > 0
  wide <- wide[keep_rows, , drop = FALSE]
  if (nrow(wide) == 0L) return(NULL)
  
  years <- wide$Year
  mat   <- as.matrix(wide[, -match("Year", names(wide)), drop = FALSE])
  storage.mode(mat) <- "double"
  
  # cumulative individuals & cumulative richness
  mat_cum_by_species <- apply(mat, 2, cumsum)
  S_obs_cum <- rowSums(mat_cum_by_species > 0)
  n_per_year <- rowSums(mat)
  n_cum <- cumsum(n_per_year)
  
  # final Chao1
  x_final <- colSums(mat)
  S_chao  <- chao1_abund(x_final)
  x_min   <- min(n_cum, na.rm = TRUE)
  x_max   <- max(n_cum, na.rm = TRUE)
  
  y_min <- 0
  y_max <- ceiling(max(S_obs_cum, S_chao, na.rm = TRUE))
  
  p <- ggplot(data.frame(Year = years, n_cum = n_cum, S_obs_cum = S_obs_cum),
              aes(x = n_cum, y = S_obs_cum)) +
    geom_line() +
    geom_point(size = 1.6) +
    geom_segment(aes(x = x_min, xend = x_max, y = S_chao, yend = S_chao),
                 linewidth = 0.9, color = "firebrick2") +
    labs(
      title = stream_name,
      subtitle = "Cumulative richness vs cumulative individuals",
      x = "Cumulative individuals",
      y = "Cumulative observed richness"
    ) +
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = integer_breaks_min(min_breaks = 3, max_breaks = 6),
      minor_breaks = NULL,
      labels = function(x) x
    ) +
    theme_fn()
  
  if (log_x) {
    p <- p + scale_x_log10(labels = scales::label_number(big.mark = ","))
  } else {
    p <- p + scale_x_continuous(labels = scales::label_number(big.mark = ","))
  }
  
  p
}

# --- Multi-page PDF exporter (9 plots per page) ---
export_stream_chao_pdf <- function(df,
                                         out_file,
                                         streams = NULL,
                                         log_x = TRUE,
                                         ncol = 3, nrow = 3,
                                         use_theme_bw = TRUE) {
  
  if (is.null(streams)) {
    streams <- df %>%
      group_by(Stream) %>%
      summarise(n_total = sum(TOTpop, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(n_total)) %>%
      pull(Stream)
  }
  
  theme_fn <- if (use_theme_bw) theme_bw else theme_plot
  
  plots <- lapply(streams, function(s) {
    tryCatch(
      plot_stream_cumulative(df, stream_name = s, log_x = log_x, theme_fn = theme_fn),
      error = function(e) NULL
    )
  })
  plots <- Filter(Negate(is.null), plots)
  if (!length(plots)) stop("No plots produced.")
  
  groups <- split(plots, ceiling(seq_along(plots) / (ncol * nrow)))
  pdf(out_file, width = 11, height = 8.5, onefile = TRUE)
  on.exit(dev.off(), add = TRUE)
  
  for (i in seq_along(groups)) {
    page_plots <- groups[[i]]
    if (length(page_plots) < (ncol * nrow)) {
      page_plots <- c(
        page_plots,
        rep(list(patchwork::plot_spacer()), (ncol * nrow) - length(page_plots))
      )
    }
    print(patchwork::wrap_plots(page_plots, ncol = ncol, nrow = nrow))
  }
  invisible(out_file)
}

# --- Examples ---
# Single stream:
# p_ac <- plot_stream_cumulative(three_pass, "Abrams Creek", log_x = TRUE)
# p_ac
#
# Multi-page PDF (9 per page):
# export_stream_chao_pdf (
#   df = three_pass,
#   out_file = "~/Downloads/cumulative_richness_by_stream.pdf",
#   log_x = TRUE,
#   use_theme_bw = TRUE
# )

#################### Combine streams for rarefaction curve ########

 
# --- All-streams plot with optional individuals-based rarefaction curve ---
plot_all_streams_cumulative <- function(df,
                                        species_col     = "Species",
                                        count_col       = "TOTpop",
                                        date_col        = "Date",
                                        log_x           = TRUE,
                                        add_rarefaction = TRUE,
                                        raref_n_points  = 400,
                                        title           = "All streams combined",
                                        theme_fn        = theme_plot) {
  # helper: exact individuals-based rarefaction expectation E[S_m]
  rarefaction_expected_S <- function(abund_vec, m_vals) {
    abund_vec <- as.numeric(abund_vec[abund_vec > 0])
    N <- sum(abund_vec)
    if (!length(abund_vec) || N <= 1) return(data.frame(m = integer(0), ESm = numeric(0)))
    
    m_vals <- m_vals[m_vals >= 1 & m_vals <= N]
    m_vals <- sort(unique(as.integer(m_vals)))
    if (!length(m_vals)) return(data.frame(m = integer(0), ESm = numeric(0)))
    
    lC_N <- lgamma(N + 1)
    ESm <- vapply(m_vals, function(m) {
      # log C(N, m)
      lC_Nm <- lC_N - (lgamma(m + 1) + lgamma(N - m + 1))
      # log C(N - n_i, m); if N - n_i < m then term = 0 (species must appear)
      valid <- (N - abund_vec) >= m
      lC_Nminusni_m <- rep(-Inf, length(abund_vec))
      lC_Nminusni_m[valid] <- lgamma(N - abund_vec[valid] + 1) -
        (lgamma(m + 1) + lgamma(N - abund_vec[valid] - m + 1))
      sum(1 - exp(lC_Nminusni_m - lC_Nm))
    }, numeric(1))
    
    data.frame(m = m_vals, ESm = ESm)
  }
  
  dat <- df %>%
    dplyr::mutate(Year = lubridate::year(.data[[date_col]])) %>%
    dplyr::group_by(Year, .data[[species_col]]) %>%
    dplyr::summarise(n = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop")
  if (nrow(dat) == 0L) return(NULL)
  
  wide <- dat %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(species_col),
                       values_from = n, values_fill = 0) %>%
    dplyr::arrange(Year)
  
  mat0 <- as.matrix(wide[, -match("Year", names(wide)), drop = FALSE])
  storage.mode(mat0) <- "double"
  
  # drop years with zero individuals
  keep_rows <- rowSums(mat0) > 0
  wide <- wide[keep_rows, , drop = FALSE]
  if (nrow(wide) == 0L) return(NULL)
  
  years <- wide$Year
  mat   <- as.matrix(wide[, -match("Year", names(wide)), drop = FALSE])
  storage.mode(mat) <- "double"
  
  # cumulative individuals & cumulative richness
  mat_cum_by_species <- apply(mat, 2, cumsum)
  S_obs_cum <- rowSums(mat_cum_by_species > 0)
  n_per_year <- rowSums(mat)
  n_cum <- cumsum(n_per_year)
  
  # final Chao1 on totals across all years (uses your chao1_abund())
  x_final <- colSums(mat)
  S_chao  <- chao1_abund(x_final)
  
  x_min <- min(n_cum, na.rm = TRUE)
  x_max <- max(n_cum, na.rm = TRUE)
  y_min <- 0
  y_max <- ceiling(max(S_obs_cum, S_chao, na.rm = TRUE))
  
  # rarefaction curve (pooled abundances)
  raref_df <- NULL
  if (add_rarefaction) {
    N <- sum(x_final)
    if (N <= raref_n_points) {
      m_vals <- seq_len(N)
    } else if (isTRUE(log_x)) {
      m_vals <- unique(floor(exp(seq(log(1), log(N), length.out = raref_n_points))))
      m_vals <- m_vals[m_vals >= 1 & m_vals <= N]
    } else {
      m_vals <- unique(round(seq(1, N, length.out = raref_n_points)))
    }
    raref_df <- rarefaction_expected_S(x_final, m_vals)
  }
  
  p <- ggplot2::ggplot(
    data.frame(Year = years, n_cum = n_cum, S_obs_cum = S_obs_cum),
    ggplot2::aes(x = n_cum, y = S_obs_cum)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.6)
  
  if (!is.null(raref_df) && nrow(raref_df) > 0) {
    p <- p + ggplot2::geom_line(
      data = raref_df,
      mapping = ggplot2::aes(x = m, y = ESm),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 0.8
    )
  }
  
  p <- p +
    ggplot2::geom_segment(
      ggplot2::aes(x = x_min, xend = x_max, y = S_chao, yend = S_chao),
      linewidth = 0.9, color = "firebrick2"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Cumulative richness vs cumulative individuals",
      x = "Cumulative individuals",
      y = "Cumulative observed richness"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = integer_breaks_min(min_breaks = 3, max_breaks = 6),
      minor_breaks = NULL,
      labels = function(x) x
    ) +
    theme_fn()
  
  if (log_x) {
    p <- p + ggplot2::scale_x_log10(labels = scales::label_number(big.mark = ","))
  } else {
    p <- p + ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","))
  }
  
  p
}

# --- Example ---
# p_all <- plot_all_streams_cumulative(three_pass, log_x = TRUE,add_rarefaction = TRUE, theme_fn = theme_bw)
# p_all
# export_all_streams_chao_pdf(three_pass, "~/Downloads/cumulative_richness_ALL_streams.pdf",
#                             log_x = TRUE, add_rarefaction = T, use_theme_bw = TRUE)

# run the all-streams plot with rarefaction
p_all <- plot_all_streams_cumulative(
  three_pass,
  log_x = TRUE,           # x-axis on log scale
  add_rarefaction = T, # turn on rarefaction curve
  theme_fn = theme_bw     # or your custom theme_plot
)

# display in RStudio / R
p_all

pdf("~/Downloads/cumulative_richness_ALL_streams.pdf", width = 7.5, height = 6)
print(p_all)
dev.off()


