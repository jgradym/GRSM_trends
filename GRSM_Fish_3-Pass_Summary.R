library(readxl)
library(patchwork)
library(lubridate)
library(mgcv)
library(grid)    
library(gtable)  
library(gridExtra)
library(rlang)
library(tidyverse)



three_pass <- read_xlsx(
  "/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Old/GRSM File Transfers/GRSM_Fish_3-Pass_Summary.xlsx",
  sheet = "Summary"
)
str(three_pass)


source("/Users/jgradym/Documents/GitHub/GRSM_trends/GSRM_fish_plotting.R")

# Sampleing effort 


# ===============================================================
# Summarize sampling effort by Year × Stream  (unique site-date)
# ===============================================================
effort_samples_stream <- three_pass %>%
  mutate(Year = lubridate::year(Date)) %>%
  distinct(Stream, Site, Date, Year) %>%          # one sample per site-date
  count(Year, Stream, name = "n_samples") %>%     # samples per Year × Stream
  ungroup()

# ===============================================================
# Plot stacked bar: samples per year, filled by Stream
# ===============================================================
p1 <- ggplot(effort_samples_stream,
             aes(x = as.integer(Year), y = n_samples, fill = Stream)) +
  geom_col() +
  scale_x_continuous(
    breaks = seq(min(effort_samples_stream$Year, na.rm = TRUE),
                 max(effort_samples_stream$Year, na.rm = TRUE), by = 5)
  ) +
  labs(x = "Year", y = "Number of samples (unique site-dates)",
       title = "Fish Three-Pass: Samples per Year by Stream") +
  theme_plot() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 5)
  ) +
  scale_fill_viridis_d(option = "turbo")
p1

# ===============================================================
# Keep streams with ≥10 sampled years; show which years were sampled
# ===============================================================
df_year_presence <- three_pass %>%
  mutate(Year = lubridate::year(Date)) %>%
  distinct(Stream, Year) %>%                       # presence/absence by year
  group_by(Stream) %>%
  filter(n_distinct(Year) >= 10) %>%               # streams with ≥10 years
  ungroup()

p2 <- ggplot(df_year_presence, aes(x = as.integer(Year), fill = Stream)) +
  geom_col(aes(y = 1)) +                           # each stream-year contributes 1
  scale_x_continuous(
    breaks = seq(min(df_year_presence$Year, na.rm = TRUE),
                 max(df_year_presence$Year, na.rm = TRUE), by = 5)
  ) +
  labs(x = "Year", y = "Streams sampled (≥10-year streams only)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_plot() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  ) +
  scale_fill_viridis_d(option = "turbo")
p2


# ===== Community metrics — FAST nested RE (Stream + Site-within-Stream) with mgcv::bam =====
# Produces multi-panel PDF (4 per page) with a single black population-level smooth per plot.
# Estimand (black line) defaults to: expected value for a *typical site* (exclude both REs).
# -------------------------------------------------------------------------
# 2) Separate species overlays (BKT=greens, RBT=blues, BNT=reds), NegBin (zeros kept)

p_sep <- plot_stream_totals_with_fit(
  three_pass, ADTDens,
  ylab = "Adult density",
  title_txt = "# Adult Density — Trout separate",
  species = c("Brown Trout", "Rainbow trout", "brook trout"),
  #streams = c("Abrams Creek","West Prong Little Pigeon"),
  species_mode = "separate",
  fit_type = "gaussian",
  show_legend = FALSE
)
print(p_sep)
title_txt <- expression("Adult Density (100" * m^-2 * ")")

p_pooled <- plot_stream_totals_with_fit(
  three_pass, 
  var = ADTDens,
  ylab = expression("Adult Density (100 " * m^-2 * ")"),
  title_txt =  "Adult Density",
  species_mode = "pooled",
  fit_type = "nb",
  show_legend = FALSE
)
print(p_pooled)

ADTmnwt

p_pooled <- plot_stream_totals_with_fit(
  three_pass, 
  var = ADTmnwt,
  ylab = "Adult weight",
  title_txt = "# Adult Weight (g)",
  species_mode = "pooled",
  fit_type = "nb",
  show_legend = FALSE
)
print(p_pooled)
# Plot species with mixed model fit

str(three_pass)
export_species_combined_pdf(
  df = three_pass,
  out_file = "~/Downloads/fish_combined4.pdf",
  fit_type = "nb"
)


export_stream_chao_pdf (
  df = three_pass,
  out_file = "~/Downloads/cumulative_richness_by_stream3.pdf",
  log_x = TRUE,
  use_theme_bw = TRUE
)



