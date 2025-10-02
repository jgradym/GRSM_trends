#Explore invert community abundance at GSRM

library(tidyverse)
inverts = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Aquatics_Macroinverts/SummaryData/Specimen_Data_Export.csv')

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
    plot.title        = element_text(size = 18, face = "plain", hjust = 10),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.background  = element_blank(),
    strip.background  = element_blank(),
    legend.text       = element_text(size = 15),   
    legend.title      = element_text(size = 18),   
    legend.position   = if (legend) "right" else "none",
    text              = element_text(family = "Helvetica")
  )
}

# remove white space around values
inverts <- inverts %>%
  mutate(across(everything(), ~ if(is.character(.x)) trimws(.x) else .x))

#Add helpful columns
inverts$Location = word(inverts$LOC_NAME, 1, sep = ",")
inverts$Site = word(inverts$LOC_NAME, 2, sep = ",")
inverts$Genus = word(inverts$Lab_Scientific_Name, 1, sep = " ")
inverts$Year = as.numeric(word(inverts$Start_Date , 1, sep = "-"))
str(inverts)

#Check sampling effort by year

inverts %>%
  group_by(Location) %>%
  summarise(n_samples = n_distinct(Sample_Code),
            n_years   = n_distinct(Year),
            .groups = "drop") %>%
  arrange(desc(n_samples)) %>%
  print(n = 27)

# Samples by year and # recorded taxa
inverts %>%
  group_by(Year) %>%
  summarise(n_samples = n_distinct(Sample_Code),
            n_recorded_taxa = n(),
            .groups = "drop") %>%
  print(n = 27)


# ===============================================================
# Summarize sampling effort by year × site
# ===============================================================
effort_samples <- inverts %>%
  group_by(Year, Location) %>%
  summarise(n_samples = n_distinct(Sample_Code), .groups = "drop")

# ===============================================================
# Plot stacked bar: samples per year, filled by site
# ===============================================================


# Total Sample effort

p = ggplot(effort_samples, aes(x = as.integer(Year), y = n_samples, fill = Location)) +
  geom_col() +
  scale_x_continuous(breaks = seq(min(effort_samples$Year), max(effort_samples$Year), by = 5)) +
  labs(x = "Year", y = "Number of samples", title = "Samples per Year by Site") +
  theme_plot() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(1, 1),       # inset in upper right
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(0.4, "cm"),     # shrink keys
    legend.text = element_text(size = 4),  # smaller font
    legend.title = element_text(size = 5)  # smaller title font
  ) +
  scale_fill_viridis_d(option = "turbo")
p

# Subset to minimum of 10 sampled years, no replicates per year

df_plot <- effort_samples %>%
  mutate(Year = as.integer(Year)) %>%
  distinct(Location, Year) %>%                 # one row per site-year
  group_by(Location) %>%
  filter(n_distinct(Year) >= 10) %>%           # keep sites with ≥10 years sampled
  ungroup()

p <- ggplot(df_plot, aes(x = Year, fill = Location)) +
  geom_col(aes(y = 1)) +                       # each site-year contributes exactly 1
  scale_x_continuous(breaks = seq(min(df_plot$Year), max(df_plot$Year), by = 5)) +
  labs(x = "Year", y = "Min 10 Years Locations Sampled") +
  scale_y_continuous(limits = c(0, 15)) +
  theme_plot() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size =  10),
    legend.title = element_text(size = 12)
  ) +
  scale_fill_viridis_d(option = "turbo")

p


sample_totals <- inverts %>%
  group_by(Year, LOC_NAME, Sample_Code) %>%
  summarise(total_count = sum(Count, na.rm = TRUE),
            n_taxa      = n_distinct(Lab_Species), .groups = "drop")

# Look a count of individuals and taxa
siteyear_spread <- sample_totals %>%
  group_by(Year, LOC_NAME) %>%
  summarise(
    mean_count = mean(total_count),
    sd_count   = sd(total_count),
    min_count  = min(total_count),
    max_count  = max(total_count),
    mean_taxa  = mean(n_taxa),
    sd_taxa    = sd(n_taxa),
    .groups = "drop"
  )

siteyear_spread



###### Abundance by Order/Family Trends ###
 
# ---- params ----
taxon_col <- "Genus"         # or "Lab_Family", "Genus"
min_samples_per_year <- 5        # min number of taxon sampled per year
long_term_min_years  <- 10        # min number of years sampled
restrict_long_term   <-T     # set TRUE to keep only long-term sites

# ---- effort summaries ----
effort_year <- inverts %>%
  group_by(Year) %>%
  summarise(n_samples = n_distinct(Sample_Code), .groups = "drop")

effort_site <- inverts %>%
  group_by(Location) %>%
  summarise(n_years = n_distinct(Year), .groups = "drop")

long_term_sites <- effort_site %>%
  filter(n_years >= long_term_min_years) %>%
  pull(Location)

# ---- optional site restriction ----
inverts_filt <- inverts %>%
  { if (restrict_long_term) filter(., Location %in% long_term_sites) else . } %>%
  mutate(Year = as.integer(Year))

# ---- per-sample totals per taxon ----
resp <- inverts_filt %>%
  group_by(Year, Sample_Code, .add = TRUE) %>%
  group_by(!!rlang::sym(taxon_col), .add = TRUE) %>%
  summarise(sample_total = sum(Count, na.rm = TRUE), .groups = "drop")

# ---- keep years with adequate samples ----
years_keep <- resp %>%
  distinct(Year, Sample_Code) %>%
  count(Year, name = "n_samples") %>%
  filter(n_samples >= min_samples_per_year) %>%
  pull(Year)

resp <- resp %>% filter(Year %in% years_keep)

# ---- yearly summaries (median + IQR) ----
summ <- resp %>%
  group_by(Year, !!rlang::sym(taxon_col)) %>%
  summarise(
    median_per_sample = median(sample_total, na.rm = TRUE),
    q25 = quantile(sample_total, 0.25, na.rm = TRUE),
    q75 = quantile(sample_total, 0.75, na.rm = TRUE),
    n   = n(),
    .groups = "drop"
  )

# ---- choose a subset of taxa (top 20 by overall median) ----
top_taxa <- summ %>%
  group_by(!!rlang::sym(taxon_col)) %>%
  summarise(overall_med = median(median_per_sample, na.rm = TRUE), .groups = "drop") %>%
  slice_max(overall_med, n = 20) %>%
  pull(!!rlang::sym(taxon_col))

summ_top <- summ %>% filter(!!rlang::sym(taxon_col) %in% top_taxa)

# ---- plot (log y, 5-year x breaks, clean panel border, title) ----
p = ggplot(
  summ_top,
  aes(x = Year, y = median_per_sample,
      color = !!rlang::sym(taxon_col),
      group = !!rlang::sym(taxon_col))
) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5), limits = c(1995, 2020)) +
  labs(x = "Year", y = "Median per-sample Genus Abundance", color = taxon_col) +
  theme_plot()+
  scale_color_viridis_d(option = "turbo") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "right"
  ) 
p



pdf("~/Downloads/invert_family_abundance.pdf")
p
dev.off()


ggplot(
  summ_top,
  aes(x = Year, y = median_per_sample,
      color = !!rlang::sym(taxon_col),
      group = !!rlang::sym(taxon_col))
) +
  stat_smooth(method = "lm", se = F) +
  geom_point() +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5), limits = c(1995, 2020)) +
  labs(x = "Year", y = "Median per-sample abundance", color = taxon_col, title = "Counts by Order") +
  theme_plot()+
  scale_color_viridis_d(option = "turbo") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "right"
  ) 







inverts %>%
  group_by(Sample_Code) %>%
  summarise(n_taxa = n(), total_count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(n_taxa))


# Focus on Abrams to explor
abrams = inverts %>% filter(Location == "Abrams Creek")

ggplot(abrams, aes(x = Start_Date, y = Count,
                    color = Genus,
                    group = Genus)) +
  geom_line(aes(color = Genus)) +
  geom_point(aes(color = Genus)) +
  theme_plot()+
  theme(legend.position = "none") +
  scale_y_log10() +
  labs(x = "Date", y = "Count", color = "Genus")

abrams_family <- abrams %>%
  group_by(Lab_Family, Year) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(abrams_family, aes(x = Year, y = total_count, color = Lab_Family, group = Lab_Family)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_y_log10() +
  labs(x = "Year", y = "Total Count", color = "Family") +
  theme(legend.position = "none") 
unique(abrams_family$Lab_Family)

abrams_order <- abrams %>%
  group_by(Lab_Order, Year) %>%
  summarise(total_count = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(abrams_order, aes(x = Year, y = total_count, color = Lab_Order, group = Lab_Order)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_y_log10() +
  labs(x = "Year", y = "Total Count", color = "Order") +
  theme(legend.position = "none") 
unique(abrams_family$Lab_Family)
unique(abrams_order$Lab_Order)

# pick top 20 orders by total abundance
top20_orders <- abrams_order %>%
  filter(!is.na(Lab_Order)) %>%
  group_by(Lab_Order) %>%
  summarise(total = sum(total_count, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total, n = 20) %>%
  pull(Lab_Order)

# filter and plot
abrams_order %>%
  filter(Lab_Order %in% top20_orders) %>%
  ggplot(aes(x = Year, y = total_count, color = Lab_Order, group = Lab_Order)) +
  geom_line() +
  geom_point() +
  scale_y_log10(name = "Total Count") +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5), limits = c(1995, 2020)) +
  theme_plot()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "right") + 
  labs(title = "Counts by Order") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, color = "black"))+
  scale_color_viridis_d(option = "turbo")

#Smooth vit
(p = abrams_order %>%
  filter(Lab_Order %in% top20_orders) %>%
  ggplot(aes(x = Year, y = total_count, color = Lab_Order, group = Lab_Order)) +
  geom_point() +
  geom_smooth(se = F) +
  scale_y_log10(breaks = c(1, 10, 100, 1000)) +
  theme_plot()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Year", y = "Total Count", color = "Order") +
  theme(legend.position = "none") + 
  scale_color_viridis_d(option = "turbo"))

p +
  geom_smooth(
    data = abrams_order %>% dplyr::filter(!is.na(Lab_Order)),
    aes(x = as.integer(Year), y = total_count, group = 1),
    inherit.aes = FALSE,
    method = "gam",
    formula = y ~ s(x, k = 5),   # small k = smoother, less edge wobble
    se = FALSE,
    color = "black",
    linewidth = 0.8
  ) +
  scale_x_continuous(
    limits = range(as.integer(abrams_order$Year), na.rm = TRUE),
    expand = expansion(mult = c(0, 0.02))  # minimal padding; prevents “into oblivion”
  )



abrams %>%
  group_by(Lab_Order) %>%
  summarise(n_records = n(),
            total_count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(n_records)) %>%
  print(n =38)

str(abrams)
unique(abrams$Site)