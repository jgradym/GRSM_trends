library(tidyverse)
library(sf)
library(janitor)
library(geojsonsf)


three_pass_locations = read_csv("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Aquatics_Fish/Three_Pass/Locations/GRSM_THREE_PASS.csv")
inverts = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Aquatics_Macroinverts/SummaryData/Specimen_Data_Export.csv')
invert_locations = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Aquatics_Macroinverts/Documents/Locations.csv')
forest_locations = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Forest_Health/Locations.csv')
soil_noland  = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Soil_Quality/Soil_water_noland_divide_meta.csv') %>% janitor::clean_names()
noland_station = unique(soil_noland$station_id)

#NPS IRMA DataStore - Noland Divide Watershed: Site Metadata (IRMA ID 705202)
noland_meta <- read_csv("https://irma.nps.gov/DataStore/DownloadFile/705202?Reference=2304536") %>% clean_names()
noland_soil_coords <- noland_meta%>%
  transmute(
    station_id = location_id,
    latitude,
    longitude,
    datum = lat_lon_datum
  ) %>%
  distinct()

#parkwide soils
soils = read_csv("~/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Soil_Quality/Soils.csv")
soils_loc = soils %>% pull(LOC_NAME) %>% unique(.)

# Define the ArcGIS REST endpoint for GRSM Veg & Soils plots
endpoint <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/ArcGIS/rest/services/GRSM_VEG_SOIL_PLOTS_VS/FeatureServer/0/query"

# Build a full query URL:
#   where=1=1   → return all features
#   outFields=… → include these non-geometry fields
#   returnGeometry=true → include spatial coordinates
#   outSR=4326  → output in WGS84 (lon/lat)
#   f=geojson   → return GeoJSON, which sf::st_read understands directly
u <- paste0(endpoint,
            "?where=1%3D1&outFields=LOC_NAME,PANEL,VS_WATERSHED,GRTS_SITE",
            "&returnGeometry=true&outSR=4326&f=geojson")

# Read the GeoJSON directly into an sf object (each point = a Veg&Soils plot)
pts <- st_read(u, quiet = TRUE)

# Keep only rows matching your soil dataset’s LOC_NAMEs, then extract lon/lat
soil_coordinates <- pts %>%
  filter(LOC_NAME %in% soils_loc) %>%                  # subset to your sites
  mutate(lon = st_coordinates(geometry)[,1],           # pull X coordinate (longitude)
         lat = st_coordinates(geometry)[,2]) %>%       # pull Y coordinate (latitude)
  st_drop_geometry() %>%                               # drop sf geometry column
  select(LOC_NAME, lon, lat, PANEL, VS_WATERSHED, GRTS_SITE)  # keep useful fields

# Re-join coordinates back to your original list to preserve duplicates and order
soil_coords_full <- tibble(LOC_NAME = soils_loc) %>%
  left_join(soil_coordinates, by = "LOC_NAME")


# --------- Combine ------
str(invert_locations)
str(three_pass_locations)
str(forest_locations)
str(soil_coordinates)
str(noland_soil_coords)




# ---- 1) Standardize each source to: dataset, site_id, display_name, lon, lat, datum ----

# parkwide soils (from your pts→soil_coordinates step)
soils_parkwide_sites <- soil_coordinates %>%
  transmute(dataset = "soils_parkwide",
            site_id = LOC_NAME,
            display_name = LOC_NAME,
            lon, lat,
            datum = "WGS84")  # from outSR=4326

# noland divide (IRMA 705202)
noland_sites <- noland_soil_coords %>%
  transmute(dataset = "soils_noland",
            site_id = station_id,
            display_name = station_id,
            lon = longitude, lat = latitude,
            datum = datum) %>%
  distinct()

# macroinverts
invert_sites <- invert_locations %>%
  clean_names() %>%
  transmute(dataset = "inverts",
            site_id = station_name,                    # code like ABAB01I&M
            display_name = loc_name,                   # "Abrams Creek, Site 1"
            lon = lon, lat = lat,
            datum = datum)

# three-pass fish
fish_sites <- three_pass_locations %>%
  clean_names() %>%
  transmute(dataset = "fish_threepass",
            site_id = station_name,                    # e.g., "ABC-1"
            display_name = paste0(park_pref_name, ", ", section),
            lon = lon, lat = lat,
            datum = "NAD83")                           # coords appear NAD83 in GRSM exports

# forests / veg plots (park-wide tree plots)
forest_sites <- forest_locations %>%
  clean_names() %>%
  transmute(dataset = "forests",
            site_id = loc_name,                        # e.g., VSX001
            display_name = loc_name,
            lon = lon, lat = lat,
            datum = datum)

# ---- 2) (Optional but safer) Harmonize datums → WGS84 (EPSG:4326) ----
to_wgs84 <- function(df) {
  # split by datum, assign CRS, transform, and drop geometry
  wgs <- df %>% filter(datum %in% c("WGS84","WGS 84","WGS_84"))
  nad <- df %>% filter(datum %in% c("NAD83","NAD 83","NAD_83"))
  other <- df %>% filter(!datum %in% c("WGS84","WGS 84","WGS_84","NAD83","NAD 83","NAD_83"))
  
  wgs_out <- if (nrow(wgs)) {
    st_as_sf(wgs, coords = c("lon","lat"), crs = 4326, remove = FALSE) %>%
      st_drop_geometry()
  } else wgs
  
  nad_out <- if (nrow(nad)) {
    st_as_sf(nad, coords = c("lon","lat"), crs = 4269, remove = FALSE) %>%  # NAD83
      st_transform(4326) %>%
      mutate(lon = st_coordinates(geometry)[,1],
             lat = st_coordinates(geometry)[,2]) %>%
      st_drop_geometry()
  } else nad
  
  bind_rows(wgs_out, nad_out, other)
}

soils_parkwide_sites <- to_wgs84(soils_parkwide_sites)
noland_sites         <- to_wgs84(noland_sites)
invert_sites         <- to_wgs84(invert_sites)
fish_sites           <- to_wgs84(fish_sites)
forest_sites         <- to_wgs84(forest_sites)

# ---- 3) Combine everything ----
sites_all <- bind_rows(
  soils_parkwide_sites,
  noland_sites,
  invert_sites,
  fish_sites,
  forest_sites
) %>%
  distinct(dataset, site_id, .keep_all = TRUE)

# Optional: sf version for plotting / distance
sites_sf <- st_as_sf(sites_all, coords = c("lon","lat"), crs = 4326)

# Quick sanity check: counts by dataset
sites_all %>% count(dataset)


#-----------------------#

watershed = st_read('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Locations/GRSM_WATERSHEDS/GRSM_WATERSHEDS.shp')[2]
plot(watershed)


# dissolve all polygons into one (kills inner boundaries)
watershed_outline <-watershed |>
  st_make_valid() |>
  st_union() |>
  st_cast("MULTIPOLYGON")

# plot: outline only
plot(st_geometry(watershed_outline), col = NA, border = "black", lwd = 2, axes = TRUE)

# convert sites to sf if not already
sites_sf <- st_as_sf(sites_all, coords = c("lon", "lat"), crs = 4326)

p = ggplot() +
  # watershed polygons: white fill, black outlines
  geom_sf(data = watershed, fill = "white", color = "black", linewidth = 0.4) +
  
  # sampling points: hollow symbols (shapes 0–14 are hollow; outline color carries dataset)
  geom_sf(
    data = sites_sf,
    aes(color = dataset, shape = dataset),
    size = 2,
    stroke = 1.0      # thicker outline so hollow symbols pop
  ) +
  
  # consistent colors (tweak as you like)
  scale_color_manual(values = c(
    "fish_threepass" = "#377eb8",
    "inverts"        = "#4daf4a",
    "forests"        = "#984ea3",
    "soils_parkwide" = "orange2",
    "soils_noland"   = "firebrick"
  )) +


  # hollow point shapes (no fill used)
  # 1 = hollow circle, 0 = hollow square, 2 = hollow triangle, 5 = hollow diamond, 6 = hollow triangle-down
  scale_shape_manual(values = c(
    "fish_threepass" = 1,
    "inverts"        = 0,
    "forests"        = 2,
    "soils_parkwide" = 5,
    "soils_noland"   = 6
  )) +
  
  # Put the legend INSIDE the plot (top-left here) with a semi-transparent white box
  theme_minimal() +
  coord_sf() +
  labs(
    title = "GRSM Sampling Locations by Dataset",
    x = "Longitude", y = "Latitude",
    color = "Dataset", shape = "Dataset"
  ) +
  theme(
    legend.position.inside = c(0.02, 0.98),              # inset: (x, y) in NPC coords
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = scales::alpha("white", 0.7), color = "grey40"),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.2)
  ) +
  # make sure legend keys show hollow symbols (no fill)
  guides(
    color = guide_legend(override.aes = list(fill = NA, alpha = 1, size = 3, stroke = 1.2)),
    shape = guide_legend(override.aes = list(fill = NA, alpha = 1, size = 3, stroke = 1.2))
  )

p
# Add neon sites at GRSM

neon_map = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Locations/NEON-SiteMap-Table.csv') %>%
  clean_names()
str(neon_map)
neon_sf <- st_as_sf(neon_map, coords = c("longitude","latitude"), crs = 4326)
unique(neon_sf$site_code)

neon_sf <- neon_sf %>%
  mutate(feature_key = str_trim(feature_key),
         type = case_when(
           str_detect(feature_key, regex("Sediment|Fish|Riparian|Discharge|Gauge", ignore_case = TRUE)) ~ "neon_aquatic",
           TRUE ~ "neon_terrestrial"
         ))

# sanity checks
neon_sf %>% count(type)
neon_sf %>% count(type, feature_key) %>% arrange(type, desc(n))
ggplot() +
  # park border first
  geom_sf(data = watershed_outline, fill = "grey95", color = "black", linewidth = 0.6) +
  # watershed outlines
  geom_sf(data = watershed, fill = NA, color = "grey60", linewidth = 0.3) +
  # 1️⃣ terrestrial points first
  geom_sf(
    data = dplyr::filter(neon_sf, type == "neon_terrestrial"),
    aes(shape = feature_key, color = type),
    fill = NA, size = 2, stroke = .5
  ) +
  # 2️⃣ aquatic points second — these plot *on top*
  geom_sf(
    data = dplyr::filter(neon_sf, type == "neon_aquatic"),
    aes(shape = feature_key, color = type),
    fill = NA, size = 2, stroke = .5
  ) +
  scale_color_manual(
    values = c(neon_aquatic = "#00BFFF", neon_terrestrial = "#FF8C00"),
    name = "Environment"
  ) +
  scale_shape_manual(values = shape_map, name = "Feature type") +
  coord_sf() +
  theme_minimal() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = scales::alpha("white", 0.8), color = "grey40"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2)
  ) +
  labs(
    title = "NEON Field Installations within Great Smoky Mountains NP",
    subtitle = "Blue = aquatic (plotted on top), Brown = terrestrial; all symbols hollow and unique per feature type",
    x = "Longitude", y = "Latitude"
  )


# Combine maps


# --- 1) Classify NEON features (robustly) ---
neon_sf <- neon_sf %>%
  mutate(
    feature_key = str_squish(feature_key),
    type = case_when(
      feature_key %in% c("Sediment Point","Fish Point","Riparian Assessment",
                         "Discharge Point","Staff Gauge") ~ "neon_aquatic",
      TRUE ~ "neon_terrestrial"
    )
  )

# --- 2) Shape maps (hollow symbols) ---
grsm_shape_map <- c(
  "fish_threepass" = 1,
  "inverts"        = 0,
  "forests"        = 2,
  "soils_parkwide" = 5,
  "soils_noland"   = 6
)

neon_shape_map <- c(
  "Tower Location"             = 1,
  "Staff Gauge"                = 0,
  "Sensor Station"             = 2,
  "Meteorological Station"     = 5,
  "Megapit"                    = 6,
  "Sediment Point"             = 3,
  "Riparian Assessment"        = 4,
  "Fish Point"                 = 7,
  "Discharge Point"            = 8,
  "Benchmark"                  = 9,
  "Hut"                        = 10,
  "Distributed Base Plot"      = 11,
  "Distributed Mammal Grid"    = 12,
  "Distributed Mosquito Point" = 13,
  "Distributed Bird Grid"      = 14,
  "Distributed Tick Plot"      = 1
)

shape_map <- c(grsm_shape_map, neon_shape_map)

# --- 3) Unified color map: earthy GRSM + neon NEON ---
col_map <- c(
  "fish_threepass" = "#94644e",  # Smoky burnt sienna
  "inverts"        = "#5a8f5a",  # muted forest green
  "forests"        = "#7b659b",  # soft violet-gray
  "soils_parkwide" = "#b66a27",  # dull burnt orange
  "soils_noland"   = "#a43a32",  # clay red
  "neon_aquatic"        = "#00BFFF",  # bright neon blue
  "neon_terrestrial"    = "#FF8C00"   # bright neon orange
)

# --- 4) Plot ---
p = ggplot() +
  geom_sf(data = watershed_outline, fill = "grey95", color = "black", linewidth = 0.6) +
  geom_sf(data = watershed, fill = NA, color = "grey60", linewidth = 0.3) +
  
  # GRSM datasets
  geom_sf(
    data = sites_sf,
    aes(color = dataset, shape = dataset),
    fill = NA, size = 2.5, stroke = .75, alpha = 0.95
  ) +
  
  # NEON neon_terrestrial
  geom_sf(
    data = filter(neon_sf, type == "neon_terrestrial"),
    aes(color = type, shape = feature_key),
    fill = NA, size = 2.5, stroke = .75
  ) +
  
  # NEON aquatic (on top)
  geom_sf(
    data = filter(neon_sf, type == "neon_aquatic"),
    aes(color = type, shape = feature_key),
    fill = NA, size = 2.5, stroke = .75
  ) +
  
  scale_color_manual(values = col_map, name = "Group") +
  scale_shape_manual(values = shape_map, name = "Type") +
  coord_sf() +
  theme_minimal() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left","top"),
    legend.background = element_rect(fill = scales::alpha("white", 0.8), color = "grey40"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2)
  ) +
  labs(
    title = "GRSM Monitoring Sites and NEON Field Installations",
    subtitle = "Smoky datasets (muted earthy colors) with NEON terrestrial (orange) and aquatic (blue) overlays — all hollow symbols",
    x = "Longitude", y = "Latitude"
  )


pdf("~/Downloads/neon_and_grsm_sites.pdf")
p
dev.off()
