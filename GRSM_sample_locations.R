library(tidyverse)
library(sf)
library(janitor)


three_pass_locations = read_csv("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Aquatics_Fish/Three_Pass/Locations/GRSM_THREE_PASS.csv")
inverts = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Aquatics_Macroinverts/SummaryData/Specimen_Data_Export.csv')
invert_locations = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Aquatics_Macroinverts/Documents/Locations.csv')
forest_locations = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Forest_Health/Locations.csv')
soil_noland  = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Soil_Quality/Soil_water_noland_divide_meta.csv') %>% janitor::clean_names()
noland_station = unique(soil_noland$station_id)
#parkwide soils
soils = read_csv("~/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Maine/Data/Soil_Quality/Soils.csv")
soils_loc = soils %>% pull(LOC_NAME) %>% unique(.)


# ---- query in chunks and combine ----
chunk_size <- 50   
chunks <- split(soils_loc, ceiling(seq_along(soils_loc) / chunk_size))

sf_list <- map(chunks, ~ {
  u <- make_url(.x)
  tryCatch(
    st_read(u, quiet = TRUE),
    error = function(e) {
      message("Failed on chunk: ", paste(.x, collapse = ", "))
      st_sf(LOC_NAME = character(), geometry = st_sfc(), crs = 4326)
    }
  )
})

pts <- bind_rows(sf_list)

# ---- extract lon/lat, keep useful fields ----
coords <- st_coordinates(pts)
out_unique <- pts %>%
  st_drop_geometry() %>%
  mutate(lon = coords[, "X"], lat = coords[, "Y"]) %>%
  dplyr::select(LOC_NAME, lon, lat, PANEL, VS_WATERSHED, GRTS_SITE)

# ---- map back to original order (duplicates preserved) ----
out_full <- tibble(LOC_NAME = soils_loc) %>%
  left_join(out_unique, by = "LOC_NAME")

# ---- show quick summaries ----
# Unique table:
out_unique
# Full table in input order (with duplicates):
out_full

# ---- optional: write CSVs ----
# readr::write_csv(out_unique, "grsm_soil_plots_unique_lonlat.csv")
# readr::write_csv(out_full,  "grsm_soil_plots_inputorder_lonlat.csv")
