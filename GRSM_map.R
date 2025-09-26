
library(tidyverse)
library(sf)
grsm_locations = read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/Shared drives/GRSM_CESU/Old/Data/MacroInvertebrates/Locations.csv')

watershed = st_read("/Users/jgradym/Downloads/GRSM_WATERSHEDS.geojson")[2]
watershed = st_read("/Users/jgradym/Downloads/GRSM_WATERSHEDS/GRSM_WATERSHEDS.shp")[2]
str(watershed)
plot(watershed)


# start from your 'watershed' object
ws <- watershed

# dissolve all polygons into one (kills inner boundaries)
watershed2 <- ws |>
  st_make_valid() |>
  st_union() |>
  st_cast("MULTIPOLYGON")

# plot: outline only
plot(st_geometry(watershed2), col = NA, border = "black", lwd = 2, axes = TRUE)

# overlay your LAT/LON points
pts_sf <- st_as_sf(
  subset(transform(grsm_locations,
                   LAT = suppressWarnings(as.numeric(LAT)),
                   LON = suppressWarnings(as.numeric(LON))),
         is.finite(LAT) & is.finite(LON)),
  coords = c("LON","LAT"), crs = 4326, remove = FALSE
)
plot(st_geometry(pts_sf), add = TRUE, pch = 16, cex = 0.8, col = "#1f78b4")


# --- 0) pick the watershed layer to draw (use 'watershed2' if you made the dissolved one) ---
ws <- if (exists("watershed2")) watershed2 else watershed
if (!is.na(sf::st_crs(ws)) && sf::st_crs(ws)$epsg != 4326) {
  ws <- sf::st_transform(ws, 4326)
}

# --- 1) clean LAT/LON ---
pts <- subset(
  transform(grsm_locations,
            LAT = suppressWarnings(as.numeric(LAT)),
            LON = suppressWarnings(as.numeric(LON))),
  is.finite(LAT) & is.finite(LON) & !is.na(StreamName) & nzchar(StreamName)
)

# ===== Option A: ONE point per stream = first occurrence (simplest) =====
pts_first <- pts[!duplicated(pts$StreamName), ]
pts_first_sf <- sf::st_as_sf(pts_first, coords = c("LON","LAT"), crs = 4326, remove = FALSE)

# draw
pdf("~/Downloads/GRSM_map2.pdf")
plot(sf::st_geometry(ws), col = NA, border = "black", lwd = 1, axes = TRUE)
plot(sf::st_geometry(pts_first_sf), add = TRUE, pch = 16, cex = 1, col = "#1f78b4")
text(pts_first$LON, pts_first$LAT, labels = pts_first$StreamName, pos = 4, cex = 0.5)
dev.off()
# # (optional) labels:
# 

neon_field = st_read("~/Downloads/NEON_Field_Sites/NEON_Field_Sites_v17.shp")
neon_aquatic = neon_field %>% filter(siteType == "Core Aquatic")
plot(neon_aquatic[3])

# ===== Option B: ONE point per stream = centroid of that stream's sites (use if you prefer) =====
# by_mean <- aggregate(cbind(LON, LAT) ~ StreamName, data = pts, FUN = mean)
# pts_cent_sf <- sf::st_as_sf(by_mean, coords = c("LON","LAT"), crs = 4326, remove = FALSE)
# plot(sf::st_geometry(ws), col = NA, border = "black", lwd = 1.8, axes = TRUE)
# plot(sf::st_geometry(pts_cent_sf), add = TRUE, pch = 16, cex = 0.9, col = "#1f78b4")
# # text(by_mean$LON, by_mean$LAT, labels = by_mean$StreamName, pos = 4, cex = 0.7)
