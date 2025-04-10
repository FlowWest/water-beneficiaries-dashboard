# data processing of the NF and HUC layers

library(sf)
library(tidyverse)
library(leaflet)


# national forests --------------------------------------------------------

nf_raw <- read_sf(here::here('data-raw', 'shapefiles', 'nf_boundaries', 'S_USA.NFSLandUnit.shp')) |>
  janitor::clean_names() |>
  filter(nfslandu_1 == "National Forest") |>
  glimpse()

unique(nf_bounds$nfslandu_2)

names <- c('Angeles National Forest',
           'Cleveland National Forest',
           'Eldorado National Forest',
           'Inyo National Forest',
           'Klamath National Forest',
           'Tahoe National Forest',
           'Lassen National Forest',
           'Los Padres National Forest',
           'Mendocino National Forest',
           'Plumas National Forest',
           'San Bernardino National Forest',
           'Sequoia National Forest',
           'Shasta National Forest',
           'Trinity National Forest',
           'Sierra National Forest',
           'Modoc National Forest',
           'Six Rivers National Forest',
           'Stanislaus National Forest')

nf_bounds <- nf_raw |>
  filter(nfslandu_2 %in% names) |>
  select(name = nfslandu_2, hq_location = hq_locatio, geometry) |>
  mutate(name = ifelse(name == "Shasta National Forest", "Shasta-Trinity National Forest", name)) |>
  st_transform("+proj=longlat +datum=WGS84") |>
  glimpse()

leaflet() |>
  addTiles() |>
  addPolygons(data = nf_bounds,
              popup = ~name)

saveRDS(nf_bounds, here::here('data', 'nf_boundaries.RDS'))


# watersheds --------------------------------------------------------------
# NOTE: large geodatabases for CV and Klamath are currently stored on egnyte and the following file paths will need
# to be updated to accomodate a different computer
#
# NOTE: we chose to use HUC6 but could adapt to HUC8 if a smaller area is desired from client
klamath_gdb <- '../../../Egnyte - flowwest/Shared/Active_Projects/116-01_Water_Beneficiary_Database/Analysis/data-raw/nhdplus/klamath/NHDPLUS_H_1801_HU4_GDB/NHDPLUS_H_1801_HU4_GDB.gdb/'
st_layers(klamath_gdb)
huc6_raw_klamath <- st_read(klamath_gdb, layer = "WBDHU6")

cv_gdb <- '../../../Egnyte - flowwest/Shared/Active_Projects/116-01_Water_Beneficiary_Database/Analysis/data-raw/nhdplus/central_valley/NHDPLUS_H_1802_HU4_GPKG/NHDPLUS_H_1802_HU4_GPKG.gpkg'
st_layers(cv_gdb)
huc6_raw_cv <- st_read(cv_gdb, layer = "WBDHU6")

huc6_klamath <- huc6_raw_klamath |>
  janitor::clean_names() |>
  select(name, huc6, geometry = Shape) |>
  st_transform(crs = "+proj=longlat +datum=WGS84") |>
  st_zm()

huc6_cv <- huc6_raw_cv |>
  janitor::clean_names() |>
  select(name, huc6, geometry = Shape) |>
  st_transform(crs = "+proj=longlat +datum=WGS84") |>
  st_zm() |>
  glimpse()

huc6 <- bind_rows(huc6_cv,
                  huc6_klamath)

saveRDS(huc6, here::here("data", "watersheds.RDS"))


# testing ways to find the overlap to assign NF to a beneficiary

nf_boundaries <- readRDS(here::here("data", "nf_boundaries.RDS")) |>
  st_transform(4326) |>
  st_make_valid()
watersheds <- readRDS(here::here("data", "watersheds.RDS")) |>
  st_transform(4326) |>
  st_make_valid()
all_datasets <- readRDS(here::here("data", "all_datasets.RDS")) |>
  st_make_valid()

# approach 1

# all_datasets_with_watershed <- st_join(all_datasets, watersheds, left = FALSE) |>
#   glimpse()
#
# watersheds_with_nf <- st_join(watersheds, nf_boundaries, left = FALSE)
#
# all_data_full <- st_join(all_datasets_with_watershed, watersheds_with_nf, left = TRUE)

# approach 2

#joining all datasets with watersheds
all_datasets_watershed <- st_join(all_datasets, watersheds, left = TRUE) |> glimpse()

# keeping only the name field for nf
nf_keys <- nf_boundaries[, c("name")]

# joining dataset with watershed and nf names, based on geometry
watershed_nf_lookup <- st_join(watersheds, nf_keys, left = TRUE)

watershed_nf <- watershed_nf_lookup |>
  st_drop_geometry() |>
  distinct(huc6, .keep_all = TRUE)  # avoiding duplicates

all_datasets_results <- left_join(all_datasets_watershed, watershed_nf, by = "huc6") |>
  st_transform(4326) |>
  rename(watershed = name.x,
         nf = name.y) |>
  glimpse()

# split points and polygons for plotting purposes

geom_types <- st_geometry_type(all_datasets_results)

# Separate point and polygon features
result_points <- all_datasets_results[geom_types %in% c("POINT", "MULTIPOINT"), ]
result_polygons <- all_datasets_results[geom_types %in% c("POLYGON", "MULTIPOLYGON"), ]


# plotting both nf and watersheds, plus all_datasets
leaflet() |>
  addTiles() |>
  addPolygons(data = nf_boundaries,
              group = "National Forests",
              color = "blue",
              weight = 1,
              fillOpacity = 0.3,
              popup = ~name) |>
  addPolygons(data = watersheds,
              group = "Watersheds",
              color = "green",
              weight = 1,
              fillOpacity = 0.3,
              popup = ~name) |>
  # point beneficiaries
  addCircleMarkers(data = result_points,
                   group = "Beneficiaries (Points)",
                   radius = 5,
                   color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.7,
                   popup = ~paste(
                     "<strong>Beneficiary Type:</strong>", beneficiary_type,
                     "<br><strong>Watershed:</strong>", watershed,
                     "<br><strong>National Forest:</strong>", nf)) |>
  # polygon-based beneficiaries
  addPolygons(data = result_polygons,
              group = "Beneficiaries (Polygons)",
              color = "orange",
              weight = 1,
              fillOpacity = 0.5,
              popup = ~paste(
                "<strong>Beneficiary Type:</strong>", beneficiary_type,
                "<br><strong>Watershed:</strong>", watershed,
                "<br><strong>National Forest:</strong>", nf)) |>
  addLayersControl(
    overlayGroups = c("National Forests", "Watersheds",
                      "Beneficiaries (Points)", "Beneficiaries (Polygons)"),
    options = layersControlOptions(collapsed = FALSE))

