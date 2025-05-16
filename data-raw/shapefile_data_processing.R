# data processing of the NF and HUC layers

library(sf)
library(tidyverse)
library(leaflet)


# national forests --------------------------------------------------------

nf_raw <- read_sf(here::here('data-raw', 'shapefiles', 'nf_boundaries', 'S_USA.NFSLandUnit.shp')) |>
  janitor::clean_names() |>
  filter(nfslandu_1 == "National Forest") |>
  glimpse()

unique(nf_raw$nfslandu_2)

names <- c('Klamath National Forest',
           'Lassen National Forest',
           'Mendocino National Forest',
           'Shasta National Forest',
           'Modoc National Forest',
           'Six Rivers National Forest')


nf_bounds <- nf_raw |>
  filter(nfslandu_2 %in% names) |>
  select(name = nfslandu_2, hq_location = hq_locatio, geometry) |>
  st_transform("+proj=longlat +datum=WGS84") |>
  glimpse()

leaflet() |>
  addTiles() |>
  addPolygons(data = nf_bounds,
              popup = ~name)

saveRDS(nf_bounds, here::here('data', 'nf_boundaries.RDS'))

# USFS SOPA and CalVTP datasets
sopa_sf <- read_sf(here::here('data-raw', 'shapefiles', 'SOPA_Forest_Management_Scrape_Forest_HUC','SOPA_Forest_Management_Scrape_Forest_HUC.shp')) |>
  st_transform("+proj=longlat +datum=WGS84") |>
  glimpse()

table(sopa_sf$NFSLANDU_2)

leaflet() |>
  addTiles() |>
  addMarkers(data = sopa_sf)

# CalVTP
cal_vtp_raw <- read_sf(here::here('data-raw', 'shapefiles', 'CalVTP_ProposedProjects_Pnt_Scrape_Forest_HUC1','CalVTP_ProposedProjects_Pnt_Scrape_Forest_HUC1.shp')) |>
  st_transform("+proj=longlat +datum=WGS84") |>
  glimpse()

table(cal_vtp_raw$CalVTP_P_3)

leaflet() |>
  addTiles() |>
  addMarkers(data = cal_vtp_raw)

# CalVTP
usfs_nepa <- read_sf(here::here('data-raw', 'shapefiles', 'USFS_NEPA_Project_Areas_3857_Forest_HUC','USFS_NEPA_Project_Areas_3857_Forest_HUC.shp')) |>
  st_transform("+proj=longlat +datum=WGS84") |>
  glimpse()

table(usfs_nepa$NFSLANDU_2)

leaflet() |>
  addTiles() |>
  addPolygons(data = usfs_nepa)



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
