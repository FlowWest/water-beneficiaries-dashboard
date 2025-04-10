# this is where we will aggregate all layers into a consistent format and
# do the geoprocessing necessary for the NF and HUCs

library(tidyverse)
library(sf)


# load in datasets --------------------------------------------------------
hydropower <- readRDS('data-raw/hydropower_clean.RDS') |>
  select(beneficiary_type, entity_name, entity_address, quantity_metric, quantity_unit, national_forest_connection, latitude, longitude, geometry)

cvp <- readRDS(here::here("data", "cvp.RDS"))

cvp_centroids <- st_centroid(cvp)
cvp_coords <- st_coordinates(cvp_centroids)
cvp$longitude <- cvp_coords[,1]
cvp$latitude <- cvp_coords[,2]

cvp <- cvp |>
  st_set_crs(4326) |>              # Set the original CRS (if missing)
  st_transform(3857) |>            # Now transform to EPSG:3857
  select(beneficiary_type, entity_name, entity_address, quantity_metric, quantity_unit,
         national_forest_connection, latitude, longitude, geometry) |>
  glimpse()


# save dataset ------------------------------------------------------------

all_datasets <- bind_rows(hydropower, cvp)


# map datasets to NF and HUCs ---------------------------------------------
# TODO: explore ways of clipping the data to the HUCs

saveRDS(all_datasets, here::here("data", "all_datasets.RDS"))
