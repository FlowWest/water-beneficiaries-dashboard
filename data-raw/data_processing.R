# this is where we will aggregate all layers into a consistent format and
# do the geoprocessing necessary for the NF and HUCs

library(tidyverse)
library(sf)


# load in datasets --------------------------------------------------------

# hydropower
hydropower <- readRDS('data-raw/hydropower_clean.RDS') |>
  select(beneficiary_type, entity_name, entity_address, quantity_metric, quantity_unit, national_forest_connection, latitude, longitude, geometry) |>
   st_transform(4326)

# cvp
cvp <- readRDS(here::here("data", "cvp.RDS"))

cvp_centroids <- st_centroid(cvp)
cvp_coords <- st_coordinates(cvp_centroids)
cvp$longitude <- cvp_coords[,1]
cvp$latitude <- cvp_coords[,2]

# cross-checked quantity_metric == NA data entry with raw data - quantity entry
# is already accounted for (Pajaro Valley WMA, Westlands WD) in Pajaro Valley WMA, Santa Clara VWDv - therefore, we are removing it
cvp <- cvp |>
  st_set_crs(4326) |>
  st_transform(4326) |>
  select(beneficiary_type, entity_name, entity_address, entity_contact, quantity_metric, quantity_unit,
         national_forest_connection, latitude, longitude, geometry) |>
  filter(!is.na(quantity_metric)) |>
  glimpse()

# swp
swp <- readRDS(here::here("data", "swp.RDS"))
swp <- st_make_valid(swp)
swp_centroids <- st_centroid(swp)
swp_coords <- st_coordinates(swp_centroids)
swp$longitude <- swp_coords[,1]
swp$latitude <- swp_coords[,2]

is_polygon <- st_geometry_type(swp) %in% c("POLYGON", "MULTIPOLYGON")
polygons_centroid <- swp[is_polygon, ] |> st_centroid()
points <- swp[!is_polygon, ]


swp <- swp |>
  st_set_crs(4326) |>
  st_transform(4326) |>
  select(beneficiary_type, entity_name, entity_address, entity_contact, quantity_metric, quantity_unit,
         national_forest_connection, latitude, longitude, geometry)

# drinking water
dws <- readRDS(here::here("data", "drinking_water_boundaries.RDS"))
dws <- st_make_valid(dws)
dws_centroids <- st_centroid(dws)
dws_coords <- st_coordinates(dws_centroids)
dws$longitude <- dws_coords[,1]
dws$latitude <- dws_coords[,2]

dws <- dws |>
select(beneficiary_type, entity_name, entity_address, entity_contact, quantity_metric, quantity_unit,
       national_forest_connection, latitude, longitude, geometry)
# binding all datasets ----------------------------------------------------
all_datasets_raw <- bind_rows(hydropower, cvp, swp, dws) |>
  st_make_valid()

# processing to find NF relationship --------------------------------------
   # part 1 ------
# opening boundaries
nf_boundaries <- readRDS(here::here("data", "nf_boundaries.RDS")) |>
  st_transform(4326) |>
  st_make_valid()
watersheds <- readRDS(here::here("data", "watersheds.RDS")) |>
  st_transform(4326) |>
  st_make_valid()

#joining all datasets with watersheds
all_datasets_watershed <- st_join(all_datasets_raw, watersheds, left = TRUE) |> glimpse()

# keeping only the name field for nf
nf_keys <- nf_boundaries[, c("name")]

watersheds <- watersheds |> # renaming to differentiate
  rename(watershed_name = name)

nf_boundaries <- nf_boundaries |> # renaming to differentiate
  rename(nf_name = name)

# assign overlap watershed to datasets
all_datasets_with_ws <- st_join(all_datasets_raw, watersheds, left = TRUE)
# assign overlap nf to watersheds - this serves as a watershed–NF lookup and do spatial join
watershed_nf <- st_join(watersheds, nf_boundaries[, "nf_name"], left = TRUE) |>
  st_drop_geometry() |>
  distinct(watershed_name, nf_name) |>
  glimpse()

# keep this for now, may use for tabular data?
# watershed_nf_lookup <- watershed_nf_lookup |>
#   st_drop_geometry() |>
#   group_by(watershed_name) |>
#   summarise(nf = paste(unique(nf_name), collapse = ", ")) |>
#   glimpse()


# joining to find dataset-nf relationship, add geometry from original all_datasets
all_datasets_results <- left_join(all_datasets_with_ws, watershed_nf, by = "watershed_name") |>
  select(-national_forest_connection) |>
  rename(national_forest_connection = nf_name) |>
  glimpse()

#TODO I think all other contractors that do not relate spatially to any of the NF, they should b
# related to the NF that fall within the Lower Sacramento Watershed, since this is where the
# SWP comes from. Check if this is the case, or if any other watershed makes more sense
# for CVP

# splitting points and polygons for plotting purposes
geom_types <- st_geometry_type(all_datasets_results)
result_points <- all_datasets_results[geom_types %in% c("POINT", "MULTIPOINT"), ]
result_polygons <- all_datasets_results[geom_types %in% c("POLYGON", "MULTIPOLYGON"), ]

# save dataset ------------------------------------------------------------
saveRDS(all_datasets_results, here::here("data", "all_datasets_results.RDS"))


# map datasets to NF and HUCs ---------------------------------------------
# TODO: explore ways of clipping the data to the HUCs

saveRDS(all_datasets, here::here("data", "all_datasets.RDS"))

# PLOTS -------------------------------------------------------------------
# plotting both nf and watersheds, plus all_datasets to check accuracy---
leaflet() |>
  addTiles() |>
  addPolygons(data = nf_boundaries,
              group = "National Forests",
              color = "blue",
              weight = 1,
              fillOpacity = 0.3,
              popup = ~nf_name) |>
  addPolygons(data = watersheds,
              group = "Watersheds",
              color = "green",
              weight = 1,
              fillOpacity = 0.3,
              popup = ~watershed_name) |>
  # point beneficiaries
  addCircleMarkers(data = result_points,
                   group = "Beneficiaries (Points)",
                   radius = 5,
                   color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.7,
                   popup = ~paste(
                     "<strong>Entity Name:</strong>", entity_name,
                     "<br><strong>Beneficiary Type:</strong>", beneficiary_type,
                     "<br><strong>Watershed:</strong>", watershed_name,
                     "<br><strong>National Forest:</strong>", national_forest_connection)) |>
  # polygon-based beneficiaries
  addPolygons(data = result_polygons,
              group = "Beneficiaries (Polygons)",
              color = "orange",
              weight = 1,
              fillOpacity = 0.5,
              popup = ~paste(
                "<strong>Beneficiary Type:</strong>", beneficiary_type,
                "<br><strong>Entity Name:</strong>", entity_name,
                "<br><strong>Watershed:</strong>", watershed_name,
                "<br><strong>National Forest:</strong>", national_forest_connection)) |>
  addLayersControl(
    overlayGroups = c("National Forests", "Watersheds",
                      "Beneficiaries (Points)", "Beneficiaries (Polygons)"),
    options = layersControlOptions(collapsed = FALSE))

