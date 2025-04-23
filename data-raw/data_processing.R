# this is where we will aggregate all layers into a consistent format and
# do the geoprocessing necessary for the NF and HUCs

library(tidyverse)
library(sf)

# load in datasets --------------------------------------------------------
# opening boundaries
nf_boundaries <- readRDS(here::here("data", "nf_boundaries.RDS")) |>
  st_transform(4326) |>
  st_make_valid()
watersheds <- readRDS(here::here("data", "watersheds.RDS")) |>
  st_transform(4326) |>
  st_make_valid()

# keeping only the name field for nf
nf_keys <- nf_boundaries[, c("name")]

watersheds <- watersheds |> # renaming to differentiate
  rename(watershed_name = name)

nf_boundaries <- nf_boundaries |> # renaming to differentiate
  rename(nf_name = name)

# hydropower
hydropower <- readRDS('data-raw/hydropower_clean.RDS') |>
  select(beneficiary_type, entity_name, entity_address, quantity_metric, quantity_unit, national_forest_connection, latitude, longitude, geometry) |>
   st_transform(4326)

## cvp ----
cvp <- readRDS(here::here("data", "cvp.RDS"))

cvp_centroids <- st_centroid(cvp)
cvp_coords <- st_coordinates(cvp_centroids)
cvp$longitude <- cvp_coords[,1]
cvp$latitude <- cvp_coords[,2]

# cross-checked quantity_metric == NA data entry with raw data - quantity entry
# is already accounted for (Pajaro Valley WMA, Westlands WD) in Pajaro Valley WMA, Santa Clara VWDv - therefore, we are removing it
cvp_raw <- cvp |>
  st_set_crs(4326) |>
  st_transform(4326) |>
  st_make_valid() |>
  select(cvp_unit_name, beneficiary_type, entity_name, entity_address, entity_contact, quantity_metric, quantity_unit,
         national_forest_connection, latitude, longitude, geometry) |>
  filter(!is.na(quantity_metric)) |>
  glimpse()

cvp_watershed <- st_join(cvp_raw, watersheds, left = TRUE) |> glimpse()

cvp_lookup <- tibble::tribble(
  ~cvp_unit_name, ~watershed_name,
  "Shasta Dam & Reservoir", "Upper Sacramento",
  "Black Butte Dam & Reservoir", "Upper Sacramento",
  "Tehama-Colusa Canal", "Upper Sacramento",
  "Corning Canal", "Upper Sacramento",
  "Colusa Basin Drain", "Lower Sacramento",
  "Folsom Dam & Reservoir", "Lower Sacramento",
  "Folsom-South Canal", "Lower Sacramento",
  "Upper American River", "Lower Sacramento",
  "Delta-Mendota Canal", "Upper Sacramento",
  "Delta-Mendota Canal",  "Klamath",
  "Contra Costa Canal", "Upper Sacramento",
  "Contra Costa Canal",  "Klamath"
  # Only including facilities that map to your 4 target basins
)


cvp_watershed_clean <- cvp_watershed |>
  left_join(cvp_lookup, by = "cvp_unit_name", suffix = c("", "_lookup")) |>
  mutate(watershed_name = case_when(!is.na(watershed_name) ~ watershed_name,
                                    is.na(watershed_name) & !is.na(watershed_name_lookup) ~ watershed_name_lookup,  # use lookup
                                    TRUE ~ NA_character_)
         ) |>
  select(beneficiary_type, entity_name, entity_address, entity_contact, quantity_metric, quantity_unit,
         national_forest_connection, watershed_name, latitude, longitude, geometry) |>
  glimpse()

cvp_centroids <- st_centroid(cvp_watershed_clean)
cvp_coords <- st_coordinates(cvp_centroids)
cvp_watershed_clean$longitude <- cvp_coords[,1]
cvp_watershed_clean$latitude <- cvp_coords[,2]
# the remainin NA's are related to lower basins like san joaquin and tulare - will add nf relationship to all
# tried adding relationshp to all basins but data becomes significantly larger, it will probably be best to
# add nf relationship manually. See code below

# cvp_watershed_clean <- cvp_watershed_clean |>
#   mutate(watershed_name = if_else(
#     is.na(watershed_name),
#     list(c("Lower Sacramento", "Upper Sacramento", "Northern California Coastal", "Klamath")),
#     list(watershed_name))) |>
#   tidyr::unnest_longer(watershed_name)

## swp ----
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


# water rights
water_rights <- readRDS('data-raw/water_rights_clean.RDS') |>
  mutate(beneficiary_type = "water right owner") |>
  st_transform(4326)

water_rights <- st_make_valid(water_rights)



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
all_datasets_raw <- bind_rows(hydropower, swp, dws, water_rights) |>
  st_make_valid()

# processing to find NF relationship --------------------------------------

# assign overlap watershed to datasets
all_datasets_watershed <- st_join(all_datasets_raw, watersheds, left = TRUE) |> glimpse()

# bind cvp
all_datasets_with_ws <- bind_rows(all_datasets_watershed, cvp_watershed_clean)

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
all_datasets_results_raw <- left_join(all_datasets_with_ws, watershed_nf, by = "watershed_name") |>
  select(-national_forest_connection) |>
  rename(national_forest_connection = nf_name) |>
  glimpse()
# fixing swp manually

all_datasets_results <- all_datasets_results_raw |>
  mutate(national_forest_connection =
           case_when(is.na(national_forest_connection) & beneficiary_type == "swp contractor" ~ "Lassen National Forest",
                     T ~ national_forest_connection))

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

# saveRDS(all_datasets_raw, here::here("data", "all_datasets.RDS"))

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


# part 2 ------
#qc'ing SWP - TODO this are the records that need separate adjustment, the the and criteria needed
all_datasets_results |>
  filter(beneficiary_type == "swp contractor" & is.na(national_forest_connection)) |>
  mutate(national_forest_connection =
           case_when(is.na(national_forest_connection) & beneficiary_type == "swp contractor" ~ "Lassen National Forest",
         T ~ national_forest_connection)) |>
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) |>
  leaflet() |>
  addTiles() |>
  addPolygons(color = "orange",
              weight = 1,
              fillOpacity = 0.5,
              popup = ~paste(
                "<strong>Beneficiary Type:</strong>", beneficiary_type,
                "<br><strong>Entity Name:</strong>", entity_name,
                "<br><strong>Watershed:</strong>", watershed_name,
                "<br><strong>National Forest:</strong>", national_forest_connection))
# qc' in CVP
# TODO this are the records that need separate adjustment, use lucid to relate cvp_unit_name to watershed
all_datasets_results |>
  filter(beneficiary_type == "cvp contractor" & is.na(national_forest_connection)) |>
  distinct(entity_name) |>
  view()
  # filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) |>
  # leaflet() |>
  # addTiles() |>
  # addPolygons(color = "purple",
  #             weight = 1,
  #             fillOpacity = 0.5,
  #             popup = ~paste(
  #               "<strong>Beneficiary Type:</strong>", beneficiary_type,
  #               "<br><strong>Entity Name:</strong>", entity_name,
  #               "<br><strong>Watershed:</strong>", watershed_name,
  #               "<br><strong>National Forest:</strong>", national_forest_connection))

