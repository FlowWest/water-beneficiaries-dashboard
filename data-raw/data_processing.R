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

## hydropower ----
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
  "Mendota Pool", "Upper Sacramento",
  "Mendota Pool",  "Klamath",
  "Contra Costa Canal", "Upper Sacramento",
  "Contra Costa Canal",  "Klamath")  # Only including facilities that map to 4 target basins

cvp_watershed_clean <- cvp_watershed |>
  left_join(cvp_lookup, by = "cvp_unit_name", suffix = c("", "_lookup")) |>
  mutate(watershed_name = case_when(!is.na(watershed_name) ~ watershed_name,
                                    is.na(watershed_name) & !is.na(watershed_name_lookup) ~ watershed_name_lookup,  # use lookup
                                    TRUE ~ NA_character_))

cvp_watershed_clean |> filter(is.na(watershed_name)) |> view() # quick check for watershed NA's

cvp_watershed_clean <- cvp_watershed_clean |>
  select(cvp_unit_name, beneficiary_type, entity_name, entity_address, entity_contact, quantity_metric, quantity_unit,
         national_forest_connection, watershed_name, latitude, longitude, geometry) |>
  glimpse()

cvp_centroids <- st_centroid(cvp_watershed_clean)
cvp_coords <- st_coordinates(cvp_centroids)
cvp_watershed_clean$longitude <- cvp_coords[,1]
cvp_watershed_clean$latitude <- cvp_coords[,2]
# the remaining watershed NA's are related to lower basins like san joaquin and tulare, we will omit these
# for more details look at project Lucid

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

## water rights ----
water_rights <- readRDS('data-raw/water_rights_clean.RDS') |>
  mutate(beneficiary_type = "water right owner") |>
  st_transform(4326) |>
  rename(entity_contact = entitity_contact)

water_rights <- st_make_valid(water_rights)

## drinking water ----
dws_raw <- readRDS(here::here("data", "drinking_water_boundaries.RDS"))
dws_raw <- st_make_valid(dws_raw)
dws_centroids <- st_centroid(dws_raw)
dws_coords <- st_coordinates(dws_centroids)
dws_raw$longitude <- dws_coords[,1]
dws_raw$latitude <- dws_coords[,2]

dws_clean <- dws_raw |>
select(pwsid, beneficiary_type, entity_name, entity_address, entity_contact, quantity_metric, quantity_unit,
       national_forest_connection, latitude, longitude, geometry)
# reading excel to link PWS to the contracts (CVP, SWP CRA, AAC)
dws_contractor_type_raw <- readxl::read_excel("data-raw/drinking_water_systems/PWS_ID_Contractors.xlsx") |>
  janitor::clean_names()

dws_projects <- left_join(dws_clean, dws_contractor_type_raw) |>
  select(-pwsid, -district) |>
  glimpse()

# planned restoration
planned_restoration_raw <-readRDS(here::here("data-raw", "planned_restoration_combined.RDS")) |>
  rename(old_nf_conn = national_forest_connection,
         old_ws_name = watershed_name) |>
  #select(-national_forest_connection, -watershed_name) |> # removing SIG logic
  glimpse()

# binding all datasets to process -----------------------------------------
all_datasets_raw <- bind_rows(hydropower, swp, dws_projects, water_rights) |>
  st_make_valid()

# processing to find NF relationship --------------------------------------

## (1) assign watershed to datasets: if polygons/points fall within a watershed
all_datasets_watershed <- st_join(all_datasets_raw, watersheds, left = TRUE) |> glimpse()
planned_restoration_watershed <- st_join(planned_restoration_raw, watersheds, left = TRUE) |> glimpse()

# bind cvp, since processing was done above using cvp_units
all_datasets_with_ws <- bind_rows(all_datasets_watershed, cvp_watershed_clean)

## (2)  assign overlap nf to watersheds - this serves as a watershed–NF lookup and do spatial join
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


## (3) joining to find dataset-nf relationship, add geometry from original all_datasets
all_datasets_results_raw <- left_join(all_datasets_with_ws, watershed_nf, by = "watershed_name") |>
  select(-national_forest_connection) |>
  rename(national_forest_connection = nf_name) |>
  glimpse()

planned_restoration_results <- left_join(planned_restoration_watershed, watershed_nf, by = "watershed_name") |>
  rename(national_forest_connection = nf_name) |>
  glimpse()

##### documentation of very different mapping to national forest boundaries between flowwest and SIG
planned_restoration_results |> select(old_nf_conn, national_forest_connection, old_ws_name, watershed_name) |> View()

planned_restoration_results_clean <- planned_restoration_results |>
  select(-old_nf_conn, -old_ws_name) |>
  filter(!is.na(national_forest_connection))


## Additional processing for drinking water systems and for those SWP contracts that do not fall within a watershed,
# we will assume that they connect to Lassen NF
# reference table for dws that relate to cvp project
dws_cvp_forests <- c("Shasta National Forest", "Lassen National Forest",
                 "Klamath National Forest", "Six Rivers National Forest")

## (4) additional manual processing
# nf's that will be related to dw systems (those that have cvp project relationship)
dws_cvp_forests <- c("Shasta National Forest", "Lassen National Forest",
                     "Klamath National Forest", "Six Rivers National Forest")

dws_cvp_expand_rows <- all_datasets_results_raw |>
  filter(is.na(national_forest_connection),
         beneficiary_type == "drinking water system",
         project == "Central Valley Project")

n_dws_cvp <- nrow(dws_cvp_expand_rows)

# expanding rows
dws_cvp_expanded <- dws_cvp_expand_rows |>
  slice(rep(1:n_dws_cvp, each = length(dws_cvp_forests))) |>
  mutate(national_forest_connection = rep(dws_cvp_forests, times = n_dws_cvp))

fallback_assigned <- all_datasets_results_raw |>
  filter(is.na(national_forest_connection),
         beneficiary_type == "swp contractor" |
           (beneficiary_type == "drinking water system" & project == "State Water Project")) |>
  mutate(national_forest_connection = "Lassen National Forest")

# keeping remaining rows
remaining <- all_datasets_results_raw |>
  filter(!(is.na(national_forest_connection) &
             (beneficiary_type == "swp contractor" |
                (beneficiary_type == "drinking water system" & project %in% c("State Water Project", "Central Valley Project")))))

# combining all results
all_datasets_results <- bind_rows(remaining, fallback_assigned, dws_cvp_expanded) |>
  select(beneficiary_type, entity_name, entity_contact, entity_address, quantity_metric, quantity_unit,
         latitude, longitude, watershed_name, huc6, national_forest_connection)


# splitting points and polygons for plotting purposes
geom_types <- st_geometry_type(all_datasets_results)
result_points <- all_datasets_results[geom_types %in% c("POINT", "MULTIPOINT"), ]
result_polygons <- all_datasets_results[geom_types %in% c("POLYGON", "MULTIPOLYGON"), ]

# save dataset ------------------------------------------------------------
saveRDS(all_datasets_results, here::here("data", "all_datasets_results.RDS"))
saveRDS(planned_restoration_results_clean, here::here("data", "planned_restoration.RDS"))

# PLOTS -------------------------------------------------------------------
# plotting both nf and watersheds, plus all_datasets to check accuracy
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

