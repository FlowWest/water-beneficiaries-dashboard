# this is where we will aggregate all layers into a consistent format and
# do the geoprocessing necessary for the NF and HUCs

library(tidyverse)
library(sf)


# load in datasets --------------------------------------------------------
hydropower <- readRDS('data-raw/hydropower_clean.RDS') |>
  select(beneficiary_type, entity_name, entity_address, quantity_metric, quantity_unit, national_forest_connection, latitude, longitude, geometry)




# map datasets to NF and HUCs ---------------------------------------------




# save dataset ------------------------------------------------------------

all_datasets <- bind_rows(hydropower)

saveRDS(all_datasets, here::here("data", "all_datasets.RDS"))
