library(sf)
library(dplyr)
library(readr)
library(janitor)

#### ---- GRAIP and BT Baseline data
# this is how the data was combined
graip_path <- 'data/R01_v3.gdb'

huc_usc <- read_sf(graip_path, layer = 'WCATT_HUCs_GL_Data_USC_Units_R01')

bt_baseline_2017 <- read_csv('data/bt_baseline_2017.csv') %>% janitor::clean_names()

bt_baseline_2017 <- bt_baseline_2017 %>%
  mutate(HUC_12 = as.character(huc_6th_code)) %>%
  left_join(huc_bt, by = 'HUC_12')

#### If changing then re-write to the files below....

# write_csv(bt_baseline_2017 %>% st_drop_geometry(), 'data/bt_baseline_with_graip_lite.csv')
#
# write_csv(wcatt_attribute_desc, 'data/WCATT_Attribute_Descriptions_GL_Data')
#
# write_sf(huc_usc, 'data/bt_baseline.gpkg', layer = 'WCATT_HUCs_GL_Data_USC_Units_R01')
#
# write_sf('data/bt_baseline.gpkg', layer = 'bt_baseline_with_graip_lite')
#
# write_sf(
#   bt_baseline_2017 %>%
#     st_as_sf() %>%
#     st_transform(crs = st_crs(huc_usc)), 'data/bt_baseline.gpkg', layer = 'bt_baseline_with_graip_lite')

#### ---- PIBO data

#bring in the PIBO data and associated fixed effects from previous work in pibo_model github repo

pibo_zonal <- read_csv('https://raw.githubusercontent.com/joshualerickson/pibo_model/main/data/gr_95_ts.csv')
pibo_habitat <- readxl::read_xlsx('data/pibo_data.xlsx', sheet = 'Habitat')
pibo_macro <- readxl::read_xlsx('data/pibo_data.xlsx', sheet = 'Macroinverts')
pibo_temp <- readxl::read_xlsx('data/pibo_data.xlsx', sheet = 'Temperature')

#pibo_av <- read_csv('https://raw.githubusercontent.com/joshualerickson/pibo_model/main/data/gr_95.csv')
pibo_all <- pibo_habitat %>% left_join(pibo_macro %>%
                                         select(SiteID, Yr, SiteName, Rich:RIVPACS)) %>%
  left_join(pibo_temp %>%
              select(SiteID, Yr, SiteName, TempDays:WMT22)) %>%
  janitor::clean_names()

#### now combine with the fixed effects
pibo_all_together <- pibo_all %>%
  filter(region == 'R1') %>%
  left_join(pibo_zonal %>% select(site_id, yr, aug_et_2000_2015_cpg_all:ave_basin_elev)) %>%
  left_join(pibo_av %>% select(site_id,MAP_UNIT_N))

### writing up the pibo data and spatial data
write_csv(pibo_all_together, 'data/pibo_all_together.csv')
write_sf(pibo_all_together %>% st_as_sf(coords = c('long', 'lat'), crs = 4326), 'data/bt_baseline.gpkg', layer = 'pibo_all_together')

