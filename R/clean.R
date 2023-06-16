
library(sf)
library(tidyverse)

graip_path <- 'Z:/simple_features/roads/R01_v3.gdb'
st_layers(graip_path)

hucs_gl <- read_sf(graip_path, layer = 'HUCs_GL_Runs_R01')

glimpse(hucs_gl)

wcatt_attribute_desc <- read_sf(graip_path, layer = 'WCATT_Attribute_Descriptions_GL_Data')
not_sure <- read_sf(graip_path, layer = 'WCATT_HUCs_GL_Data_SI_Units_R01')
huc_usc <- read_sf(graip_path, layer = 'WCATT_HUCs_GL_Data_USC_Units_R01')


bt_local <- read_sf('Z:/simple_features/fish/BT_Local_Populations2018/BT_Local_Populations2018.shp') %>%
  st_transform(crs = st_crs(huc_usc))


library(mapview)
library(leaflet.extras2)

mapviewOptions(fgb = TRUE)
huc_usc %>% select(sdel, sdelFS) %>% mapview(zcol = 'sdel')

huc_bt <- st_filter(huc_usc, bt_local, .predicate = st_intersects)

bt_baseline_2017 <- read_csv('data/bt_baseline_2017.csv') %>% janitor::clean_names()

bt_baseline_2017 <- bt_baseline_2017 %>%
  mutate(HUC_12 = as.character(huc_6th_code)) %>%
  left_join(huc_bt, by = 'HUC_12')

admin <- read_sf('Z:/simple_features/lands/admin_units.shp') %>% st_transform(st_crs(huc_usc))

roads <- read_sf('Z:/simple_features/roads/r1_rd_core.shp') %>% st_transform(st_crs(huc_usc))
rd_crop <- roads %>% st_crop(bt_baseline_2017 %>% st_as_sf(crs = st_crs(bt_local)) %>% filter(HUC_12 == '170102031107'))
admin_crop <- admin %>% st_crop(bt_baseline_2017 %>% st_as_sf(crs = st_crs(bt_local)) %>% filter(HUC_12 == '170102031107'))

huc_usc %>% select(sdel, sdelFS) %>% mapview(zcol = 'sdel')


huc_bt %>% mapview(size = 3, zcol = 'sdrFSOp')

library(plotly)
ggplotly(huc_bt %>% st_drop_geometry() %>%
           filter(FS_Land_sqmi >= 1) %>%
  mutate(rdens = clenFS/FS_Land_sqmi) %>%
  filter(rdens < 4) %>%
  ggplot(aes(sdelFS, rdens, color = WCF_RdDen_FSOp_HA)) +
  geom_point(aes(label = HUC_12)))
huc_bt %>% filter(sdelFS == 0)

huc_bt %>%
  ggplot(aes(sdrFSOp)) +
  geom_histogram()


ggplotly(bt_baseline_2017 %>% st_drop_geometry() %>%
  ggplot(aes(sdrFSOp, road_density, color = road_density_rating)) +
  geom_point())

ggplotly(bt_baseline_2017 %>%
           st_drop_geometry() %>%
           filter(FS_Land_sqmi > 1) %>%
           ggplot(aes(sdrFS, road_density)) +
           geom_point(shape = 21, aes(fill = WCF_RdDenFSOp_HA,
                                      text = paste0(paste0('Sediment Delivery Ratio: ', sdrFS, '<br>'),
                                                  paste0('Road Denisty: ', road_density, '<br>'),
                                             paste0('HUC 12 #: ', HUC_12, '<br>'),
                                             paste0('Baseline Rating:', road_density_rating))), color = 'black', size = 3) +
           scale_fill_manual(values = c('forestgreen', 'yellow', 'red')) +
           custom_theme() +
           labs(y = 'FS Only Road Density',
                x = 'Sediment Delivery/Sediment Produced (FS Only)',
                Fill = 'Baseline Rating'), tooltip = list('text'))

resourceviz::cairo_view()
library(resourceviz)

bt_baseline_2017 %>%
  st_drop_geometry() %>%
  filter(fs_huc_area > 1000) %>%
  ggplot(aes(sdrFS, road_density)) +
  geom_point(shape = 21, aes(fill = fs_huc_area), color = 'black', size = 3) +
  #scale_fill_manual(values = c('forestgreen', 'yellow', 'red')) +
  scale_fill_gradientn(colors = hcl.colors(11, 'Zissou1')) +
  custom_theme() +
  labs(y = 'FS Only Road Density',
       x = 'Sediment Delivery/Sediment Produced (FS Only)',
       fill = 'Baseline Rating') +
  facet_wrap(~huc_4th_name)

bt_baseline_2017 %>% st_drop_geometry() %>% filter(fs_huc_area > 10000) %>%
  ggplot(aes(sdelFS, road_density)) +
  geom_point(shape = 21, aes(fill = total_road_length),color = 'black', size = 3) +
  #scale_fill_manual(values = c('forestgreen', 'yellow', 'red')) +
  scale_fill_gradientn(colors = rev(hcl.colors(11, 'Zissou1'))) +
  custom_theme() +
  labs(y = 'FS Only Road Density',
       x = 'Sediment Delivery (FS Only)',
       fill = 'Baseline Rating')
install.packages('ggpairs')

ggpairs(bt_baseline_2017 %>% st_drop_geometry() %>% select(),
        mapping = aes(color = mgmt), upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                       alpha = 0.6)))+
  custom_theme() + labs(title = 'Comparing Mean Bank Angle with Potential Covariates')
 # facet_wrap(~huc_4th_name)

bt_baseline_2017 %>% st_as_sf(crs = st_crs(bt_local)) %>%
  ggplot() +
  geom_sf(aes(fill = road_density_rating, size = 2)) +
  scale_fill_manual(values = c('forestgreen', 'yellow', 'red')) +
  custom_theme() +
  labs(fill = 'Baseline Rating')

nhd <- nhdplusTools::get_nhdplus(bt_baseline_2017 %>%
                                   st_as_sf(crs = st_crs(bt_local)) %>%
                                   filter(HUC_12 == '170102031107')) %>% st_transform(st_crs(bt_baseline_2017 %>%
                                                                       st_as_sf(crs = st_crs(bt_local)) %>%
                                                                       filter(HUC_12 == '170102031107')))
rd_crop %>% st_intersection(admin_crop) %>% view()
  ggplot() +
  geom_sf(data = admin_crop, fill = 'forestgreen', alpha = 0.5) +
  geom_sf(data = rd_crop %>% filter(JURISDICTI == 'FS - FOREST SERVICE'),
          alpha = 1, lwd = 0.25, col = 'red') +
  geom_sf(data = bt_baseline_2017 %>%
            st_as_sf(crs = st_crs(bt_local)) %>%
            filter(HUC_12 == '170102031107'),
          fill = NA,
          lwd = 1,
          linetype = 2) +
  geom_sf(data = nhd, col = 'blue', lwd = 1, alpha = 0.5) +
  custom_theme(map_void = 4) +
  labs(title = 'Example of Road Density vs GRAIP-Lite',
       subtitle = 'Blue = NHDPlus; Dashed Line = HUC 170102031107; Green = US FOREST SERVICE; Red = USFS RD Jurisdiction')


  library(ggiraph)
 ggplot() +
    geom_sf(data = admin_crop, fill = 'forestgreen', alpha = 0.5) +
    geom_sf(data = rd_crop %>% filter(JURISDICTI == 'FS - FOREST SERVICE'),
            alpha = 1, lwd = 0.25, col = 'red') +
    geom_sf(data = bt_baseline_2017 %>%
              st_as_sf(crs = st_crs(bt_local)) %>%
              filter(HUC_12 == '170102031107'),
            fill = NA,
            lwd = 1,
            linetype = 2) +
    geom_sf(data = nhd, col = 'blue', lwd = 1, alpha = 0.5) +
    custom_theme(map_void = 4) +
    labs(title = 'Example of Road Density vs GRAIP-Lite',
         subtitle = 'Blue = NHDPlus; Dashed Line = HUC 170102031107; Green = US FOREST SERVICE; Red = USFS RD Jurisdiction')

  gg_point = ggplot(data = data) +
    geom_point_interactive(aes(x = wt, y = qsec, color = disp,
                               tooltip = carname, data_id = carname)) +
    theme_minimal()

  girafe(ggobj = gg)



  library(ggplot2)
  library(ggiraph)
  library(cowplot)
  bt_scatter <-
    bt_baseline_2017 %>% st_drop_geometry() %>% filter(fs_huc_area > 10000) %>%
    ggplot(aes(sdelFS, road_density)) +
    geom_point_interactive(shape = 21, color = 'black', size = 3,
                           aes(
                             tooltip = paste0('HUC #: ', HUC_12, '<br>',
                                              'Road Density Rating: ', road_density_rating),
                             data_id = HUC_12)) +
    #scale_fill_manual(values = c('forestgreen', 'yellow', 'red')) +
    custom_theme() +
    theme(legend.position = 'none') +
    labs(y = 'FS Only Road Density',
         x = 'Sediment Delivery (FS Only)',
         fill = 'Baseline Rating')

  # then make the map
  bt_map <- bt_baseline_2017 %>% st_as_sf(crs = st_crs(huc_usc)) %>%
    filter(fs_huc_area > 10000)  %>%
    ggplot() +
    geom_sf_interactive(
      aes(
        tooltip = paste0('HUC #: ', HUC_12, '<br>',
                         'Road Density Rating: ', road_density_rating),
        data_id = HUC_12
      ),
      size = 0.2, color = "black"
    ) +
    custom_theme(map_void = 4)
  # then combine
  girafe(
    ggobj = plot_grid(bt_scatter, bt_map),
    width_svg = 8,
    height_svg = 4,
    options = list(
      opts_selection(
        type = "multiple",
        only_shiny = FALSE
      )
    )
  )