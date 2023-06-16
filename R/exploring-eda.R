
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
  geom_point(shape = 21,aes(fill = sdrFS), color = 'black', size = 3) +
  #scale_fill_manual(values = c('forestgreen', 'yellow', 'red')) +
  scale_fill_gradientn(colors = rev(hcl.colors(11, 'Zissou1'))) +
  geom_abline(slope = 1, intercept = 0) +
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
library(ggiraph)
css_default_hover <- girafe_css_bicolor(primary ="black", secondary = 'black')

set_girafe_defaults(
  opts_hover = opts_hover(css = "img{ opacity: 0.5;}"),
  opts_zoom = opts_zoom(min = 1, max = 4),
  opts_tooltip = opts_tooltip(css = "padding:3px;color:black;"),
  opts_sizing = opts_sizing(rescale = TRUE),
  opts_toolbar = opts_toolbar(saveaspng = FALSE, position = "bottom", delay_mouseout = 5000)
)

p2 <- bt_baseline_2017 %>% st_as_sf(crs = st_crs(bt_local)) %>%
  ggplot() +
  geom_sf_interactive(aes(fill = integrated_6th_level_huc_rating,
                          tooltip = paste0('HUC #: ', HUC_12, '<br>',
                                           'Rating: ', integrated_6th_level_huc_rating),
                          data_id = HUC_12), lwd = 0.1) +
  scale_fill_manual(values = c( 'forestgreen', 'yellow', 'red', 'grey')) +
  custom_theme(map_void = 4) +
  labs(fill = 'Baseline Rating')

girafe(ggobj = p2,
       options = list(
         opts_hover = opts_hover(),
         opts_zoom = opts_zoom(min = 1, max = 4),
         opts_tooltip = opts_tooltip(use_fill = TRUE, css = "padding:3px;color:black;opacity: 0.25;"),
         opts_sizing = opts_sizing(rescale = TRUE),
         opts_toolbar = opts_toolbar(saveaspng = FALSE, position = "bottom", delay_mouseout = 5000),
         opts_sizing(width = .7)
       ))

p1 <- bt_baseline_2017 %>% st_as_sf(crs = st_crs(bt_local)) %>%
  pivot_longer(cols = portion_of_watershed_in_eca_24:rca) %>%
  ggplot() +
  geom_sf_interactive(aes(fill = value,
                          tooltip = paste0('HUC #: ', HUC_12, '<br>',
                                           'Rating: ', value),
                          data_id = HUC_12), lwd = 0.1) +
  scale_fill_manual(values = c( 'forestgreen', 'yellow', 'red', 'grey')) +
  custom_theme(map_void = 4) +
  labs(fill = 'Baseline Rating') +
  theme(strip.text = element_text(size = 3), legend.position = 'none') +
  facet_wrap(~name)
library(cowplot)
girafe(
  ggobj = plot_grid(p1, p2),
  options = list(opts_hover = opts_hover(css = css_default_hover),
                 opts_zoom = opts_zoom(min = 1, max = 4),
                 opts_tooltip = opts_tooltip(use_fill = TRUE, css = "padding:3px;color:black;"),
                 opts_sizing = opts_sizing(rescale = TRUE),
                 opts_toolbar = opts_toolbar(saveaspng = FALSE, position = "bottom", delay_mouseout = 5000),
                 opts_sizing(width = .7),
                 opts_selection(
                   type = "multiple",
                   only_shiny = FALSE
                 )
  )
)
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
