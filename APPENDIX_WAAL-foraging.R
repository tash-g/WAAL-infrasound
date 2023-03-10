
# Code for plotting wind data in space ------------------------------------

wind.arrows <- wind.sp %>%
  slice(which(row_number() %% 10 == 1))

ggplot(wind.sp, 
       aes(x = lon, 
           y = lat, 
           fill = windSp, 
           angle = windDir / 180 * pi, 
           radius = scales::rescale(windSp, c(.2, .8)))) +
  geom_raster() +
  geom_spoke(data = wind.arrows,
             arrow = arrow(length = unit(0.05, 'inches'))) + 
  scale_fill_distiller(palette = "RdYlGn") + 
  coord_equal(expand = 0) + 
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal')

