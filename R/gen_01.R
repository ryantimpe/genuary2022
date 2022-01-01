library(tidyverse)

#Draw 10,000 of something

# I want to draw hexagons...
# Take the geom_circle() script from ggforce, 
# ...and draw 6-sided circles instead of 360-sided

StatHexagon <- ggproto('StatHexagon', Stat,
                      compute_panel = function(data, scales, 
                                               n = 6 #Magic happens here
                                               ) {
                        data$start <- 0
                        data$end <- 2 * pi
                        ggforce:::arcPaths(data, n+0.5)
                      },
                      
                      required_aes = c('x0', 'y0', 'r')
)

stat_hexagon <- function(mapping = NULL, data = NULL, geom = 'hexagon',
                        position = 'identity', n = 360, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatHexagon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, ...)
  )
}

GeomHexagon <- ggproto('GeomHexagon', ggforce::GeomShape,
                      default_aes = list(
                        colour = 'black', fill = NA, size = 0.5, linetype = 1,
                        alpha = NA
                      )
)

geom_hexagon <- function(mapping = NULL, data = NULL, stat = 'hexagon',
                        position = 'identity', n = 6, expand = 0, radius = 0,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                        ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomHexagon,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}

# Now draw
expand_grid(x = 1:250,
            y = 1:40) %>% 
  #On odd-numbered X, Y starts at 1 and goes up by 3
  mutate(y0 = case_when(
    x %% 2 == 1 ~ (y-1)*3,
    x %% 2 == 0 ~ ((y-3) + 2.5)*3 
  ),
  x0 = x * sqrt(3)/2, #Ratio of the shorter radius to longer radius
  #Add some fun dynamics to the radius pattern
  r = sin(x/25+y/12),
  r = scales::rescale(r, to = c(0.35, 1), from = c(-1, 1)),
  #Fill with an opposing pattern
  fill = sin(x/50 - y/30)) %>% 
  ggplot() +
  geom_hexagon(
    aes(x0=x0, y0=y0, r=r, fill = fill),
    color = colorspace::darken("#191970")
  ) +
  scale_fill_gradient2(
    low = "#191970", mid = "#fd5e53", high = "#FFFFA7"
  ) + 
  coord_fixed(expand = F) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = colorspace::darken("#191970"))
  )


ggsave("genuary_01.png", device = "png", width = 10, height = 5.5)
