#Destroy a square
library(tidyverse)
set.seed(12238)

#The square

square_size = 400

the_square <- expand_grid(
  x=1:square_size, 
  y=1:square_size
) %>% 
  mutate(
    color = "#87ceeb"
  )

num_squares = 9

cols_squares = c("#87ceeb", "#F26F31", "#FF69B4", "#29ab87")
# cols_squares <- sample(rep(cols_squares, 3), num_squares, F)
#I want to force more diversity in the colors...
cols_squares = c(sample(cols_squares, 4), 
                 sample(cols_squares, 4), 
                 sample(cols_squares, 4))[1:num_squares]
scales::show_col(cols_squares)

# Minor slices out of the square
cols_squares %>% purrr::map(function(this_col){
  
  num_slices = sample(2:6, 1)
  
  the_destroyed <- the_square %>% 
    mutate(color = this_col)
  
  for(ii in 1:num_slices){
    # The Circle
    circ_origin_size = square_size * runif(1, 2/4, 3/4) 
    circ_origin_theta = runif(1, 0, 2)*pi
    
    circ_x0 = circ_origin_size*cos(circ_origin_theta) - circ_origin_size*sin(circ_origin_theta) + square_size/2 
    circ_y0 = circ_origin_size*cos(circ_origin_theta) + circ_origin_size*sin(circ_origin_theta) + square_size/2
    circ_r  = square_size * runif(1, 1/4, 4/5)
    
    #The Destruction
    theta = runif(1, -0.2, 0.2) * pi
    
    #The destroyed square
    the_destroyed <- the_destroyed %>% 
      #Figure out if part of the square is in circle's area
      mutate(
        overlap = ((x-circ_x0)^2 + (y-circ_y0)^2)^(1/2) <= circ_r
      ) %>% 
      #Rotate the overlapping points
      mutate(
        x1 = (x - circ_x0)*cos(theta) - (y - circ_y0)*sin(theta) + circ_x0,
        y1 = (y - circ_y0)*cos(theta) + (x - circ_x0)*sin(theta) + circ_y0
      ) %>% 
      mutate(x = ifelse(overlap, x1, x),
             y = ifelse(overlap, y1, y)) %>% 
      mutate(color = case_when(
        overlap ~ colorspace::darken(color, ii/10),
        TRUE ~ color)) %>% 
      select(-x1, -y1, -overlap) 
    
  }
  
  
  the_destroyed %>% 
    ggplot(aes(x, y)) +
    geom_raster(data = the_square, fill = "#e9e9e9") +
    geom_point(aes(color = color), shape = 15) +
    scale_color_identity() +
    coord_fixed(xlim = c(-square_size*(1/4), square_size*(5/4)), 
                ylim = c(-square_size*(1/4), square_size*(5/4))
                )  +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#fefae6", color = NA)
    )
}) %>% 
  patchwork::wrap_plots()


ggsave("genuary_05.png", device = "png",
       width = 12, height = 12)
