#Dithering

library(tidyverse)
set.seed(21574)
 
#Rainbow squares
cols_rnb <- c(
  "#9400D3", #Violet
  "#4B0082", #Indigo
  "#0000FF", #Blue
  "#00FF00", #Green
  "#FFFF00", #Yellow
  "#FF7F00", #Orange
  "#FF0000"  #Red
)

cols_outline <- "#000000"
width_outline <- 5

dim_x = 200*5
dim_y = 140*5

lum_tolerance <- 0.2 #Puts more of the color in each pixel section
dither_size <- 5 #Square size of the pixel sections
dither_max_shade <- 0.5 #Max % of shade pixels in section

rnb_base <- expand_grid(
  x = 1:dim_x,
  y = 1:dim_y
)

all_r_x = sample(seq(5, dim_x*0.7, by = 20), length(cols_rnb), replace = FALSE)
all_r_y = sample(seq(5, dim_y*0.7, by = 20), length(cols_rnb), replace = FALSE)

rnb_1 <- rnb_base

#For each color, create a rectangle in the canvas
for(ii in seq_along(cols_rnb)) {
  r_width  = round(runif(1, min = dim_x / 7, max = dim_x / 3))
  r_height = round(runif(1, min = dim_y / 7, max = dim_y / 3))
  
  r_x = all_r_x[ii]
  r_y = all_r_y[ii]
  
  #Generate a deep shading gradient across the full canvas
  r_grad_x = sample(5:40, 1)
  r_grad_y = sample(5:40, 1)
  r_grad_dir = sample(c(-1, 1), 1)
  r_grad_power = 1
  
  rnb_1 <- rnb_1 %>% 
    mutate(this_gradient = sin((x/r_grad_x)^r_grad_power + r_grad_dir*(y/r_grad_y)^r_grad_power),
           this_gradient = scales::rescale(this_gradient, 
                                           from = c(-1, 1), to = c(-0.4, 0.4))) %>% 
    mutate(this_rect := case_when(
      x >= (r_x+width_outline) & x <= (r_x + (r_width-width_outline)) &
        y >= (r_y+width_outline) & y <= (r_y + (r_height-width_outline)) ~ colorspace::lighten(cols_rnb[ii], this_gradient),
      x >= r_x & x <= (r_x + r_width) &
        y >= r_y & y <= (r_y + r_height) ~ cols_outline
    )) %>% 
    select(-this_gradient)
  #! you could stop now and have gradient rectangles...
  #... but this is a dithering prompt
  
  #Turn rects into a dithered color of white, black, and that rainbow color
  #Trying my hand at manual dithering
  target_rgb <- farver::decode_colour(cols_rnb[ii])
  target_lum <- (0.299*target_rgb[1] + 0.587*target_rgb[2] + 0.114*target_rgb[3])
  
  rnb_1a <- rnb_1 %>% 
    drop_na(this_rect) %>% 
    filter(this_rect != cols_outline) %>% 
    mutate(this_rect_split = purrr::map(this_rect, 
                                        ~farver::decode_colour(.x) %>% 
                                          set_names(c("R", "G", "B")))) %>% 
    unnest_wider(this_rect_split) %>% 
    mutate(luminance = (0.299*R + 0.587*G + 0.114*B),
           lum_compare = case_when(
             abs(luminance - target_lum) <= lum_tolerance ~ 0,
             TRUE ~ sign(luminance - target_lum))
           ) %>% 
    #In each dither_size square section, share of pixels lighter & darker than target
    mutate(x_group = x %/% dither_size, 
           y_group = y %/% dither_size) %>% 
    group_by(x_group, y_group) %>% 
    mutate(shade_darker = ceiling(sum(lum_compare < 0)*(dither_max_shade)),
           shade_lighter = ceiling(sum(lum_compare > 0)*(dither_max_shade)),
           pixel_id = rank((x+y)%%2, ties = "first")) %>% 
    ungroup() %>% 
    arrange(x_group, y_group, pixel_id) %>% 
    #Convert pixels to black, white, or this color
    group_by(x_group, y_group) %>% 
    mutate(this_rect_dithered = case_when(
      pixel_id <= shade_darker ~ "#000000",
      max(pixel_id) - pixel_id + 1 <= shade_lighter ~ "#ffffff",
      TRUE ~ cols_rnb[ii]
    )) %>% 
    ungroup()
  
  rnb_1 <- rnb_1  %>% 
    left_join(rnb_1a %>% 
                select(x, y, this_rect_dithered), 
              by = c("x", "y")) %>% 
    mutate(!!paste0("rect_", ii) := ifelse(!is.na(this_rect_dithered),
                                           this_rect_dithered,
                                           this_rect)) %>% 
    select(-this_rect, -this_rect_dithered)
  
}

#plot it
rnb_1 %>% 
  pivot_longer(starts_with("rect_")) %>% 
  drop_na(value) %>% 
  ggplot() +
  aes(x, y) +
  geom_raster(aes(fill = value)) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#FFDE00")
  )

ggsave("genuary_02.png", device = "png",
       width = 10, height = 10 * dim_y/dim_x)

