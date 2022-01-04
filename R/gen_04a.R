#The next next Fidenza

library(tidyverse)
set.seed(12335)

#Initialize grid
canvas_size = 400

flowfield_raw <- expand_grid(x = 1:canvas_size,
                             y = 1:round(canvas_size)) %>% 
  mutate(angle = sin(x/50 + y/80) + cos(x/50 - y/80),
         angle = scales::rescale(angle, from = c(-2, 2), to = c(0, 0.75))) %>%
  #Add noise
  # mutate(angle = angle + runif(n(), -0.05, 0.05)) %>% 
  pivot_wider(names_from = x, values_from = angle) %>% 
  select(-y) %>% 
  as.matrix()


#Rectangle cascading
num_origins = 16

rect_start <- tibble::tibble(
  rect_id = 1:num_origins,
  posi_id = 1, 
  x0 = 5  + 40*((rect_id-1) %% (num_origins/4)), 
  x1 = 25 + 40*((rect_id-1) %% (num_origins/4)),
  y0 = 5  + 40*((rect_id-1) %/% (num_origins/4)), 
  y1 = 30 + 40*((rect_id-1) %/% (num_origins/4))
) 

#See the starting grid
# rect_start %>% 
#   ggplot() +
#   aes(group = rect_id) +
#   geom_rect(aes(xmin = x0, xmax = x1,
#                 ymin = y0, ymax = y1),
#             color = "#222288", fill = NA) +
#   scale_y_reverse() +
#   coord_equal()

1:num_origins %>% 
  purrr::map_dfr(
    function(this_rect){
      num_rect <- sample(20:50, 1)
      rect_zoom <- 1# scales::rescale(num_rect, from = c(20, 50), to = c(1, 1.3))
      
      this_rect_df <- rect_start %>% 
        filter(rect_id == this_rect)
      
      for(posi in 1:num_rect){
        
        this_length <- round(posi ^ (1/2))
        
        this_zoom <- (1 + ((rect_zoom-1) * posi/num_rect )) - 1
        
        this_posi <- this_rect_df %>% 
          #Increment rectangle
          filter(
            posi_id == max(posi_id)
          ) %>% 
          mutate(posi_id = max(posi_id) + 1,
                 this_angle = flowfield_raw[round(y0), ceiling(abs(x0))]*pi) %>% 
          #Shift it down and to the right
          mutate(across(starts_with("x"), ~.x + this_length*cos(this_angle))) %>% 
          mutate(across(starts_with("y"), ~.x + this_length*sin(this_angle))) %>% 
          #Zoom the 1s
          mutate(x1 = x1 + this_zoom * this_length*cos(this_angle),
                 y1 = y1 + this_zoom * this_length*sin(this_angle)) %>% 
          select(-this_angle)
        
        this_rect_df <- this_rect_df %>% 
          bind_rows(this_posi)
      }
      return(this_rect_df)
    }
  ) -> rect_all

#For each rect path, choose one of 4 sides to display
cols_sides = c("#3ec7e3", "#FFFF40", "#F26F31", "#696969") %>% 
  sample(4)

rect_sides_and_colors <- tibble::tibble(
  rect_id = 1:num_origins,
  rect_side = sample(c("front", "rear", "upper", "bottom"), num_origins,
                     replace = TRUE),
  rect_color = cols_sides[as.numeric(factor(rect_side))]
)

1:num_origins %>% 
  purrr::map_dfr(
    function(this_rect){
      this_side <- rect_sides_and_colors %>% 
        filter(rect_id == this_rect) %>% 
        pull(rect_side)
      
      if(this_side == "front"){
        rect_all_front <- bind_rows(
          rect_all %>% select(rect_id, posi_id,
                              x=x0, y=y1),
          rect_all %>% select(rect_id, posi_id,
                              x=x0, y=y0) %>% 
            arrange(desc(posi_id)) 
        )%>% 
          filter(rect_id == this_rect)
      } else if( this_side == "upper"){
        rect_all_upper <- bind_rows(
          rect_all %>% select(rect_id, posi_id,
                              x=x1, y=y0),
          rect_all %>% select(rect_id, posi_id,
                              x=x0, y=y0) %>% 
            arrange(desc(posi_id))
        )%>% 
          filter(rect_id == this_rect)
      } else if(this_side == "rear"){
        rect_all_rear <- bind_rows(
          rect_all %>% select(rect_id, posi_id,
                              x=x1, y=y0),
          rect_all %>% select(rect_id, posi_id,
                              x=x1, y=y1) %>% 
            arrange(desc(posi_id))
        )%>% 
          filter(rect_id == this_rect)
      } else{
        rect_all_bottom <- bind_rows(
          rect_all %>% select(rect_id, posi_id,
                              x=x0, y=y1),
          rect_all %>% select(rect_id, posi_id,
                              x=x1, y=y1) %>% 
            arrange(desc(posi_id))
        )%>% 
          filter(rect_id == this_rect)
      }
    }
  ) -> rect_all_polygon 

rect_all %>% 
  ggplot() +
  aes(group = rect_id) +
  geom_rect(data = rect_all %>% 
              group_by(rect_id) %>% 
              filter(posi_id == 1),
            aes(xmin = x0, xmax = x1,
                ymin = y0, ymax = y1),
            color = "#222288", 
            fill = colorspace::darken("#191970", 0.2)) +
  #Filled sides
  geom_polygon(data = rect_all_polygon %>% 
                 left_join(rect_sides_and_colors),
               aes(x=x,y=y, fill = rect_color),
               #fill = "#FF4040", 
               alpha = 0.5,
               color = "black") +
  scale_fill_identity() +
  #Front rectanlge
  geom_rect(data = rect_all %>% 
              group_by(rect_id) %>% 
              filter(posi_id %in% c(max(posi_id))),
            aes(xmin = x0, xmax = x1,
                ymin = y0, ymax = y1),
            color = "#191970", fill = colorspace::darken("#fefae6", 0.05),
            alpha = 0.9) +
  scale_y_reverse() +
  coord_equal() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#fefae6", color = NA)
  )




