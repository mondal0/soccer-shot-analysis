Soccer_shots <- read.csv("data/Soccer_data.csv")

opl = Soccer_shots[Soccer_shots$situation == 0 & Soccer_shots$shot_type == 1,][,c(2,17,18)]
opr = Soccer_shots[Soccer_shots$situation == 0 & Soccer_shots$shot_type == 0,][,c(2,17,18)]
spl = Soccer_shots[Soccer_shots$situation == 1 & Soccer_shots$shot_type == 1,][,c(2,17,18)]
spr = Soccer_shots[Soccer_shots$situation == 1 & Soccer_shots$shot_type == 0,][,c(2,17,18)]



#############################
library(ggplot2)
library(patchwork)
library(magick)



draw_soccer_field <- function() {
  field <- data.frame(
    x = c(0.5, 1, 1, 0.5, 0.5), 
    y = c(0, 0, 1, 1, 0) 
  )
  
  penalty_box <- data.frame(
    x = c(1, 1, 0.8421053, 0.8421053, 1),
    y = c(0.2027028, 0.7972972, 0.7972972, 0.2027028, 0.2027028)
  )
  
  six_yard_box <- data.frame(
    x = c(1, 1, 0.9473684, 0.9473684, 1),
    y = c(0.3648649, 0.6351351, 0.6351351, 0.3648649, 0.3648649)
  )
  
  D_box <- data.frame(
    x = 0.8947368 + 0.0877193 * cos(seq(17*pi/24, 31*pi/24, length.out = 100)),  
    y = 0.5 + 0.0877193 * sin(seq(17*pi/24, 31*pi/24, length.out = 100))     
  )
  
  center_circle <- data.frame(
    y = 0.5 + 0.0877193 * cos(seq(0, pi, length.out = 100)), 
    x = 0.5 + 0.0877193 * sin(seq(0, pi, length.out = 100))  
  )
  
  left_corner <- data.frame(
    y = 1 - 0.01 * cos(seq(0, pi/2, length.out = 100)),  
    x = 1 - 0.01 * sin(seq(0, pi/2, length.out = 100))   
  )
  
  right_corner <- data.frame(
    y = 0 + 0.01 * cos(seq(0, pi/2, length.out = 100)), 
    x = 1 - 0.01 * sin(seq(0, pi/2, length.out = 100))  
  )
  
  ggplot() + 
    geom_polygon(data = field, aes(x = x, y = y), fill = "white", color = "#a9a9a9") + 
    geom_polygon(data = penalty_box, aes(x = x, y = y), fill = NA, color = "#a9a9a9") +  
    geom_polygon(data = six_yard_box, aes(x = x, y = y), fill = NA, color = "#a9a9a9") + 
    geom_path(data = D_box, aes(x = x, y = y), color = "#a9a9a9") +  
    geom_path(data = center_circle, aes(x = x, y = y), color = "#a9a9a9") +  
    annotate("segment", x = 1, xend = 1, y = 0.445946, yend = 0.554054, size = 1, color = "#a9a9a9")+  
    annotate("point", x = 0.8947368, y = 0.5, size = 1, color = "#a9a9a9") + 
    annotate("point", x = 0.5, y = 0.5, size = 1, color = "#a9a9a9") +
    geom_path(data = left_corner, aes(x = x, y = y), color = "#a9a9a9") + 
    geom_path(data = right_corner, aes(x = x, y = y), color = "#a9a9a9")  
}


plot1 <- draw_soccer_field() +
  geom_point(data = opl, aes(x = x, y = y, shape = factor(result), alpha = factor(result), color = factor(result)), size = 1) + 
  scale_shape_manual(values = c(4, 20)) +  
  scale_alpha_manual(values = c(0.5, 0.2)) +  
  scale_color_manual(values = c("black", "gray")) + 
  theme_minimal() +
  labs(x = "", y = "") + theme_void() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank())


plot2 <- draw_soccer_field() +
  geom_point(data = opr, aes(x = x, y = y, shape = factor(result), alpha = factor(result), color = factor(result)), size = 1) + 
  scale_shape_manual(values = c(4, 20)) +  
  scale_alpha_manual(values = c(0.5, 0.2)) +  
  scale_color_manual(values = c("black", "gray")) +  
  theme_minimal() +
  
  labs(x = "", y = "") + theme_void() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank())


plot3 <- draw_soccer_field() +
  geom_point(data = spl, aes(x = x, y = y, shape = factor(result), alpha = factor(result), color = factor(result)), size = 1) + 
  scale_shape_manual(values = c(4, 20)) +  
  scale_alpha_manual(values = c(0.5, 0.2)) +  
  scale_color_manual(values = c("black", "gray")) +  
  theme_minimal() +
  
  labs(x = "", y = "") + theme_void() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank())


plot4 <- draw_soccer_field() +
  geom_point(data = spr, aes(x = x, y = y, shape = factor(result), alpha = factor(result), color = factor(result)), size = 1) + 
  scale_shape_manual(values = c(4, 20)) +  
  scale_alpha_manual(values = c(0.5, 0.2)) +  
  scale_color_manual(values = c("black", "gray")) +  
  theme_minimal() +
  
  labs(x = "", y = "") + theme_void() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank())


p = (plot3 | plot1) /  (plot4 | plot2) 
p
# This figure is a rotated version of Figure 1

# ggsave("P1.png",
#        
#        plot = p,
#        
#        width = 5,
#        
#        height = 10,
#        
#        units = "in",
#        
#        dpi = 300)
