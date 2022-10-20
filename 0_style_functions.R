
style_line <- function(...) {
  
  theme_minimal()+
    theme(plot.title = element_text(family = "serif", face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(family = "serif", size = 8, hjust = 0.5),
          plot.caption = element_text(size = 10, family = "serif", face = "bold"),
          legend.text = element_text(family = "serif", size = 10),
          legend.title = element_text(family = "serif", face = "bold", size = 10),
          axis.title = element_text(family = "serif", size = 10, face = "italic"),
          axis.text.x = element_text(face = "bold", angle = 45, size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "bottom")
  
}


style_sf <- function(legenda = FALSE){
  
  legend_position <- ifelse(  legenda  == TRUE, "bottom" , "none")
  
  theme(plot.title = element_text(family = "serif", face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(family = "serif", size = 8, hjust = 0.5),
        plot.caption = element_text(face = "bold", size = 10),
        plot.caption.position = "plot", # caption se alinha ao plot
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = legend_position,
        legend.title = element_text(face = "bold"))
  
}
