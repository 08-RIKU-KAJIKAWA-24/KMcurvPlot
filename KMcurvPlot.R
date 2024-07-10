

### Install and load packages ###
require("extrafont")
require("scales")
require("tidyverse")




### viewer open for font ###
# If you don't have "extrafont" package, you have to run the following code only once -----
# extrafont::font_import()
# extrafont::loadfonts(device = "win")

# abailable font -----
View(extrafont::fonttable())




### Kaplan-Meier plot arrangement function ###
KMplot <- function(csv.path,                               #--- path of dataset 
                   n.arm = 2,                              #--- num of arms
                   maxtime = 10,                           #--- maxtime of Xaxis
                   xint = 1,                               #--- interval of Xaxis
                   yint = 0.1,                             #--- interval of Yaxis
                   plot.size = 0.5,                        #--- thickness of plot
                   axis.size= 0.5,                         #--- thickness of axis
                   col.bg = "white",                       #--- backgroud color
                   col.axis = "black",                     #--- axis color
                   col.arm = c("#59CEE9", "#FF00FF"),      #--- each arm's plot color
                   font = "Calibri",                       #--- label font
                   ylim = "",                              #--- Yaxis label ("prop" -> "0% to 100%" defalt -> "0.0 to 1.0")
                   xlabel = "Years of randomizaion",       #--- Xaxis label
                   ylabel = "Overall survival"             #--- Yaxis label
                   ){
  
  # === Import dataset =======================================================================================
  dat <<- read.csv(file = csv.path)
  
  # === Single arm plot ======================================================================================
  if(n.arm == 1){
    
    # data arrange -----
    leng.1 <- dat %>% select(leng1) %>% filter(!is.na(leng1)) %>% max()
    s.1 <- dat %>% select(s1) %>% filter(!is.na(s1)) %>% min()
    dat$leng1 <- ifelse(is.na(dat$leng1), leng.1, dat$leng1)
    dat$s1 <- ifelse(is.na(dat$s1), s.1, dat$s1)
    
    # create plot -----
    panel <- dat %>% 
      ggplot() +
      labs(x = xlabel,
           y = ylabel) +
      theme_bw() +
      theme(plot.background = element_rect(colour = col.bg, fill = col.bg),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(colour = col.bg, fill = col.bg),
            axis.line = element_line(colour = col.axis, linewidth = axis.size),
            axis.text = element_text(colour = col.axis, size = 15),
            axis.ticks = element_line(colour = col.axis),
            axis.text.x = element_text(margin = unit(rep(4, 4), "mm")),
            axis.text.y = element_text(margin = unit(rep(4 ,4), "mm")),
            axis.title.x = element_text(colour = col.axis, size = 20, family = font),
            axis.title.y = element_text(colour = col.axis, size = 20, family = font),
            axis.ticks.x = element_line(linewidth = axis.size),
            axis.ticks.y = element_line(linewidth = axis.size),
            axis.ticks.length = unit(-2.5, "mm")) +
      scale_x_continuous(breaks = seq(-1, maxtime+1, xint), limits = c(0, maxtime+0.2), expand = c(0, 0)) +
      geom_step(mapping = aes(x = leng1, y = s1), stat = "identity", colour = col.arm[1], linewidth = plot.size)
  
  # === 2 arms plot ===========================================================================================  
  }else if(n.arm == 2){
    
    #data arrange -----
    leng.1 <- dat %>% select(leng1) %>% filter(!is.na(leng1)) %>% max()
    s.1 <- dat %>% select(s1) %>% filter(!is.na(s1)) %>% min()
    dat$leng1 <- ifelse(is.na(dat$leng1), leng.1, dat$leng1)
    dat$s1 <- ifelse(is.na(dat$s1), s.1, dat$s1)
    
    leng.2 <- dat %>% select(leng2) %>% filter(!is.na(leng2)) %>% max() 
    s.2 <-dat %>% select(s2) %>% filter(!is.na(s2)) %>% min()
    dat$leng2 <- ifelse(is.na(dat$leng2), leng.2, dat$leng2)
    dat$s2 <- ifelse(is.na(dat$s2), s.2, dat$s2)
    
    # create plot -----
    panel <- dat %>% 
      ggplot() +
      labs(x = xlabel,
           y = ylabel) +
      theme_bw() +
      theme(plot.background = element_rect(colour = col.bg, fill = col.bg),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(colour = col.bg, fill = col.bg),
            axis.line = element_line(colour = col.axis, linewidth = axis.size),
            axis.text = element_text(colour = col.axis, size = 15),
            axis.ticks = element_line(colour = col.axis),
            axis.text.x = element_text(margin = unit(rep(4, 4), "mm")),
            axis.text.y = element_text(margin = unit(rep(4 ,4), "mm")),
            axis.title.x = element_text(colour = col.axis, size = 20, family = font),
            axis.title.y = element_text(colour = col.axis, size = 20, family = font),
            axis.ticks.x = element_line(linewidth = axis.size),
            axis.ticks.y = element_line(linewidth = axis.size),
            axis.ticks.length = unit(-2.5, "mm")) +
      scale_x_continuous(breaks = seq(-1, maxtime+1, xint), limits = c(0, maxtime+0.2), expand = c(0, 0)) +
      geom_step(mapping = aes(x = leng1, y = s1), stat = "identity", colour = col.arm[1], linewidth = plot.size) +
      geom_step(mapping = aes(x = leng2, y = s2), stat = "identity", colour = col.arm[2], linewidth = plot.size)
    
  # === 3 arms plot ===========================================================================================
  }else{
    
    #data arrange -----
    leng.1 <- dat %>% select(leng1) %>% filter(!is.na(leng1)) %>% max()
    s.1 <- dat %>% select(s1) %>% filter(!is.na(s1)) %>% min()
    dat$leng1 <- ifelse(is.na(dat$leng1), leng.1, dat$leng1)
    dat$s1 <- ifelse(is.na(dat$s1), s.1, dat$s1)
    
    leng.2 <- dat %>% select(leng2) %>% filter(!is.na(leng2)) %>% max() 
    s.2 <-dat %>% select(s2) %>% filter(!is.na(s2)) %>% min()
    dat$leng2 <- ifelse(is.na(dat$leng2), leng.2, dat$leng2)
    dat$s2 <- ifelse(is.na(dat$s2), s.2, dat$s2)
    
    leng.3 <- dat %>% select(leng3) %>% filter(!is.na(leng3)) %>% max() 
    s.3 <-dat %>% select(s3) %>% filter(!is.na(s3)) %>% min()
    dat$leng3 <- ifelse(is.na(dat$leng3), leng.3, dat$leng3)
    dat$s3 <- ifelse(is.na(dat$s3), s.3, dat$s3)
    
    # create plot ---
    panel <- dat %>% 
      ggplot() +
      labs(x = xlabel,
           y = ylabel) +
      theme_bw() +
      theme(plot.background = element_rect(colour = col.bg, fill = col.bg),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(colour = col.bg, fill = col.bg),
            axis.line = element_line(colour = col.axis, linewidth = axis.size),
            axis.text = element_text(colour = col.axis, size = 15),
            axis.ticks = element_line(colour = col.axis),
            axis.text.x = element_text(margin = unit(rep(4, 4), "mm")),
            axis.text.y = element_text(margin = unit(rep(4 ,4), "mm")),
            axis.title.x = element_text(colour = col.axis, size = 20, family = font),
            axis.title.y = element_text(colour = col.axis, size = 20, family = font),
            axis.ticks.x = element_line(linewidth = axis.size),
            axis.ticks.y = element_line(linewidth = axis.size),
            axis.ticks.length = unit(-2.5, "mm")) +
      scale_x_continuous(breaks = seq(-1, maxtime+1, xint), limits = c(0, maxtime+0.2), expand = c(0, 0)) +
      geom_step(mapping = aes(x = leng1, y = s1), stat = "identity", colour = col.arm[1], linewidth = plot.size) +
      geom_step(mapping = aes(x = leng2, y = s2), stat = "identity", colour = col.arm[2], linewidth = plot.size) +
      geom_step(mapping = aes(x = leng3, y = s3), stat = "identity", colour = col.arm[3], linewidth = plot.size)
  }
  
  # === setting Yaxis label (0 to 1, 0% to 100%) ==============================================================
  if(ylim == "prop"){
    panel <- panel + 
      scale_y_continuous(labels = percent, breaks = seq(-1, 1.1, yint), limits = c(0, 1.02), expand = c(0, 0))
  }else{
    panel <- panel +
      scale_y_continuous(breaks=seq(-1, 1.1, yint), limits=c(0, 1.02), expand = c(0, 0))
  }
  
  return(panel)
}


### Save ######################################################################################################################################
# filename <- "Example_KMplot"
# extension <- "jpg"
# save.path <- "C://..."
# dpi <- 600
# ggsave(file = str_c(filename, extension, sep = "."),
#        plot = last_plot(),
#        path = save.path,
#        dpi = dpi)



### Example ###################################################################################################################################
# Kaplan-Meier plot ---
KMplot(csv.path = "C://...",
       col.bg = "#203864",
       col.axis = "white",
       col.arm = c("#59CEE9", "#FF00FF"),
       ylim = "prop")

# Save the plot on the DESKTOP ---
filename <- "JCOGxxxx_ASCO_poster"
extension <- "jpg"
save.path <- "C://desktop"
dpi <- 600
ggsave(file = str_c(filename, extension, sep = "."),
       plot = last_plot(),
       path = save.path,
       dpi = dpi)







### End of Program ###
