#------------------------------------------------------------------------------#  
# load packages and set environment ----
options(scipen=999) # Disables scientific notation          
options(digits=6)   # Limits the number of digits printed       
#Sys.setlocale("LC_ALL", locale = "Hebrew")

if (!require("pacman")){                                  
  install.packages("pacman")}                            
pacman::p_load(
  pacman, 
  data.table,
  tidyverse,
  patchwork,
  visdat,
  ophiR,
  DT,
  stringi,
  shiny)

pacman::p_load(
  pacman, 
  ggplot2,
  ggeasy,
  patchwork,
  esquisse,
  glue,
  ggtext,
  showtext,
  ragg
)

geom_text_size_small <- 12
geom_text_size_big <- 32

#------------------------------------------------------------------------------#
# script ----
data_for_114 <- 
  as.data.table(
    xlsx::read.xlsx(
      '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
      19
    )
  )

data_for_114_1 <- 
  data_for_114[1:2]

data_for_114_2 <- 
  data_for_114[4:5]

setnames(
  data_for_114_2,
  "mean_sal",
  "ratio"
)

data_for_114_1$mean_sal <- as.numeric(data_for_114_1$mean_sal)
data_for_114_2$ratio <- as.numeric(data_for_114_2$ratio)



plot_for_114_2 <- 
  data_for_114_2 %>% 
  ggplot() +
    aes(x = stringi::stri_reverse(sex), y = ratio) +
    geom_bar(stat = "identity",
             width = 0.4,
             fill = c("#273746", "#D68910")) +
    geom_text(
      aes(
        x = stringi::stri_reverse(sex),
        label = scales::percent(ratio)
        ),
         position = position_stack(vjust = 0.5),
         vjust = 1,
         family = 'Rubik',
         size = geom_text_size_big,
         color = "#FBFCFC") +
    geom_text(
      aes(
        x = stringi::stri_reverse(sex),
        label = stringi::stri_reverse(sex)
      ),
         position = position_stack(vjust = 0.5),
         vjust = -1,
         family = 'Rubik',
         size = geom_text_size_small,
         fontface = "bold",
         color = "#FBFCFC") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 0.72),
                       breaks = seq(0, 0.7, 0.1),
                       labels= scales::percent(seq(0, 0.7, 0.1),
                                               accuracy = 1)) + 
    theme_classic() +
    coord_flip() +
    easy_remove_x_axis(what = c("line"))

  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.x = element_line(size=0.5, colour = "#939393"))



plot_for_114_1 <- 
  data_for_114_1 %>% 
    ggplot() +
    aes(x = stringi::stri_reverse(sex), y = mean_sal) +
    geom_bar(stat = "identity",
             width = 0.4,
             fill = c("#273746", "#D68910")) +
    geom_text(
      aes(
        x = stringi::stri_reverse(sex),
        label = scales::comma(mean_sal,
                              accuracy = 1, prefix = "₪")
        ),
         position = position_stack(vjust = 0.5),
        vjust = 1,
         family = 'Rubik',
         size = geom_text_size_big,
         color = "#FBFCFC") +
    geom_text(
      aes(
        x = stringi::stri_reverse(sex),
        label = stringi::stri_reverse(sex)
        ),
         position = position_stack(vjust = 0.5),
         fontface = "bold",
         vjust = -1,
         family = 'Rubik',
         size = geom_text_size_small,
         color = "#FBFCFC") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 28000),
                       breaks = seq(0, 28000, 5000),
                       labels= scales::comma(seq(0, 25000, 5000),
                                             accuracy = 1, prefix = "₪")) +
  theme_classic() +
    coord_flip() + 
    easy_remove_x_axis(what = c("line"))
  


  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.x = element_line(size=0.5, colour = "#939393"))
  






title_size = 50
subtitle_size = 40
x_axis_size = 28
y_axis_size = 28
geom_text_size = 30
caption_size = 25

title_text_1 = 'שכר ממוצע למגדר' %>% stringi::stri_reverse()
title_text_2 = 'שיעור עובדים לפי מגדר' %>% stringi::stri_reverse()
subtitle_text = paste0('2020 ,', stringi::stri_reverse('נתונים מאוחדים )ממשלתיים, כללית, איכילוב ובני ציון('))

x_axis_text = ''
y_axis_text = ''
caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')



choose_font_family <- 'Bona-Nova'


file <- 'plot/plot_for_114_2020_1K.png'
ragg::agg_png(file,
              width = 4000,
              height = 1500,
              res = 200,
              units = 'px')

plot_for_114_1 <- 
plot_for_114_1 +
  # use the 's_gg_plot_parms' snippet
  # and 's_gg_google_fonts'
  labs(
    title = title_text_1,
    subtitle = subtitle_text,
    y = x_axis_text,
    x = y_axis_text,
    color = "",
    fill = "",
    size = ""
  ) +
  theme(
    plot.title = 
      element_text(
        family = 'Rubik', size = title_size, hjust = 1, 
        color = "black", face = "bold"
      ),
    plot.subtitle = 
      element_text(
        family = 'Rubik', size = subtitle_size, hjust = 1, 
        color = "black"
      ),
    axis.text.x = 
      element_markdown(
        #margin = margin(t = 12),
        family = 'Rubik', size = x_axis_size, 
        color = "gray30", face = "bold"
      ),
    plot.caption = 
      element_text(
        family = choose_font_family, size = caption_size, 
        color = "black", face = "bold"
      ),

    
    axis.line = element_line(colour = "black", size = 1),
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major.x = element_line(size=1, colour = "gray85"),
    legend.position = "bottom", legend.direction = "horizontal",
    legend.background = element_rect(linetype = "blank"),
    legend.title=element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(c(10,20,0,40))
    
    #axis.ticks = element_line(linetype = "blank"),
    #panel.grid.major.x = element_line(size=1, colour = "gray85"),
    #legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
    #legend.position = "none",
    #plot.margin = margin(c(25,15,5,0))
    
  )


plot_for_114_2 <- 
  plot_for_114_2 +
  # use the 's_gg_plot_parms' snippet
  # and 's_gg_google_fonts'
  labs(
    title = title_text_2,
    subtitle = subtitle_text,
    y = x_axis_text,
    x = y_axis_text,
    caption = caption_text,
    color = "",
    fill = "",
    size = ""
  ) +
  theme(
    plot.title = 
      element_text(
        family = 'Rubik', size = title_size, hjust = 1, 
        color = "black", face = "bold"
      ),
    plot.subtitle = 
      element_text(
        family = 'Rubik', size = subtitle_size, hjust = 1, 
        color = "black"
      ),
    axis.text.x = 
      element_markdown(
        #margin = margin(t = 12),
        family = 'Rubik', size = x_axis_size, 
        color = "gray30", face = "bold"
      ),
    plot.caption = 
      element_text(
        family = choose_font_family, size = caption_size, 
        color = "black", face = "bold"
      ),
    legend.text = 
      element_text(
        family = 'Rubik', size = caption_size, 
        color = "black"
      ),
    
    axis.line = element_line(colour = "black", size = 1),
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major.x = element_line(size=1, colour = "gray85"),
    legend.position = "bottom", legend.direction = "horizontal",
    legend.background = element_rect(linetype = "blank"),
    legend.title=element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(c(10,40,0,20))
    
    
    
    #axis.ticks = element_line(linetype = "blank"),
    #panel.grid.major.x = element_line(size=1, colour = "gray85"),
    #legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
    #legend.position = "none",
    #plot.margin = margin(c(25,15,5,0))
    
  )


plot_for_114_1 + plot_for_114_2

invisible(dev.off())
knitr::include_graphics(file)

