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
  ggnewscale,
  ggtext,
  showtext,
  ragg
)

geom_text_size_small <- 12
geom_text_size_big <- 32

v <- rep("שכר", 32)
#------------------------------------------------------------------------------#
# script ----
data_for_115 <- 
  as.data.table(
    xlsx::read.xlsx(
      '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
      20
    )
  )

data_for_115

data_for_115$v <- v

data_for_115$sex <- 
  factor(
    data_for_115$sex,
    levels = c(
     "נשים",
     "גברים"
     )
   )

data_for_115$dirog_kiboz <- 
  stringr::str_wrap(data_for_115$dirog_kiboz, 10)

data_for_115$dirog_kiboz <- 
  fct_reorder(
    data_for_115$dirog_kiboz,
    -data_for_115$tot_count
  )


data_for_115$ratio_top <- 
  ifelse(
    data_for_115$sex == "נשים",
    data_for_115$ratio,
    NA
  )
data_for_115$ratio_bottom <- 
  ifelse(
    data_for_115$sex == "נשים",
    NA,
    data_for_115$ratio
  )


plot_for_115 <- 
  data_for_115 %>% 
  ggplot() + 
    aes(
      x = dirog_kiboz,
      y = ratio,
      fill = sex
      ) +
    geom_bar(
      stat = "identity",
      position = "stack"
      ) +
  geom_text(
    aes(
      x = dirog_kiboz,
      y = 0.97,
      label = 
        scales::percent(
          ratio_top,
          accuracy = 1
        )
    ),
    size = 2.8,
    fontface = "bold",
    color = "black"
  ) +
  geom_text(
    aes(
      x = dirog_kiboz,
      y = 0.015,
      label = 
        scales::percent(
          ratio_bottom,
          accuracy = 1
        )
    ),
    size = 2.8,
    fontface = "bold",
    color = "white"
  ) +
  
  scale_fill_manual(
    values = c(
      "#D68910",
      "#273746"
    )
  ) +
  new_scale_fill()+
  # הוספת הטקסט מתחת לתחילת הגרף
  geom_label(
    aes(
      x = dirog_kiboz,
      y = 0,
      label = 
        scales::comma(
          count,
          accuracy = 1
        ),
      fill = v
    ),
    color = "black",
    fontface = "bold",
    vjust = 1.2,
    size = 2.5,
    label.r	= unit(0.1, "lines"),
    label.padding = unit(0.15, "lines"),
    label.size = 0,
    nudge_y = 0
    ) +
  scale_fill_manual(
    values = c(
      "#EED5A3"
    )
  ) +
      
  #geom_text(aes(x = dirog_kiboz,
    #              y = one,
    #              label = round(ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim)),
    #          vjust = 1,
    #          size = 4,
    #          color = "black") +
    #
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1.07), 
      breaks = seq(0, 1, 0.1),
      labels= 
        scales::percent(
          seq(0, 1, 0.1),
          accuracy = 1,
          suffix = "%"
          )
      ) +
    labs(
      title = "התפלגות מגדרית לפי דירוגים שונים",
      subtitle = "נתונים מאוחדים",
      y = "",
      x = "",
      caption = "מקור: אגף שכר והסכמי עבודה משרד האוצר",
      fill = ""
      ) +
    theme_classic() +
  easy_remove_y_axis(what = c("ticks", "title", "line")) +
  easy_remove_x_axis(what = c("ticks")) +
  theme(
      axis.text.x = 
        element_text(
          margin = margin(t = 14),
          family = 'Rubik', size = 8, 
          color = "black", 
          face="bold",
          angle = 90, hjust = 1, vjust = 0.5
        ),
      axis.text.y = 
        element_text(
          family = 'Rubik', size = 8, 
          face="bold",
          color = "black"
        ),
      #legend.background = element_rect(linetype = "blank"),
      plot.subtitle = 
        element_text(
          family = 'Rubik',
          hjust = 1, 
          size = 12
          ), 
      plot.title = 
        element_text(
          family = 'Rubik',
          hjust = 1,
          size = 16,
          face="bold",
          color = "black"
          ),
      plot.margin = margin(c(8,5,0,5)),
      axis.line = element_line(colour = "black", size = 0.65),
      panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
      legend.position = c(0.05, 1.08),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      #legend.justification = c("right", "top"),
      #legend.box.just = "right",
      legend.box = "horizontal",
      legend.margin = margin(0,0,0,0),
      #legend.key.height= unit(0.7, 'cm'),
      #legend.key.width= unit(1, 'cm'),
      legend.text = 
        element_text(
          family = 'Rubik',
          size=8
          ),
      legend.title=element_blank()
      ) +
  guides(
    fill = 
      guide_legend(
        override.aes = aes(label = ""),
        label.position = "top",
        nrow = 1
      ),
    colour = 
      guide_legend(
        label.position = "top",
        nrow = 1
      )
  ) +
  coord_cartesian(clip = 'off') +
  geom_segment(
      x =  16.2,
      xend = 0.8,
      y =  -0.055,
      yend =-0.055,
      arrow = arrow(length = unit(0.15, "cm"))
    ) 
  
  

    
  
data_for_115 %>% names()

plot_for_115

file <- 'plot/plot_for_115_2020_1K.png'
ragg::agg_png(file,
              width = 1920,
              height = 1080,
              res = 200,
              units = 'px')

plot_for_115

invisible(dev.off())
knitr::include_graphics(file)


