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
data_for_116 <- 
  as.data.table(
    xlsx::read.xlsx(
      '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
      21
    )
  )

data_for_116

data_for_116$dirog_kiboz <- 
  stringr::str_wrap(data_for_116$dirog_kiboz, 10)

data_for_116$variable <- 
  fcase(
    data_for_116$variable == "dist_misra", "פער ממוצע למשרה",
    default = "פער ממוצע לעובד"
  )


#data_for_116$dirog_kiboz <- 
#  fct_reorder(
#    data_for_116$dirog_kiboz,
#    -data_for_116$value
#  )
#



data_for_116$dirog_kiboz <- 
fct_reorder2(
  data_for_116$dirog_kiboz,
  fct_rev(data_for_116$variable),
  data_for_116$value
)



plot_for_116 <- 
  data_for_116[dirog_kiboz != "אחר"] %>% 
  ggplot() +
  aes(
    x = dirog_kiboz,
    y = value,
    fill = variable
    ) +
  geom_bar(
    stat = "identity",
    position = position_dodge(0.7),
    width = 0.6
    ) +
  geom_hline(
    yintercept = 0,
    size = 1
    ) +
  geom_text(
    aes(
      y = ifelse(value > 0, value, 0.005), 
      label = scales::percent(
        value,
        accuracy = 1
        )
      ),
      position=position_dodge(width=0.7),
      vjust=-0.25,
      size = 3.8,
    fontface = "bold"
    ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(-0.21, 0.41),
    breaks = seq(-0.2, 0.4, 0.1),
    labels= 
      scales::percent(
        seq(-0.2, 0.4, 0.1),
        accuracy = 1
        )
    ) +
  theme_classic() + 
  scale_fill_manual(
    values = c("#273746", "#45B39D")
    ) +
  labs(
    title = "פערי שכר מגדריים",
    subtitle = "נתונים מאוחדים",
    y = "",
    x = "",
    fill = "",
    caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר"
    ) + 
  theme(
    axis.text.x = 
      element_text(
        family = 'Rubik', size = 10, 
        color = "black", 
        face = "bold",
        angle = 90, hjust = 1, vjust = 0.5
      ),
    axis.text.y = 
      element_text(
        family = 'Rubik', size = 8, 
        color = "black",
        face = "bold"
      ),
    plot.subtitle = 
      element_text(
        family = 'Rubik',
        hjust = 1, 
        face="bold",
        size = 18,
        margin = margin(b = 24)
      ), 
    plot.title = 
      element_text(
        family = 'Rubik',
        hjust = 1,
        size = 36,
        face="bold",
        color = "black"
      ),
    legend.text = 
      element_text(
        family = 'Rubik',
        size = 12,
        face="bold",
        color = "black",
        margin = margin(l = 18, r = 0, unit = "pt")
      ),
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
    
    legend.position = c(0.99, 0.94),
    legend.justification = c("right"),
    legend.key.height= unit(0.4, 'cm'),
    legend.key.width= unit(0.4, 'cm'),
    legend.margin = margin(0,0,0,0)
  
    ) +
  guides(
    fill = 
      guide_legend(
        label.position = "left",
        nrow = 1
      )
  ) +
  easy_remove_x_axis(what = c("line")) +
  easy_remove_y_axis(what = c("line"))


file <- 'plot/plot_for_116_2020.png'
ragg::agg_png(file,
              width = 7*500,
              height = 3*500,
              res = 200,
              units = 'px')

plot_for_116

invisible(dev.off())
knitr::include_graphics(file)

