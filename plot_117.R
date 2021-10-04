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

#------------------------------------------------------------------------------#
# script ----
data_for_117 <- 
  as.data.table(
    xlsx::read.xlsx(
      '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
      22
    )
  )

data_for_117

data_for_117$sex <- 
  fct_rev(
    data_for_117$sex
  )

data_for_117$dirog_kiboz <- 
  fct_rev(
    data_for_117$dirog_kiboz
  )

data_for_117$variable <- 
  fct_reorder(
    data_for_117$variable,
    data_for_117$value
  )


plot_for_117 <- 
  data_for_117 %>% 
  ggplot() +
  aes(
    sex, 
    value, 
    fill = variable
    ) +
  geom_bar(
    stat = "identity"
    ) +
  scale_fill_manual(
    values = 
      c(
      "#78281F",
      "#512E5F",
      "#1B4F72",
      "#186A3B",
      "#7E5109"
    )
  ) +
  geom_text(
    aes(
      label = 
        ifelse(
          value > 2500, 
          scales::comma(
            value,
            accuracy = 1
            ),
          NA
          )
      ),
      position = position_stack(vjust = 0.5),
      size = 3.5,
    color = "white",
    fontface = "bold"
    ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 61000),
    breaks = seq(0, 61000, 10000),
    labels= 
      scales::comma(
        seq(0, 61000, 10000),
        accuracy = 1,
        prefix = "₪"
      )
  ) +
  facet_grid(.~ dirog_kiboz) +
  theme_classic() +
  easy_remove_y_axis(what = "line") +
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
        family = 'Rubik', size = 16, 
        color = "black", 
        face = "bold"
      ),
    axis.text.y = 
      element_text(
        family = 'Rubik', size = 12, 
        color = "black",
        face = "bold"
      ),
    plot.subtitle = 
      element_text(
        family = 'Rubik',
        hjust = 1, 
        face="bold",
        size = 18
      ), 
    plot.title = 
      element_text(
        family = 'Rubik',
        hjust = 1,
        size = 32,
        face="bold",
        color = "black"
      ),
    
    strip.background = element_blank(),
    strip.text = 
      element_text(
        family = 'Rubik',
        hjust = 0.5,
        size = 16,
        face="bold",
        color = "black",
        margin = margin(b = 24, t = 8)
      ),
    legend.text = 
      element_text(
        family = 'Rubik',
        size = 10,
        face="bold",
        color = "black",
        margin = margin(l = 20, r = 0, unit = "pt")
      ),
    plot.caption = 
      element_text(
        family = 'Rubik',
        size = 10,
        face="bold",
        color = "black",
        hjust = 0.975,
        margin = margin(t = 20, b = 0, unit = "pt")
      ),
    
    
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
    
    plot.margin = 
      margin(
        b = 3
      ),
    
    legend.margin = margin(0,0,0,0),
    legend.position = c(0.45,-0.08),
    legend.key.height= unit(0.4, 'cm'),
    legend.key.width= unit(0.4, 'cm')
  ) +
  guides(
    fill = 
      guide_legend(
        label.position = "left",
        nrow = 1
      )
  ) 


file <- 'plot/plot_for_117_2020.png'
ragg::agg_png(file,
              width = 4*500,
              height = 3*500,
              res = 200,
              units = 'px')

plot_for_117

invisible(dev.off())
knitr::include_graphics(file)

