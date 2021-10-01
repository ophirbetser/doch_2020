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

title_size = 20
subtitle_size = 16
x_axis_size = 20
y_axis_size = 18
geom_text_size = 16
caption_size = 18


#------------------------------------------------------------------------------#
# script ----
data_for_99 <- 
  as.data.table(
    xlsx::read.xlsx(
      '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
      4
    )
  )

data_for_99

data_for_99$dirog_kiboz <-
  str_pad(
    data_for_99$dirog_kiboz,
    width = 22,
    side = "both",
    pad = "–"
  )


data_for_99$dirog_kiboz <- 
  fct_reorder(
    data_for_99$dirog_kiboz,
    data_for_99$ratio_sum_num_ovedim
  )


# create plot ----

plot_for_99_1 <- 
  data_for_99 %>% 
  ggplot() +
    aes(
      x =  dirog_kiboz,
      y = -1*ratio_sum_num_ovedim
      ) +
    geom_bar(
      stat = "identity",
      fill = "#283747",
      width = 0.8
      ) +
    geom_text(
      aes(
        x = dirog_kiboz,
        label = scales::percent(
          ratio_sum_num_ovedim,
          0.01
          )
        ),
        hjust=1.1,
        size = 5,
      family = "Rubik",
      fontface = "bold"
      ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(-0.46, 0.008),
      breaks = seq(-1, 0, 0.05),
      labels = 
        scales::percent(
          seq(1, 0.00, -0.05),
          1
        )
      ) +  
    coord_flip() +
    theme_classic() +
    geom_segment(
      aes(
        x = 0.3,
        xend = 17.7,
        y = 0,
        yend = 0
        ),
      size = 1.5) +
  labs(
    title = "אחוז עובדים לפי דירוגים",
    subtitle = "נתונים מאוחדים",
    y = "",
    x = "",
    caption = ""
    ) +
  theme(
    panel.grid.major.x = element_line(size=1.2, colour = "#C0C0C0"),
    plot.title = 
      element_text(
        family = 'Rubik', size = title_size, hjust = 1, 
        color = "black", face = "bold"
      ),
    plot.subtitle = 
      element_text(
        family = 'Rubik', size = subtitle_size, hjust = 1, 
        color = "black", face = "bold"
      ),
    axis.text.x = 
      element_text(
        family = 'Rubik',
        face="bold", 
        color = "black",
        size = 12
        ),
    plot.margin = margin(c(15,0,0,0))
    )
  
plot_for_99_2 <- 
  data_for_99 %>% 
  ggplot() +
    aes(
      x = dirog_kiboz, 
      y = ratio_sum_sal_bruto_shotef_hefrshim
      ) +
    geom_bar(
      stat = "identity",
      fill = "#2E86C1",
      width = 0.8
      ) +
    geom_text(
      aes(
        x = dirog_kiboz,
        label = scales::percent(
          ratio_sum_sal_bruto_shotef_hefrshim,
          0.01
          )
        ),
        hjust=-0.15,
        size = 5,
      family = "Rubik",
      fontface = "bold"
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 0.46),
      breaks = seq(0, 1, 0.05),
      labels = scales::percent(
        seq(0, 1, 0.05),
        1
        )
      ) + 
    coord_flip() +
    theme_classic() +
    geom_segment(
      aes(
        x = 0.3,
        xend = 17.7,
        y = 0,
        yend = 0
        ),
      size = 3
    ) +
    labs(
      title = "התפלגות הוצאות השכר לפי דירוגים",
      subtitle = "נתונים מאוחדים",
      y = "",
      x = "",
      caption = ""
      ) +
    theme(
      panel.grid.major.x = element_line(size=1.2, colour = "#C0C0C0"),
      plot.title = 
        element_text(
          family = 'Rubik', size = title_size, hjust = 1, 
          color = "black", face = "bold"
        ),
      plot.subtitle = 
        element_text(
          family = 'Rubik', size = subtitle_size, hjust = 1, 
          color = "black", face = "bold"
        ),
      plot.margin = margin(c(15,0,0,0)),
      axis.text.y = 
        element_text(
          face="bold",
          color = "black",
          size = 12,
          angle = 0,
          hjust = 0.5
          ),
      axis.text.x = 
        element_text(
          family = 'Rubik',
          face="bold",
          color = "black",
          size = 12
          )
      ) 

(plot_for_99_1 + plot_for_99_2)




# output png ----

#title_size = 28
#subtitle_size = 24
#x_axis_size = 20
#y_axis_size = 18
#geom_text_size = 16
#caption_size = 18
#
#title_text = 'שכר רופאים, לפי דרגה ואזור בית חולים' %>% stringi::stri_reverse()
#subtitle_text = paste0('2020 ,', stringi::stri_reverse('ממוצע למשרה מלאה, נתונים מאוחדים )ממשלתיים, כללית, איכילוב ובני ציון('))
#x_axis_text = ''
#y_axis_text = ''
#caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')
#
#choose_font_family <- 'Bona-Nova'
#

file <- 'plot/plot_for_99_2020_1K.png'
ragg::agg_png(file,
              width = 7*500,
              height = 3*500,
              res = 200,
              units = 'px')

plot_for_99_1 <- 
  plot_for_99_1 +
  easy_remove_x_axis(what = c("line", "ticks", "title")) +
  easy_remove_y_axis(what = c("ticks", "title", "line", "text")) 
  

plot_for_99_2 <- 
  plot_for_99_2 +
  easy_remove_x_axis(what = c("line", "ticks", "title")) +
  easy_remove_y_axis(what = c("ticks", "title", "line")) 


(plot_for_99_1 + plot_for_99_2)


invisible(dev.off())
knitr::include_graphics(file)














