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
  DT,
  visdat,
  ophiR,
  shiny)

pacman::p_load(
  pacman, 
  ggplot,
  ggeasy,
  patchwork,
  esquisse,
  glue,
  ggtext,
  showtext,
  ragg
)

#------------------------------------------------------------------------------#
# script ----

data_for_102 <- 
as.data.table(
openxlsx::readWorkbook(
  '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
  '102'
)
)

data_for_102 %>% dt_



data_for_102$dirog_kiboz <- 
  stringi::stri_reverse(
  data_for_102$dirog_kiboz
)


#data_for_102$dirog_kiboz <- 
#str_replace_all(data_for_102$dirog_kiboz, " ", "\n")

data_for_102$dirog_kiboz <- 
  fct_reorder(
    data_for_102$dirog_kiboz,
    data_for_102$mean_h
  )



plot_for_102 <- 
  data_for_102 %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = ratio, fill = halki_misra_chank) +
  geom_bar(stat = "identity",
           position = "fill",
           width = 0.7) +
  geom_point(aes(x = dirog_kiboz, y = mean_h),
             color = "#F0C92C",
             show.legend = F,
             size = 3) +
  #geom_text(aes(label = scales::percent(ratio, 1)),
  #          position=position_fill(vjust = .5),
  #          size = 2.8) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.07),
                     breaks = seq(0, 1, 0.1),
                     labels = scales::percent(seq(0, 1, 0.1), 1)) + 
  
  theme_classic() +
  scale_fill_manual(values = c("#2E86C1", "#1D8348", "#7DCEA0", "#1C2833", "#566573")) +
  coord_cartesian(clip = 'off') +
  easy_remove_y_axis(what = "line")



title_size = 32
subtitle_size = 22
x_axis_size = 15
y_axis_size = 15
geom_text_size = 10
caption_size = 15

title_text = "היקף העסקה בדירוגים נבחרים" %>% stringi::stri_reverse()
subtitle_text = paste0('2020 ,', stringi::stri_reverse('מערכת הבריאות, נתונים מאוחדים'))
x_axis_text = ''
y_axis_text = ''
caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')

choose_font_family <- 'Bona-Nova'


file <- 'plot/plot_for_102_2020_1K.png'
ragg::agg_png(file,
        width = 1920,
        height = 1080,
        res = 200,
        units = 'px')

plot_for_102 +
  geom_label(
    aes(
      x = dirog_kiboz,
      y = mean_h,
      label = scales::percent(mean_h, accuracy = 1)  
    ), 
    fill = "#F0C92C",
    color = "black",
    #fontface = "bold",
    size = 5.5,
    label.r	= unit(0.1, "lines"),
    label.padding = unit(0.1, "lines"),
    label.size = 0,
    nudge_y = 0.045
  ) +
  geom_segment(x =20.1, xend = -0.1, y = 1.17, yend = 1.17) +

  # use the 's_gg_plot_parms' snippet
  # and 's_gg_google_fonts'
    labs(
      title = title_text,
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
            color = "black", face = "bold",
            margin=margin(0,0,8,0)
            
          ),
        plot.subtitle = 
          element_text(
            family = 'Rubik', size = subtitle_size, hjust = 1, 
            color = "black",
            margin=margin(0,0,10,0)
          ),
        axis.text.x = 
          element_text(
            family = 'Rubik', size = x_axis_size, 
            color = "black", 
            angle = 90, hjust = 1
          ),
        axis.text.y = 
          element_text(
            family = 'Rubik', size = y_axis_size, 
            color = "black"
          ),
        plot.caption = 
          element_text(
            family = 'Rubik', size = caption_size, 
            color = "black"
          ),
        legend.text = 
          element_text(
            family = 'Rubik', size = 12, 
            color = "black"
          ),

        
        axis.ticks = element_line(linetype = "blank"),
        axis.line = element_line(colour = "black", size = 1),
        legend.background = element_rect(color = NA),
        panel.grid.major.y = element_line(size=0.9, colour = "gray85"),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        
        
        legend.justification = c("right", "top"),
        legend.margin = margin(6, 6, 6, 20)
        
      )


invisible(dev.off())
knitr::include_graphics(file)







