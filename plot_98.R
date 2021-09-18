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
data_for_98 <- 
  as.data.table(
  xlsx::read.xlsx(
    '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
    3
  )
)



# סידור הצבע והמיקום של הטקסט
data_for_98$labels_in_plot_place <- 
  ifelse(data_for_98$variable == "ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra",
         data_for_98$value,
         0)
data_for_98$labels_in_plot_color <- 
  ifelse(data_for_98$variable == "ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra",
         "black",
         "white")
data_for_98$mid_in_plot <- 
  ifelse(data_for_98$variable == "ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra",
         data_for_98$mid,
         NA)




# סידור לייבלים אחרון
data_for_98$variable <- factor(data_for_98$variable,
                               levels = 
                                 c(
                                   "ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim",
                                   "ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra"
                                 )
)






data_for_98$dirog_kiboz <- 
  stringi::stri_reverse(data_for_98$dirog_kiboz)


data_for_98$dirog_kiboz <- 
  fct_reorder(data_for_98$dirog_kiboz,
              -data_for_98$value, min)



data_for_98$dirog_kiboz <- 
forcats::fct_relevel(data_for_98$dirog_kiboz,
                     stringi::stri_reverse("מערכת הבריאות"),
                     after = Inf)


  
plot_for_98 <- 
  data_for_98 %>% 
  ggplot() + 
  aes(x=dirog_kiboz, y=value, fill=variable) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.85) +
  geom_point(aes(x = dirog_kiboz,
                 y = mid_in_plot), 
             color = "orange",
             position=position_dodge(width=0.85),
             size = 10,
             shape = 95) +
  geom_text(aes(y = labels_in_plot_place,
                label = scales::comma(value, accuracy = 1),
                color = I(labels_in_plot_color)),
            position=position_dodge(width=-0.85),
            angle = 90,
            hjust=-0.15,
            size = 6,
            #fontface = 'bold',
            family = 'Rubik') +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(40000)),
                     breaks = seq(0, 40000, 5000),
                     labels = scales::comma(seq(0, 40000, 5000),
                                            accuracy = 1,
                                            prefix = "₪")) +
  theme_classic() + 
  easy_remove_y_axis(what = 'line') +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(values = c("#45B39D","#273746"))






title_size = 40
subtitle_size = 30
x_axis_size = 20
y_axis_size = 18
geom_text_size = 20
caption_size = 15

title_text = "שכר ממוצע וחציוני בדירוגים נבחרים" %>% stringi::stri_reverse()
subtitle_text = paste0('2020 ,', stringi::stri_reverse('מערכת הבריאות הממשלתית'))
x_axis_text = ''
y_axis_text = ''
caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')

choose_font_family <- 'Bona-Nova'



file <- 'plot_for_98_2020_1K.png'
ragg::agg_png(file,
              width = 1920,
              height = 1080,
              res = 200,
              units = 'px')

plot_for_98 +
  geom_label(
    aes(
      x = dirog_kiboz,
      y = 0,
      label = "2%"
      ), 
    fill = "#EED5A3",
    color = "black",
    fontface = "bold",
    vjust = 1.2,
    size = 5.5,
    label.r	= unit(0.1, "lines"),
    label.padding = unit(0.15, "lines"),
    label.size = 0,
    nudge_y = 0.01
    ) +
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
        color = "black", face = "bold"
      ),
    plot.subtitle = 
      element_text(
        family = 'Rubik', size = subtitle_size, hjust = 1, 
        color = "black", face = "bold"
      ),
    axis.text.x = 
      element_markdown(
        margin = margin(t = 12),
        family = 'Rubik', size = x_axis_size, 
        color = "gray30",
        angle = 90, hjust = 1
      ),
    axis.text.y = 
      element_text(
        family = 'Rubik', size = y_axis_size, 
        color = "gray30"
      ),
    plot.caption = 
      element_text(
        family = choose_font_family, size = caption_size, 
        color = "black", face = "bold"
      ),
    legend.text = 
      element_text(
        family = 'David-Libre', size = caption_size, 
        color = "black"
      ),
    
    
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major.y = element_line(size=1, colour = "gray85"),
    legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
    legend.position = "none"
    
    
    )


invisible(dev.off())
knitr::include_graphics(file)

