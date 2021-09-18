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
data_for_97 <- 
  xlsx::read.xlsx(
    '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
    2
  )


data_for_97$label_p_2019 <- 
  ifelse(data_for_97$year == 2020,
         data_for_97$ratio_sub_1,
         NA)

data_for_97$label_sum_misra_2019 <- 
  ifelse(data_for_97$year == 2020,
         data_for_97$sum_halki_misra / 12,
         NA)

data_for_97$dirog_kiboz_for_97 <- 
  stringi::stri_reverse(data_for_97$dirog_kiboz_for_97)


data_for_97$label_text <- 
  glue(
    '{scales::percent(data_for_97$label_p_2020, accuracy = 1)} | {scales::comma(data_for_97$label_sum_misra_2020, accuracy = 1)}'
    )

data_for_97$label_text <- 
ifelse(
  str_detect(data_for_97$label_text, "NA"),
  NA,
  data_for_97$label_text
)

data_for_97$label_vjust <- 
  rep(
    c(
     0.5 ,
     1,
     0.5, 
     -1,
     0.5 
    ),
    times  = 9
  )


plot_for_97 <- 
  data_for_97 %>% 
  ggplot() +
  aes(x = year, y = ratio_sub_1, color = dirog_kiboz_for_97) +
  geom_line(size = 1.5) +
  scale_x_continuous(limits = c(2012, 2020.5),
                     breaks = seq(2011, 2020, 1),
                     labels = seq(2011, 2020, 1)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.0, 0.41),
                     breaks = seq(0, 0.4, 0.05),
                     labels= scales::percent(seq(0, 0.4, 0.05),
                                             accuracy = 1)) +
  geom_text(aes(label= label_text),
            hjust = -0.1,
            vjust = data_for_97$label_vjust,
            size = 6,
            show.legend = F
            ) +

  theme_classic() + 
  #scale_fill_manual(values = c("#273746", "#45B39D")) +
  coord_cartesian(clip = 'off') +
  easy_remove_y_axis(what = c("line", "ticks")) +
  scale_color_manual(values = c("#707B7C", "#58D68D", "#239B56", "#2874A6", "#154360")) 


data_for_97
plot_for_97

title_size = 40
subtitle_size = 30
x_axis_size = 15
y_axis_size = 15
geom_text_size = 15
caption_size = 20

title_text = "שיעור גידול המשרות לפי שנים" %>% stringi::stri_reverse()
subtitle_text = paste0('2009-2019 ,', stringi::stri_reverse('מערכת הבריאות הממשלתית'))
x_axis_text = ''
y_axis_text = ''
caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')

choose_font_family <- 'Bona-Nova'



file <- 'plot/plot_for_97_2020_1K.png'
ragg::agg_png(file,
              width = 1920,
              height = 1080,
              res = 200,
              units = 'px')

plot_for_97 +
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
      element_text(
        family = 'Rubik', size = x_axis_size, 
        color = "black", face = "bold"
      ),
    axis.text.y = 
      element_text(
        family = 'Rubik', size = y_axis_size, 
        color = "black", face = "bold"
      ),
    plot.caption = 
      element_text(
        family = choose_font_family, size = caption_size, 
        color = "black", face = "bold"
      ),
    legend.text = 
      element_text(
        family = 'Rubik', size = geom_text_size, 
        color = "black"
      ),
    
    panel.grid.major.y = element_line(size=1, colour = "#AFAFAF"),
    plot.margin = margin(c(5,30,20,0)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA, colour = NA)
    )


invisible(dev.off())
knitr::include_graphics(file)