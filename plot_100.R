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
data_for_100 <- 
  as.data.table(
    xlsx::read.xlsx(
      '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
      5
    )
  )



data_for_100$dirog_kiboz <- 
  stringi::stri_reverse(data_for_100$dirog_kiboz)


data_for_100$dirog_kiboz <- 
  fct_reorder(data_for_100$dirog_kiboz, data_for_100$mean)


plot_for_100 <-
  data_for_100 %>% 
  ggplot() +
  geom_segment(aes(y = dirog_kiboz,
                   yend = dirog_kiboz,
                   x = low_mean,
                   xend = high_mean),
               size = 4,
               color = "#60C1A8") +
  geom_point(aes(y = dirog_kiboz,
                 x = mean),
             color = "yellow",
             shape = 15,
             size = 3) +
  geom_point(aes(y = dirog_kiboz,
                 x = median),
             color = "blue",
             shape = 15,
             size = 3) +
  
  geom_text(aes(y = dirog_kiboz,
                x = low_mean,
                label = scales::comma(low_mean, accuracy = 1)),
            hjust = 1.1,
            size = 6,
            family = 'Rubik') +
  geom_text(aes(y = dirog_kiboz,
                x = high_mean,
                label = scales::comma(high_mean)),
            hjust = -0.1,
            size = 6,
            family = 'Rubik') +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 82000),
                     breaks = seq(0, 82000, 10000),
                     labels = scales::comma(seq(0, 82000, 10000),
                                            accuracy = 1,
                                            prefix = "₪")) +
  theme_classic() +
  easy_remove_x_axis(what = c("line", "ticks")) +
  coord_cartesian(clip = 'off')



title_size = 40
subtitle_size = 30
x_axis_size = 18
y_axis_size = 15
geom_text_size = 20
caption_size = 12

title_text = "עשירוני שכר, לפי דירוגים" %>% stringi::stri_reverse()
subtitle_text = paste0('2020 ,', stringi::stri_reverse('מערכת הבריאות הממשלתית'))
x_axis_text = ''
y_axis_text = ''
caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')

choose_font_family <- 'Bona-Nova'



file <- 'plot/plot_for_100_2020_1K.png'
ragg::agg_png(file,
              width = 1920,
              height = 1080,
              res = 200,
              units = 'px')

plot_for_100 +
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
        family = 'Rubik', size = x_axis_size, 
        color = "black"
      ),
    axis.text.y = 
      element_text(
        family = 'Rubik', size = y_axis_size, 
        color = "black", face = "bold"
      ),
    plot.caption = 
      element_text(
        family = 'M-PLUS-Rounded-1c', size = caption_size, 
        color = "black", face = "bold"
      ),
    legend.text = 
      element_text(
        family = 'David-Libre', size = caption_size, 
        color = "black"
      ),
    
    
    axis.line = element_line(colour = "black", size = 1.3),
    legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
    panel.grid.major.x = element_line(size=0.9, colour = "gray85"),
    plot.margin = margin(c(10,30,10,0))
    
    
  )


invisible(dev.off())
knitr::include_graphics(file)

