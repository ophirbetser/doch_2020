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
data_for_101 <- 
  as.data.table(
    xlsx::read.xlsx(
      '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
      6
    )
  )


data_for_101$dirog_kiboz <- 
  stringi::stri_reverse(data_for_101$dirog_kiboz)

data_for_101$dirog_kiboz <- fct_reorder(data_for_101$dirog_kiboz,
                                        data_for_101$ratio)



plot_for_101 <- 
  data_for_101 %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = ratio) +
  geom_bar(stat = "identity",
           width = 0.6, 
           fill = "#45B39D") +
  geom_text(aes(x = dirog_kiboz, label = round(ratio, 2)),
            size = 6,
            hjust = -0.2,
            family = 'Rubik') +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 5.3),
                     breaks = seq(0, 6, 1)) +
  coord_flip() +
  theme_classic() + 
  easy_remove_x_axis(what = c("line"))






title_size = 40
subtitle_size = 30
x_axis_size = 20
y_axis_size = 18
geom_text_size = 20
caption_size = 15

title_text = "אי שיוויון בים עשירונים לפי דירוג" %>% stringi::stri_reverse()
subtitle_text = paste0('2020 ,', stringi::stri_reverse('היחס בין שכר העשירון העליון לשכר העשירון התחתון, בתי חולים ממשלתיים'))
x_axis_text = ''
y_axis_text = ''
caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')

choose_font_family <- 'Bona-Nova'



file <- 'plot/plot_for_101_2020_1K.png'
ragg::agg_png(file,
              width = 1600,
              height = 1080,
              res = 200,
              units = 'px')


plot_for_101 +
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
        #margin = margin(t = 12),
        family = 'Rubik', size = x_axis_size, 
        color = "gray30"
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
    panel.grid.major.x = element_line(size=1, colour = "gray85"),
    legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
    legend.position = "none",
    plot.margin = margin(c(15,10,5,0))
    
  )


invisible(dev.off())
knitr::include_graphics(file)





