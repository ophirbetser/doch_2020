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

#------------------------------------------------------------------------------#
# script ----
data_for_103 <- 
  as.data.table(
    xlsx::read.xlsx(
      '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
      8
    )
  )



data_for_103$level_doc <- 
  stringi::stri_reverse(data_for_103$level_doc)

data_for_103$level_doc <- 
  fct_reorder(
    data_for_103$level_doc,
    data_for_103$SAL
  )



plot_for_103 <- 
  data_for_103 %>% ggplot() +
  aes(x = level_doc, y = SAL) +
  geom_bar(stat = "identity",
           width = 0.6,
           fill = "#212F3D",
           color = "white") +
  geom_text(aes(x = level_doc, label = scales::comma(SAL, accuracy = 1)),
            vjust = 0.3,
            hjust = -0.1,
            size = 12,
            color = "black",
            family = 'Rubik') +
  #geom_text(aes(x = level_doc, y = zero,label = scales::comma(NUM_12, accuracy = 1)),
  #          vjust = 0.3,
  #          hjust = -0.2,
  #          size = 3,
  #          color = "#F5B041") +
  geom_segment(aes(y = 0, yend = 0, x = 0.4, xend = 7.6), size = 1) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-1000, 92200),
                     breaks = seq(0, 90000, 10000),
                     labels = scales::comma(seq(0, 90000, 10000),
                                            accuracy = 1,
                                            prefix = "₪")) + 
  coord_flip(clip = 'off')  +
  theme_classic() +
  easy_remove_x_axis(what = c("ticks", "line")) +
  easy_remove_y_axis(what = c("ticks", "line"))



plot_for_103

title_size = 65
subtitle_size = 45
x_axis_size = 30
y_axis_size = 30
geom_text_size = 20
caption_size = 25

title_text = "שכר למשרה של רופא לפי שלב קריירה" %>% stringi::stri_reverse()
subtitle_text = paste0('2020 ,', stringi::stri_reverse('שכר ממוצע למשרה מלאה, מערכת הבריאות הממשלתית'))
x_axis_text = ''
y_axis_text = ''
caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')

choose_font_family <- 'Bona-Nova'



file <- 'plot/plot_for_103_2020_1K.png'
ragg::agg_png(file,
              width = 3000,
              height = 2200,
              res = 200,
              units = 'px')


plot_for_103 +
  geom_label(
    aes(
      y = -600,
      x = level_doc,
      label = str_pad(scales::comma(NUM_12, accuracy = 1), width = 5, side = "both", "."),
      family = "mono"
    ),
    inherit.aes = F,
    fill = "#EED5A3",
    color = "black",
    fontface = "bold",
    vjust = 0.5,
    hjust = 1,
    size = 10,
    label.r	= unit(0.1, "lines"),
    label.padding = unit(0.15, "lines"),
    label.size = 0,
    nudge_y = 0
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
        #margin = margin(t = 12),
        family = 'Rubik', size = x_axis_size, 
        color = "gray30"
      ),
    axis.text.y = 
      element_text(
        margin = margin(r = 50),
        family = 'Rubik', size = y_axis_size, 
        color = "gray30"
      ),
    plot.caption = 
      element_text(
        family = choose_font_family, size = caption_size, 
        color = "black", face = "bold"
      ),

    
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major.x = element_line(size=1, colour = "gray85"),
    legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
    legend.position = "none",
    plot.margin = margin(c(25,15,5,0))
    
  )

invisible(dev.off())
knitr::include_graphics(file)









