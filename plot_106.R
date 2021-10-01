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
data_for_106 <- 
  as.data.table(
    xlsx::read.xlsx(
      '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
      11
    )
  )

data_for_106


data_for_106$ezor_agaf <- 
  stringi::stri_reverse(data_for_106$ezor_agaf)

data_for_106$ezor_agaf %>% unique()

data_for_106$ezor_agaf <- 
  factor(
    data_for_106$ezor_agaf,
    levels = c(
      "זכרמ",
      "3+2 הירפירפ",
      "1 הירפירפ"
      
    )
  )

data_for_106$level_doc_for_106 <- 
  stringi::stri_reverse(data_for_106$level_doc_for_106)

data_for_106$level_doc_for_106 %>% unique()

data_for_106$level_doc_for_106 <- 
  factor(
    data_for_106$level_doc_for_106,
    levels = c(
      "החמתמ",
      "ימוחת",
      "ריעצ החמומ",
      "ריכב החמומ",
      "להנמ"
    )
  )


plot_for_106 <- 
  data_for_106 %>% 
  ggplot() +
  aes(x = level_doc_for_106, y = SAL, fill = ezor_agaf) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"),
           width = 0.6) +
  geom_text(aes(x = level_doc_for_106, y = 0, label = scales::comma(SAL, accuracy = 1)),
            position=position_dodge(width = 0.6),
            angle = 90,
            vjust = 0.5,
            hjust = -0.2,
            color = "white",
            family = 'Rubik', 
            size = 7) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 80500),
                     breaks = seq(0, 80000, 10000),
                     labels= scales::comma(seq(0, 80000, 10000),
                                           accuracy = 1, prefix = "₪")) +
  theme_classic() +
  scale_fill_manual(values = c("#17A589", "#7FB3D5", "#2471A3", "#212F3D")) +
  easy_remove_y_axis(what = "line")


plot_for_106


title_size = 28
subtitle_size = 24
x_axis_size = 20
y_axis_size = 18
geom_text_size = 16
caption_size = 18

title_text = 'שכר רופאים, לפי דרגה ואזור בית חולים' %>% stringi::stri_reverse()
subtitle_text = paste0('2020 ,', stringi::stri_reverse('ממוצע למשרה מלאה, נתונים מאוחדים )ממשלתיים, כללית, איכילוב ובני ציון('))
x_axis_text = ''
y_axis_text = ''
caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')

choose_font_family <- 'Bona-Nova'


file <- 'plot/plot_for_106_2020_1K.png'
ragg::agg_png(file,
        width = 1920,
        height = 1080,
        res = 200,
        units = 'px')

plot_for_106 +
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
        family = 'Rubik', size = caption_size, 
        color = "black"
      ),
    
    
    
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major.y = element_line(size=1, colour = "gray85"),
    legend.position = "bottom", legend.direction = "horizontal",
    legend.background = element_rect(linetype = "blank"),
    legend.title=element_blank()
    
    
    #axis.ticks = element_line(linetype = "blank"),
    #panel.grid.major.x = element_line(size=1, colour = "gray85"),
    #legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
    #legend.position = "none",
    #plot.margin = margin(c(25,15,5,0))
    
  )


invisible(dev.off())
knitr::include_graphics(file)

