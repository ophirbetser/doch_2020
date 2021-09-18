#------------------------------------------------------------------------------#
# script info and options ----
#### File Name:   ##########                 
#### Date:        2021-09-17           
#### Author:      Ophir Betser              
#### Author Mail: ophir.betser@gmail.com    
#### phone number: 053-4209480

# get data

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
data_for_96 <- 
xlsx::read.xlsx(
  '/Users/ophirbetser/Ophir/doch_2020/output_all_health_2020_2021_08_27.xlsx',
  1
)


data_for_96$sal_chank_mean <- 
  fcase(data_for_96$sal_chank_mean == '6K-', "עד ₪6,000",
        data_for_96$sal_chank_mean == '6K-9K', "₪9,000 - ₪6,000",
        data_for_96$sal_chank_mean == '9K-12K', "₪12,000 - ₪9,000",
        data_for_96$sal_chank_mean == '12K-15K', "₪15,000 - ₪12,000",
        data_for_96$sal_chank_mean == '15K-20K', "₪20,000 - ₪15,000",
        data_for_96$sal_chank_mean == '20K-25K', "₪25,000 - ₪20,000",
        data_for_96$sal_chank_mean == '25K-30K', "₪30,000 - ₪25,000",
        data_for_96$sal_chank_mean == '30K-40K', "₪40,000 - ₪30,000",
        data_for_96$sal_chank_mean == '40K-50K', "₪50,000 - ₪40,000",
        data_for_96$sal_chank_mean == '50K-60K', "₪60,000 - ₪50,000",
        data_for_96$sal_chank_mean == '60k+', "מעל ₪60,000")



data_for_96$sal_chank_mean <- factor(data_for_96$sal_chank_mean,
                                     levels = c("עד ₪6,000", 
                                                "₪9,000 - ₪6,000",
                                                "₪12,000 - ₪9,000",
                                                "₪15,000 - ₪12,000",
                                                "₪20,000 - ₪15,000",
                                                "₪25,000 - ₪20,000",
                                                "₪30,000 - ₪25,000",
                                                "₪40,000 - ₪30,000",
                                                "₪50,000 - ₪40,000",
                                                "₪60,000 - ₪50,000",
                                                "מעל ₪60,000"))





# https://fonts.google.com/?subset=hebrew
font_add_google(family="karantina", "Karantina")
font_add_google(family="Bona-Nova", "Bona Nova")
font_add_google(family="M-PLUS-1p", "M PLUS 1p")
font_add_google(family="David-Libre", "David Libre")
font_add_google(family="Tinos", "Tinos")
font_add_google(family="M-PLUS-Rounded-1c", "M PLUS Rounded 1c")
font_add_google(family="Amatic-SC", "Amatic SC")

showtext_auto()

title_size = 50
subtitle_size = 35
x_axis_size = 20
y_axis_size = 20
geom_text_size = 20
caption_size = 30

title_text = "התפלגות העובדים לפי רמות שכר" %>% stringi::stri_reverse()
subtitle_text = paste0('2009-2019 ,', stringi::stri_reverse('שכר למשרה מלאה, מערכת הבריאות הממשלתית'))
x_axis_text = ''
y_axis_text = ''
caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')

choose_font_family <- 'Bona-Nova'

plot_for_96 <- 
  data_for_96 %>% 
  ggplot() + 
  aes(x = sal_chank_mean, y = sum_ovedim_p) +
  geom_bar(stat = "identity",
           width = 1,
           fill = "#212F3D",
           color = "white") +
  geom_text(aes(x = sal_chank_mean, label = scales::percent(sum_ovedim_p, accuracy = 0.1)),
            vjust=-0.25,
            size = 8,
            family = 'David-Libre') +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.31), 
                     breaks = seq(0, 0.5, 0.05),
                     labels= scales::percent(seq(0, 0.5, 0.05),
                                             accuracy = 0.1,
                                             suffix = "%")) +
  scale_x_discrete(guide = guide_axis(angle = 90))  +
  theme_classic() +
  easy_remove_y_axis(what = c("line", "ticks"))



file <- 'plot_for_96_2020_1K.png'
ragg::agg_png(file,
        width = 1920,
        height = 1080,
        res = 200,
        units = 'px')

plot_for_96 +
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
            family = choose_font_family, size = title_size, hjust = 1, 
            color = "black", face = "bold"
          ),
        plot.subtitle = 
          element_text(
            family = choose_font_family, size = subtitle_size, hjust = 1, 
            color = "black", face = "bold"
          ),
        axis.text.x = 
          element_text(
            family = 'David-Libre', size = x_axis_size, 
            color = "black", face = "bold"
          ),
        axis.text.y = 
          element_text(
            family = 'David-Libre', size = y_axis_size, 
            color = "black", face = "bold"
          ),
        plot.caption = 
          element_text(
            family = choose_font_family, size = caption_size, 
            color = "black", face = "bold"
          ),
        
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid")
      )


invisible(dev.off())
knitr::include_graphics(file)




#  labs(title = "התפלגות העובדים לפי רמות שכר",
#       subtitle = "שכר למשרה מלאה, מערכת הבריאות הממשלתית, 2009-2019",
#       y = "",
#       x = "",
#       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
#  theme(
#    axis.text.x = element_text(face="bold", size = 10, color = "black"),
#    axis.text.y = element_text(face="bold", size = 10, color = "black"),
#    axis.ticks = element_line(linetype = "blank"),
#    legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
#    plot.subtitle = element_text(hjust = 1, size = 12), 
#    plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
#    panel.grid.major.y = element_line(size=0.5, colour = "#939393")
#  )

# 4K ----


title_size = 100
subtitle_size = 70
x_axis_size = 40
y_axis_size = 40
geom_text_size = 40
caption_size = 30

title_text = "התפלגות העובדים לפי רמות שכר" %>% stringi::stri_reverse()
subtitle_text = paste0('2009-2019 ,', stringi::stri_reverse('שכר למשרה מלאה, מערכת הבריאות הממשלתית'))
x_axis_text = ''
y_axis_text = ''
caption_text = stringi::stri_reverse('מקור: אגף שכר והסכמי עבודה, משרד האוצר')

choose_font_family <- 'Bona-Nova'

plot_for_96 <- 
  data_for_96 %>% 
  ggplot() + 
  aes(x = sal_chank_mean, y = sum_ovedim_p) +
  geom_bar(stat = "identity",
           width = 1,
           fill = "#212F3D",
           color = "white") +
  geom_text(aes(x = sal_chank_mean, label = scales::percent(sum_ovedim_p, accuracy = 0.1)),
            vjust=-0.25,
            size = 16,
            family = 'David-Libre') +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.31), 
                     breaks = seq(0, 0.5, 0.05),
                     labels= scales::percent(seq(0, 0.5, 0.05),
                                             accuracy = 0.1,
                                             suffix = "%")) +
  scale_x_discrete(guide = guide_axis(angle = 90))  +
  theme_classic() +
  easy_remove_y_axis(what = c("line", "ticks"))


file <- 'plot_for_96_2020_4K.png'
ragg::agg_png(file,
              width = 3840,
              height = 2160,
              res = 200,
              units = 'px')

plot_for_96 +
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
        family = choose_font_family, size = title_size, hjust = 1, 
        color = "black", face = "bold"
      ),
    plot.subtitle = 
      element_text(
        family = choose_font_family, size = subtitle_size, hjust = 1, 
        color = "black", face = "bold"
      ),
    axis.text.x = 
      element_text(
        family = 'David-Libre', size = x_axis_size, 
        color = "black"
      ),
    axis.text.y = 
      element_text(
        family = 'Amatic-SC', size = y_axis_size, 
        color = "black", face = "bold"
      ),
    plot.caption = 
      element_text(
        family = choose_font_family, size = caption_size, 
        color = "black", face = "bold"
      ),
    
    panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
    legend.background = element_rect(fill = NA, colour = "black", linetype = "solid")
  )


invisible(dev.off())
knitr::include_graphics(file)




#------------------------------------------------------------------------------#
# usethis ----
esquisse::esquisser(viewer = "browser")
usethis::edit_rstudio_snippets()

#------------------------------------------------------------------------------#
# clearing environment ----
p_unload(all)   # Clear packages                  
dev.off()       # Clear plots      
cat("\014")     # Clear console 
rm(list = ls()) # Clear environment                           
#.rs.restartR() # restart R studio                    

#------------------------------------------------------------------------------#