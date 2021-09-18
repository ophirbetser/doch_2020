
# התאמת סביבת העבודה
# והעבודה עם עברית ב 
# R
options(scipen=999) # Disables scientific notation          
options(digits=6)   # Limits the number of digits printed       

Sys.setlocale("LC_ALL", locale = "Hebrew")
setwd("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA")


# טעינת ספריות רלוונטיות
pacman::p_load(pacman, data.table, patchwork, dplyr,
               ggplot2, dplyr, visdat, DT, ggeasy,
               lubridate, shiny, esquisse, ggrepel,
               forcats, openxlsx, ggtext,
               glue, showtext, stringr)


# ניקוי סביבת העבודה
rm(list = ls())

# תחילת עבודה 
# script ----

# טעינת כל קבצי הנתונים של בתי החולים הממשלתיים:
dt_20 <- fread("mem\\ישן\\health_2020_data_21_08_15.csv", 
               encoding = "UTF-8")

dt_19 <- fread("mem\\ישן\\health_2019_data_21_08_02.csv", 
            encoding = "UTF-8")
dt_18 <- fread("mem\\ישן\\health_2018_data_21_08_01.csv", 
               encoding = "UTF-8")
dt_17 <- fread("mem\\ישן\\health_2017_data_21_08_01.csv", 
               encoding = "UTF-8")
dt_16 <- fread("mem\\ישן\\health_2016_data_21_08_01.csv", 
               encoding = "UTF-8")
dt_15 <- fread("mem\\ישן\\health_2015_data_21_08_01.csv", 
               encoding = "UTF-8")
dt_14 <- fread("mem\\ישן\\health_2014_data_21_08_01.csv", 
               encoding = "UTF-8")
dt_13 <- fread("mem\\ישן\\health_2013_data_21_08_01.csv", 
               encoding = "UTF-8")
dt_12 <- fread("mem\\ישן\\health_2012_data_21_08_01.csv", 
               encoding = "UTF-8")
dt_11 <- fread("mem\\ישן\\health_2011_data_21_08_01.csv", 
               encoding = "UTF-8")
dt_10 <- fread("mem\\ישן\\health_2010_data_21_08_01.csv", 
               encoding = "UTF-8")
dt_09 <- fread("mem\\ישן\\health_2009_data_21_08_01.csv", 
               encoding = "UTF-8")


# עמודה 14 מכילה תאריכי לידה, אני מסיר אותה-
# כי היא עושה לי בעיות באיחוד
# של הנתונים לסט נתונים אחד

# בנוסף ב 19 ו 20 יש עמודה שאין באחרים כגע
# כמובן שיש לעדכן את השליפות ואז העמודה תהיה
# [1] "תוספות"

# שהיא מספר
#
dt_all <- rbind(dt_09[, -14], 
                dt_10[, -14], 
                dt_11[, -14], 
                dt_12[, -14], 
                dt_13[, -14], 
                dt_14[, -14], 
                dt_15[, -14], 
                dt_16[, -14], 
                dt_17[, -14], 
                dt_18[, -14], 
                dt_19[, -c(14, 25)], 
                dt_20[, -c(14, 25)])


# שינוי פורמט התאריך לפורמט נכון
dt_all$`תאריך שכר` <- as_date(dt_all$`תאריך שכר`)

# קצת סטטיסטיקה ראשונית על הנתונים
#view(descr(dt)) 
#view(dfSummary(dt)) 
#------------------------------------------------------------------------------#

# עידכון שמות העמודות של סט הנתונים ----
old_names <- copy(names(dt_all))
names_dict <- data.frame(old = old_names,
                         new = old_names)
names_dict %>% datatable()

setnames(dt_all, "מספר עובדים", "num_ovedim")
setnames(dt_all, "ת.ז. של עובד", "id")
setnames(dt_all, "תאריך שכר", "date")
setnames(dt_all, "מגדר", "sex")
setnames(dt_all, "אגף", "agaf")
setnames(dt_all, "קוד אגף", "code_agaf")

setnames(dt_all, "משכורת ברוטו שוטף", "sal_bruto_shotef")
setnames(dt_all, "משכורת ברוטו שוטף והפרשים", "sal_bruto_shotef_hefrshim")
setnames(dt_all, "יסוד משולב", "isod_msolav")
setnames(dt_all, "עבודה נוספת", "avoda_noseft")
setnames(dt_all, "החזר הוצאות", "ehzer_hozaot")
setnames(dt_all, "תשלומים אחרים", "taclomim_aharim")

setnames(dt_all, "פרישה עשירונים סכום", "asironim")
setnames(dt_all, "מענקים סכום", "mankim")

setnames(dt_all, "חלקיות חודש", "halki_hodesh")
setnames(dt_all, "חלקיות חודש משרה", "halki_hodesh_misra")
setnames(dt_all, "חלקיות משרה", "halki_misra")

setnames(dt_all, "קוד דירוג", "code_dirog")
setnames(dt_all, "דירוג", "dirog")
setnames(dt_all, "קוד דרגה", "code_darga")
setnames(dt_all, "דרגה", "darga")


names_dict$new <- names(dt_all)
names_dict %>% datatable()


#------------------------------------------------------------------------------#
dt_all$year <- year(dt_all$date)


dt_all$dirog_kiboz <- fcase(
                        dt_all$code_dirog == 1, "מנהלי",
                        dt_all$code_dirog %in% c(7, 8), "רנטגנאים",
                        dt_all$code_dirog %in% c(33, 43), "עובדי מעבדה",
                        dt_all$code_dirog %in% c(47, 48, 548), "פארה רפואיים",
                        dt_all$code_dirog %in% c(87, 587), "אקדמאים בחוזים מיוחדים",
                        dt_all$code_dirog %in% c(148, 948), "מינהל ומשק",
                        dt_all$code_dirog %in% c(557, 57, 566, 568, 569), "בכירים",
                        dt_all$code_dirog %in% c(964, 164, 555), "מומחים",
                        dt_all$code_dirog == 11, "אקדמאים בהסכם קיבוצי",
                        dt_all$code_dirog == 12, "מהנדסים",
                        dt_all$code_dirog == 13, "הנדסאים וטכנאים",
                        dt_all$code_dirog == 16, "פסיכולוגים",
                        dt_all$code_dirog == 20, "פזיותרפיסתים",
                        dt_all$code_dirog == 24, "עובדים סוציאלים",
                        dt_all$code_dirog == 31 & dt_all$code_darga == 40, "רופאים סטאזרים",
                        dt_all$code_dirog == 31 & dt_all$code_darga != 40, "רופאים ללא סטאזרים",
                        dt_all$code_dirog == 34, "רוקחים",
                        dt_all$code_dirog %in% c(38, 39 ), "אחים ואחיות כולל",
                        dt_all$code_dirog == 49, "מרפאים בעיסוק",
                        default = "אחר")


#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#

# 96 ----
dt_19 <- dt_all[year == 2019]


# משכורות מתוקננת
dt_19$sal_bruto_shotef_hefrshim_for_full_misra <- 
  dt_19$sal_bruto_shotef_hefrshim / dt_19$halki_misra

# משכורת מתוקננת שנתית
dt_19[,
   mean_sal_bruto_shotef_hefrshim_for_full_misra := 
     sum(sal_bruto_shotef_hefrshim) / sum(halki_misra),
   by = id]


# הוספת מספר ההופעות של העובד בנתונים
dt_19[,
   sum_num_ovedim := sum(num_ovedim),
   by = id]


# מעבר לנתונים ברמת תעודת הזהות
dt_19_by_id <- 
  dt_19[,
     .SD[c(1)],
     by = id]


dt_19_by_id$sal_chank_mean <- 
  fcase(dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra < 6000, "עד ₪6,000",
        dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(6000, 9000), "₪9,000 - ₪6,000",
        dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(9000, 12000), "₪12,000 - ₪9,000",
        dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(12000, 15000), "₪15,000 - ₪12,000",
        dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(15000, 20000), "₪20,000 - ₪15,000",
        dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(20000, 25000), "₪25,000 - ₪20,000",
        dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(25000, 30000), "₪30,000 - ₪25,000",
        dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(30000, 40000), "₪40,000 - ₪30,000",
        dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(40000, 50000), "₪50,000 - ₪40,000",
        dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(50000, 60000), "₪60,000 - ₪50,000",
        dt_19_by_id$mean_sal_bruto_shotef_hefrshim_for_full_misra > 60000, "מעל ₪60,000")



dt_19_by_id$sal_chank_mean <- factor(dt_19_by_id$sal_chank_mean,
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





showtext_auto()


# גרף ונתונים ל 96
data_for_96 <- 
dt_19_by_id[,
            .(sum_ovedim = sum(sum_num_ovedim) / 12), 
            by = sal_chank_mean][, sum_ovedim_p := (sum_ovedim / sum(sum_ovedim))]

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
            size = 3.5) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.31), 
                     breaks = seq(0, 0.5, 0.05),
                     labels= scales::percent(seq(0, 0.5, 0.05),
                                             accuracy = 0.1,
                                             suffix = "%")) +
  scale_x_discrete(guide = guide_axis(angle = 90))  +
  theme_classic() +
  labs(title = "התפלגות העובדים לפי רמות שכר",
       subtitle = "שכר למשרה מלאה, מערכת הבריאות הממשלתית, 2009-2019",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(
        axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")
        ) + 
  easy_remove_y_axis(what = "line")

plot_for_96
#------------------------------------------------------------------------------#

# 97 ----


dt_all$dirog_kiboz_for_97 <- fcase(
  dt_all$code_dirog %in% c(1, 148, 948, 87, 587, 11, 12, 13), "מנהל ומשק",
  dt_all$code_dirog %in% c(34, 48, 548, 20, 24, 16, 49, 47), "פארה רפואיים",
  dt_all$code_dirog == 31, "רופאים",
  dt_all$code_dirog %in% c(38, 39 ), "אחים ואחיות",
  default = "אחר")

data_for_97 <- 
dt_all[year %between% c(2011, 2019),
       .(sum_halki_misra = sum(halki_misra)),
       by = .(year, dirog_kiboz_for_97)][,
                                         sum_2011 := min(sum_halki_misra),
                                         by = dirog_kiboz_for_97][,
                                                                  ":="(ratio = (sum_halki_misra / sum_2011),
                                                                       ratio_sub_1 = (sum_halki_misra / sum_2011) - 1)][
                                                                         , sum_12 := sum_halki_misra/12]




data_for_97$dirog_kiboz_for_97 <- factor(data_for_97$dirog_kiboz_for_97, 
                                         levels = c("אחר",
                                           "פארה רפואיים",
                                           "אחים ואחיות",
                                           "רופאים",
                                           "מנהל ומשק"))



data_for_97$label_p_2019 <- 
  ifelse(data_for_97$year == 2019,
         data_for_97$ratio_sub_1,
         NA)

data_for_97$label_sum_misra_2019 <- 
  ifelse(data_for_97$year == 2019,
         data_for_97$sum_halki_misra / 12,
         NA)


plot_for_97 <- 
  data_for_97 %>% 
  ggplot() +
  aes(x = year, y = ratio_sub_1, color = dirog_kiboz_for_97) +
  geom_line(size = 1.5) +
  scale_x_continuous(limits = c(2011, 2019.5),
                     breaks = seq(2011, 2019, 1),
                     labels = seq(2011, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.0, 0.41),
                     breaks = seq(0, 0.4, 0.05),
                     labels= scales::percent(seq(0, 0.4, 0.05),
                                             accuracy = 1)) +
  geom_text(aes(label=scales::percent(label_p_2019,
                                      accuracy = 1)),
            hjust = -0.1) +
  geom_text(aes(label=scales::comma(label_sum_misra_2019,
                                      accuracy = 1,
                                    prefix = " | ")),
            hjust = -0.6) +
  
  theme_classic() + 
  scale_fill_manual(values = c("#273746", "#45B39D")) +
  labs(title = "שיעור גידול המשרות לפי שנים",
       subtitle = "מערכת הבריאות הממשלתית, 2011 - 2019",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") + 
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=1, colour = "#AFAFAF"),
        plot.margin = margin(c(0,30,20,0)),
        legend.position = "bottom",
        legend.title = element_blank()
        ) +
  coord_cartesian(clip = 'off') +
  easy_remove_y_axis(what = c("line")) +
  scale_color_manual(values = c("#707B7C", "#58D68D", "#239B56", "#2874A6", "#154360"))


plot_for_97
#------------------------------------------------------------------------------#
# 98 ----

# מספר המשרות הכולל
# נתון:
# מספר משרות = 30,076
num_misrot <- sum(dt_19$halki_misra) / 12 # 30076.4

# מספר העובדים הכולל
tot_num_ovdim <- sum(dt_19$num_ovedim) / 12 # 33676

# וניצור את הנתונים לגרף:

dt_for_98 <- 
  dt_19[,
        .(ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra),
          ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim = sum(sal_bruto_shotef_hefrshim) / sum(num_ovedim),
          mid = median(sal_bruto_shotef_hefrshim_for_full_misra)),
        by = dirog_kiboz]

#dt_for_98 %>% View()

# נמיין את הלבליים של המשתנה שלנו מהגדול לקטן
dt_for_98$dirog_kiboz <- fct_reorder(dt_for_98$dirog_kiboz,
                                     -dt_for_98$ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra)


# הנתונים נראים טוב
dt_for_98_melt <- 
  melt.data.table(dt_for_98[dirog_kiboz != "בכירים" & dirog_kiboz != "אחר",
                            .(dirog_kiboz,
                              ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra,
                              ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim)],
                  id=1)

dt_for_98_melt <- merge(dt_for_98_melt, dt_for_98[, .(dirog_kiboz, mid)],
                        by = "dirog_kiboz")

# נוסיף את ממוצאי השכר של כלל המערכת:
dt_for_98_melt <- 
  rbind(dt_for_98_melt, list("מערכת הבריאות הממשלתית",
                             "ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra",
                             (sum(dt_19$sal_bruto_shotef_hefrshim) / num_misrot / 12),
                             NA))

data_for_98 <- 
  rbind(dt_for_98_melt, list("מערכת הבריאות הממשלתית",
                             "ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim",
                             (sum(dt_19$sal_bruto_shotef_hefrshim) / tot_num_ovdim / 12),
                             NA))

# סידור לייבלים אחרון
data_for_98$variable <- factor(data_for_98$variable,
                               levels = 
                                 c(
                                   "ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim",
                                   "ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra"
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





plot_for_98 <- 
  data_for_98 %>% 
  ggplot() + 
  aes(x=dirog_kiboz, y=value, fill=variable) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.5) +
  geom_point(aes(x = dirog_kiboz,
                 y = mid_in_plot), 
             color = "orange",
             position=position_dodge(width=0.5),
             size = 10,
             shape = 95) +
  geom_text(aes(y = labels_in_plot_place,
                label = scales::comma(value, accuracy = 1),
                color = I(labels_in_plot_color)),
            position=position_dodge(width=-0.5),
            angle = 90,
            hjust=-0.2,
            size = 3.5,
            fontface = 'bold') +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(40000)),
                     breaks = seq(0, 40000, 5000),
                     labels = scales::comma(seq(0, 40000, 5000),
                                            accuracy = 1,
                                            prefix = "\244")) +
  theme_classic() + 
  scale_fill_manual(values = c("#45B39D","#273746")) +
  labs(title = "שכר ממוצע וחציוני בדירוגים נבחרים",
       subtitle = "שכר ממוצע לעובד ולמשרה מלאה, מערכת הבריאות הממשלתית, 2019",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") + 
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black", angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(face="bold", size = 12, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "none") +
  easy_remove_y_axis(what = 'line') +
  coord_cartesian(clip = 'off')


plot_for_98
#------------------------------------------------------------------------------#

# 99 ----
# מוכן: כן

# יש לעשות סינון של הקבוצות הדירוגים שמופיעות בדוח
# בעמוד הזה
dt_for_99 <- 
  dt_19[! dirog_kiboz %in% c("בכירים"),
     .(sum_sal_bruto_shotef_hefrshim = sum(sal_bruto_shotef_hefrshim),
       sum_num_ovedim = sum(num_ovedim)),
     by = dirog_kiboz]

dt_for_99$ratio_sum_sal_bruto_shotef_hefrshim <- 
  dt_for_99$sum_sal_bruto_shotef_hefrshim / sum(dt_for_99$sum_sal_bruto_shotef_hefrshim)

dt_for_99$ratio_sum_num_ovedim <- 
  dt_for_99$sum_num_ovedim / sum(dt_for_99$sum_num_ovedim)

dt_for_99$dirog_kiboz <- fct_reorder(dt_for_99$dirog_kiboz,
                                     dt_for_99$ratio_sum_num_ovedim)

# שמירת הנתונים
data_for_99 <- dt_for_99

plot_for_99_1 <- 
  data_for_99 %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = -1*ratio_sum_num_ovedim) +
  geom_bar(stat = "identity", fill = "#283747", width = 0.8) +
  geom_text(aes(x = dirog_kiboz, label = scales::percent(ratio_sum_num_ovedim, 0.01)),
            hjust=1.1,
            size = 3) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.46, 0),
                      breaks = seq(-1, 0, 0.05),
                      labels = scales::percent(seq(1, 0, -0.05), 1)) +  
  coord_flip() +
  theme_classic() +
  labs(title = "אחוז עובדים לפי דירוגים",
       subtitle = "מערכת הבריאות הממשלתית, 2019",
       y = "",
       x = "",
       caption = "") +
  easy_remove_y_axis(what = c("ticks", "title", "text", "line")) + 
  easy_remove_x_axis(what = c("line", "title")) +
  theme(panel.grid.major.x = element_line(size=1.2, colour = "#C0C0C0"),
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        axis.text.x = element_text(face="bold", color = "black", size = 10),
        plot.margin = margin(c(0,0,0,0))) +
  geom_segment(aes(x = 0, xend = 20, y = 0, yend = 0),
               size = 3)

  


plot_for_99_2 <- 
  data_for_99 %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = ratio_sum_sal_bruto_shotef_hefrshim) +
  geom_bar(stat = "identity", fill = "#2E86C1", width = 0.8) +
  geom_text(aes(x = dirog_kiboz, label = scales::percent(ratio_sum_sal_bruto_shotef_hefrshim, 0.01)),
            hjust=-0.15,
            size = 3) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.46),
                     breaks = seq(0, 1, 0.05),
                     labels = scales::percent(seq(0, 1, 0.05), 1)) + 
  coord_flip() +
  theme_classic() +
  labs(title = "התפלגות והצאות השכר לפי דירוגים",
       subtitle = "מערכת הבריאות הממשלתית, 2019",
       y = "",
       x = "",
       caption = "") +
  easy_remove_x_axis(what = c("line")) +
  easy_remove_y_axis(what = c("ticks", "title", "line")) + 
  theme(panel.grid.major.x = element_line(size=1.2, colour = "#C0C0C0"),
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.margin = margin(c(0,0,0,0)),
        axis.text.y = element_text(face="bold", color = "black", size = 12, angle = 0, hjust = 0),
        axis.text.x = element_text(face="bold", color = "black", size = 10)) +
  geom_segment(aes(x = 0, xend = 20, y = 0, yend = 0),
               size = 3)

(plot_for_99_1 + plot_for_99_2)


#------------------------------------------------------------------------------#

# 100 ----
# עשירונים

# נוסיף לכל עובד את סכום חלקיות המשרה השנתית שלו
dt_19[, sum_halki_misra := sum(halki_misra), by = id]


# נוסיף עמודה לכל אדם של השכר הנקי שלו, על פי הניקוי שמופיע באפיון הדוח
dt_19$asironim <- ifelse(is.na(dt_19$asironim), 0, dt_19$asironim)
dt_19[,
   mean_clean := ((sum(sal_bruto_shotef_hefrshim) - sum(asironim)) / sum(halki_misra)),
   by = .(id, dirog_kiboz)]


# נעבור לקיבוץ על פי תעודת זהות
dt_by_id_100 <- 
  dt_19[,
     .SD[c(.N)],
     by = .(id)]

# נסנן חלקיות משרה קטנה מ 2
dt_by_id_100 <- dt_by_id_100[sum_halki_misra > 2]

# נוסיף חיווי לעשירון שכר
dt_by_id_100[, ntile := ntile(mean_clean, 10), by = dirog_kiboz]

# ניצור את הנתונים
dt_100 <- 
merge(
merge(
merge(
dt_by_id_100[ntile == 10 & !(dirog_kiboz %in% c("אחר","בכירים")),
             .(high_mean = round(mean(mean_clean))),
             by = dirog_kiboz],
dt_by_id_100[ntile == 1 & !(dirog_kiboz %in% c("אחר","בכירים")),
             .(low_mean = round(mean(mean_clean))),
             by = dirog_kiboz],
by = "dirog_kiboz"),
dt_by_id_100[!(dirog_kiboz %in% c("אחר","בכירים")),
             .(mean = round(mean(mean_clean))),
             by = dirog_kiboz],
by = "dirog_kiboz"),
dt_by_id_100[!(dirog_kiboz %in% c("אחר","בכירים")),
             .(median = round(median(mean_clean))),
             by = dirog_kiboz],
by = "dirog_kiboz")



# סידור הלייבלים
# נמיין את הלבליים של המשתנה שלנו מהגדול לקטן
dt_100$dirog_kiboz <- fct_reorder(dt_100$dirog_kiboz,
                                     dt_100$mean)



# יצירת הגררף
data_for_100 <- dt_100

plot_for_100 <-
  data_for_100 %>% 
  ggplot() +
  geom_segment(aes(y = dirog_kiboz,
                   yend = dirog_kiboz,
                   x = low_mean,
                   xend = high_mean),
               size = 4,
               color = "#45B39D") +
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
                label = scales::comma(low_mean)),
            hjust = 1.1,
            fontface = 'bold') +
  geom_text(aes(y = dirog_kiboz,
                x = high_mean,
                label = scales::comma(high_mean)),
            hjust = -0.1,
            fontface = 'bold') +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 82000),
                     breaks = seq(0, 82000, 10000),
                     labels = scales::comma(seq(0, 82000, 10000),
                                            accuracy = 1,
                                            prefix = "\244")) +
  labs(title = "עשירוני שכר לפי דירוגים",
       subtitle = "שכר למשרה מלאה, מערכת הבריאות הממשלתית, 2019",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme_classic() +
  theme(
    axis.line = element_line(colour = "black", size = 1.3),
    axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_text(face="bold", size = 12, color = "black"),
        #axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.x = element_line(size=1.2, colour = "#C0C0C0"),
    plot.margin = margin(c(10,30,10,0))) +
  easy_remove_x_axis(what = c("line", "ticks")) +
  coord_cartesian(clip = 'off')
  

plot_for_100

#------------------------------------------------------------------------------#

# 101 ----

# הגרף הזה הולך להיות מבוסס על נתוני הגרף הקודם
# נוסיף את היחס בין העשירון העליון והתחתון לכל עשירון
data_for_100$ratio <- data_for_100$high_mean / data_for_100$low_mean

# נמיין את הלבליים של המשתנה שלנו מהגדול לקטן
data_for_100$dirog_kiboz <- fct_reorder(data_for_100$dirog_kiboz,
                                        data_for_100$ratio)


# שמירת הנתונים בשם
data_for_101 <- data_for_100
# וניצור את הגרף

plot_for_101 <- 
data_for_100 %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = ratio) +
  geom_bar(stat = "identity",
           width = 0.6, 
           fill = "#45B39D") +
  geom_text(aes(x = dirog_kiboz, label = round(ratio, 2)),
            size = 4,
            hjust = -0.1) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 6.1),
                     breaks = seq(0, 6, 1)) +
  labs(title = "אי שיוויון בים עשירונים לפי דירוג",
       subtitle = "היחס בין שכר העשירון העליון לשכר העשירון התחתון, בתי חולים ממשלתיים, 2019",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme_classic() +
  theme(
    axis.line = element_line(colour = "black", size = 1.3),
    axis.text.x = element_text( size = 12, color = "black"),
        #axis.text.y = element_blank(),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.x = element_line(size=0.5, colour = "#939393")) +
  coord_flip() +
  easy_remove_x_axis(what = c("line"))

#------------------------------------------------------------------------------#

# 102 ----


# נוסיף חיווי על חלקיות המשרה של העובד
# יצירת קיבציי קבוצות היקף משרה
dt_19$halki_misra_chank <-
  fcase(dt_19$halki_misra < 0.25 , "25",
        dt_19$halki_misra < 0.5 , "25 to 50",
        dt_19$halki_misra < 0.75, "50 to 75",
        dt_19$halki_misra < 0.95, "75 to 95",
        dt_19$halki_misra >= 0.95, "95 +")


dt_for_102 <- 
  merge(
  dt_19[,
     .N,
     by = .(dirog_kiboz, halki_misra_chank)],
dt_19[,
      .(mean_h = mean(halki_misra)),
      by = .(dirog_kiboz)],
by = "dirog_kiboz")


dt_for_102[,
           ratio := round(N / sum(N), 2),
           by = dirog_kiboz ]


dt_for_102$dirog_kiboz <- fct_reorder(dt_for_102$dirog_kiboz,
                                      dt_for_102$mean_h)


data_for_102 <- dt_for_102
plot_for_102 <- 
  data_for_102 %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = ratio, fill = halki_misra_chank) +
  geom_bar(stat = "identity",
           position = "fill",
           width = 0.7) +
  geom_point(aes(x = dirog_kiboz, y = round(mean_h, 1)),
                 color = "#D4AC0D",
                 size = 3.5) +
  geom_text(aes(label = scales::percent(ratio, 1)),
            position=position_fill(vjust = .5),
            size = 2.8) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.1),
                     labels = scales::percent(seq(0, 1, 0.1), 1)) + 
  
  theme_classic() +
  labs(title = "היקף העסקה בדירוגים נבחרים",
       subtitle = "מערכת הבריאות הממשלתית, 2019",
       y = "",
       x = "",
       fill = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=1, colour = "#939393"),
        plot.margin = margin(c(30,10,0,0))) +
  scale_fill_manual(values = c("#2E86C1", "#1D8348", "#7DCEA0", "#1C2833", "#566573")) +
  coord_cartesian(clip = 'off') +
  easy_remove_y_axis(what = 'line')


plot_for_102
#------------------------------------------------------------------------------#
# נעשה ניסיון נוסף לשיחזור הגרף ברמת התעודת זהות

# נוסיף חלקיות משרה ממוצעת לאדם
dt_19[,
   mean_halki_misra := mean(halki_misra),
   by = id]

# מעבר לנתונים ברמת תעודת הזהות
dt_19_by_id <- 
  dt_19[,
        .SD[c(1)],
        by = id]


dt_19_by_id$halki_misra_chank <-
  fcase(dt_19_by_id$mean_halki_misra < 0.5 , "25 to 50",
        dt_19_by_id$mean_halki_misra < 0.75, "50 to 75",
        dt_19_by_id$mean_halki_misra < 0.95, "75 to 95",
        dt_19_by_id$mean_halki_misra >= 0.95, "95 +")

dt_19_by_id$dirog_kiboz <- factor(dt_19_by_id$dirog_kiboz, levels = c("פסיכולוגים",
                                                                "מרפאים בעיסוק",
                                                                "פארה רפואיים",
                                                                "פזיותרפיסתים",
                                                                "עובדי מעבדה",
                                                                "מנהלי",
                                                                "אקדמאים בהסכם קיבוצי",
                                                                "רופאים ללא סטאזרים",
                                                                "אחים ואחיות כולל",
                                                                "מומחים",
                                                                "רוקחים",
                                                                "מינהל ומשק",
                                                                "אקדמאים בחוזים מיוחדים",
                                                                "אחר",
                                                                "מהנדסים",
                                                                "הנדסאים וטכנאים",
                                                                "עובדים סוציאלים",
                                                                "רנטגנאים",
                                                                "רופאים סטאזרים"))

dt_19_by_id[mean_halki_misra > 0.25,
         .N,
         by = .(dirog_kiboz, halki_misra_chank)] %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = N, fill = halki_misra_chank) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_brewer() +
  theme_classic()

dt_19_by_id[mean_halki_misra > 0.25,
         .(N = sum(sum_num_ovedim)),
         by = .(dirog_kiboz, halki_misra_chank)] %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = N, fill = halki_misra_chank) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_brewer() +
  theme_classic()

#------------------------------------------------------------------------------#

# 103 ----


# יצירת רמת מומחיות
top_menel_ids <- c(59284067,59680421,59004911,29478708,56284532,54003702,51273548,55437115,53935318,22092779,51843712,55348387,53420915,308708999,57698490,26043711,52046935,57258436,17511312,54154596,56126295,56203268,55976070)
dt_19$level_darga <-
  fcase(dt_19$code_darga %in% c(50, 70, 75) , "מתמחה",
        dt_19$code_darga %in% c(85, 80), "מומחה צעיר",
        dt_19$code_darga %in% c(90, 91, 95, 96), "מומחה בכיר",
        dt_19$code_darga %between%  c(96, 142) & !(dt_19$id %in% top_menel_ids), "מנהל",
        dt_19$code_darga %between%  c(96, 142) & (dt_19$id %in% top_menel_ids), "מנהל בית חולים",
        dt_19$code_darga %in% c(210, 220, 230, 240, 250, 260), "תחומי",
        default = "סטזר")

# הכנת הנתונים לגרף
dt_for_103 <- 
  dt_19[code_dirog == 31,
     .(NUM = .N,
       NUM_12 = .N /12,
       SAL = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra)),
     by = level_darga]

# מיון ליבליים
dt_for_103$level_darga <- fct_reorder(dt_for_103$level_darga,
                                      dt_for_103$SAL)


dt_for_103$zero <- 0


data_for_103 <- dt_for_103

plot_for_103 <- 
  data_for_103 %>% ggplot() +
  aes(x = level_darga, y = SAL) +
  geom_bar(stat = "identity",
           width = 0.6,
           fill = "#212F3D",
           color = "white") +
  geom_text(aes(x = level_darga, label = scales::comma(SAL, accuracy = 1)),
            vjust = 0.3,
            hjust = 1.2,
            size = 3,
            color = "#FBFCFC") +
  geom_text(aes(x = level_darga, y = zero,label = scales::comma(NUM_12, accuracy = 1)),
            vjust = 0.3,
            hjust = -0.2,
            size = 3,
            color = "#F5B041") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 91000),
                     breaks = seq(0, 90000, 10000),
                     labels = scales::comma(seq(0, 90000, 10000),
                                           accuracy = 1,
                                           prefix = "\244")) + 
  theme_classic() +
  labs(title = "שכר למשרה של רופא לפי שלב קריירה",
       subtitle = "שכר ממוצע למשרה מלאה, מערכת הבריאות הממשלתית, 2019",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.x = element_line(size=0.5, colour = "#939393")) +
  coord_flip()  +
  easy_remove_x_axis(what = "line")


plot_for_103
#------------------------------------------------------------------------------#

# 104 ----

dt_all$level_darga_for_104 <-
  fcase(dt_all$code_darga %in% c(50, 70, 75) , "מתמחה",
        dt_all$code_darga %in% c(40) , "סטאזר",
        dt_all$code_darga %in% c(85, 80), "מומחה צעיר",
        dt_all$code_darga %in% c(90, 91, 95, 96), "מומחה בכיר",
        dt_all$code_darga %between%  c(96, 142), "מנהל",
        dt_all$code_darga %in% c(210, 220, 230, 240, 250, 260), "תחומי",
        default = "")

# sql:
#case WHEN "MMD_DIRUG"."CODE_DARGA" IN (50, 70, 75) then 'עם מענקים 1 מתמחה' 
#WHEN "MMD_DIRUG"."CODE_DARGA" = 40 then 'סטזר' 
#WHEN "MMD_DIRUG"."CODE_DARGA" IN (80, 85) then '3 מומחה צעיר' 
#WHEN "MMD_DIRUG"."CODE_DARGA" IN (90, 91, 95, 96) then '4 מומחה בכיר' 
#WHEN "MMD_DIRUG"."CODE_DARGA" BETWEEN 96 AND 142 then '5 מנהל' 
#WHEN "MMD_DIRUG"."CODE_DARGA" IN (210, 220, 230, 240, 250, 260) then '2 תחומי' 
#when "MMD_OVDIM"."OVED_ID_FOR_HR_SACHAR" in (59284067,53935318,59680421,29478708,59004911,56284532,54003702,51273548,22092779,1019460,22269963,7311152,17511312,26043711,50104165,52046935,56126295,56203268,57258436,308708999,22354674,24348542,58719097) then 'מנהל ביח' else '' end


dt_104 <- 
  dt_all[year %between% c(2009, 2019) & code_dirog == 31,
         .(sum_sal = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra)),
         by = .(year, level_darga_for_104)][,
                                           sum_2009 := min(sum_sal),
                                           by = level_darga_for_104][,
                                                                    ":="(ratio = (sum_sal / sum_2009),
                                                                         ratio_sub_1 = (sum_sal / sum_2009) - 1)][
                                                                           , sum_12 := sum_sal/12]




dt_all$mankim <- ifelse(is.na(dt_all$mankim), 0, dt_all$mankim)
dt_104_no_manak <- 
  dt_all[year %between% c(2009, 2019) & code_dirog == 31 & level_darga_for_104 == "מתמחה",
         .(sum_sal = (sum(sal_bruto_shotef_hefrshim) - sum(mankim)) / sum(halki_misra)),
         by = .(year, level_darga_for_104)][,
                                            sum_2009 := min(sum_sal),
                                            by = level_darga_for_104][,
                                                                      ":="(ratio = (sum_sal / sum_2009),
                                                                           ratio_sub_1 = (sum_sal / sum_2009) - 1)][
                                                                             , sum_12 := sum_sal/12]
dt_104_no_manak$level_darga_for_104 <- "מתמחה ללא מענקים"


dt_104 <- rbind(dt_104, dt_104_no_manak)

#dt_104[year == 2019, .(ratio_2019 = ratio_sub_1)] %>% View()

dt_104 <- merge(dt_104,
                dt_104[year == 2019, .(level_darga_for_104, ratio_2019 = ratio_sub_1)],
                by = "level_darga_for_104")


data_for_104 <- dt_104


plot_for_104 <- 
data_for_104[!level_darga_for_104 %in% c("", "תחומי")] %>% 
  ggplot() +
  aes(x = year, y = ratio_sub_1, color = level_darga_for_104) +
  geom_line(size = 1.5) +
  geom_text(aes(x = 2019,
                y = ratio_2019,
                label = scales::percent(ratio_2019, 1)),
            hjust = -0.1) +
  scale_x_continuous(
    breaks = seq(2009, 2019, 1),
    labels = seq(2009, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.0, 0.91),
                     breaks = seq(0, 0.9, 0.1),
                     labels= scales::percent(seq(0, 0.9, 0.1),
                                             accuracy = 1)) +
  theme_classic() + 
  scale_fill_manual(values = c("#273746", "#45B39D")) +
  labs(title = "קצב גידול נומינלי בשכר הרופאים, לפי תפקידים",
       subtitle = "שיעור הגידול בשכר, מערכת הבריאות הממשלתית, 2009 - 2019",
       y = "",
       x = "",
       color = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") + 
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "top") +
  easy_remove_y_axis(what = c("line"))


plot_for_104
#------------------------------------------------------------------------------#

# 105 ----


#------------------------------------------------------------------------------#

# 106 ----

# ניצור את קיבוץ הדרגה של העמוד הזה
dt_19$level_darga_for_106 <-
  fcase(dt_19$code_darga %in% c(50, 70, 75) , "מתמחה",
        dt_19$code_darga %in% c(85, 80), "מומחה צעיר",
        dt_19$code_darga %in% c(90, 91, 95, 96), "מומחה בכיר",
        dt_19$code_darga %between%  c(96, 142), "מנהל",
        dt_19$code_darga %in% c(210, 220, 230, 240, 250, 260), "תחומי",
        default = "סטזר")

# ואת ההגדרה לאזור האגף
dt_19$ezor_agaf <-
  fcase(dt_19$code_agaf %in% c(307,643,137,307) , "פריפריה 2",
        dt_19$code_agaf %in% c(207,107), "פריפריה 1",
        dt_19$code_agaf %in% c(413,507,313,113), "פריפריה 3",
        default = "מרכז")


# וכעת ניצור את הנתונים
dt_for_106 <- 
  dt_19[code_dirog == 31 & level_darga_for_106 != "סטזר",
     .(NUM = .N,
       NUM_12 = .N /12,
       SAL = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra)),
     by = .(level_darga_for_106, ezor_agaf)]


dt_for_106$level_darga_for_106 <- factor(dt_for_106$level_darga_for_106,
                                         levels = c("מתמחה",
                                                    "תחומי",
                                                    "מומחה צעיר",
                                                    "מומחה בכיר",
                                                    "מנהל"))

dt_for_106$ezor_agaf <- factor(dt_for_106$ezor_agaf,
                               levels = c("מרכז",
                                          "פריפריה 3",
                                          "פריפריה 2",
                                          "פריפריה 1"))




data_for_106 <- dt_for_106

plot_for_106 <- 
data_for_106 %>% 
  ggplot() +
  aes(x = level_darga_for_106, y = SAL, fill = ezor_agaf) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single")) +
  geom_text(aes(x = level_darga_for_106, label = scales::comma(SAL, accuracy = 1)),
            position=position_dodge(width = 0.9),
            vjust = -0.5,
            hjust = 0.5,
            size = 3) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 80000),
                     breaks = seq(0, 80000, 10000),
                    labels = scales::comma(seq(0, 80000, 10000), accuracy = 1, prefix = "\244")) + 
  theme_classic() +
  labs(title = "שכר רופאים, לפי דרגה ואזור בית חולים",
       subtitle = "שכר ממוצע למשרה מלאה, מערכת הבריאות הממשלתית, 2019",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_text(face="bold", size = 12, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.background = element_rect(linetype = "blank"),
        legend.title=element_blank()) +
  scale_fill_manual(values = c("#17A589", "#7FB3D5", "#2471A3", "#212F3D")) +
  easy_remove_y_axis(what = "line")



plot_for_106
#------------------------------------------------------------------------------#

# 107 ----

# יצירת מסד נתונים עבור רופאים בשנים 
# 2010
# ו2019
# בלבד
dt_107 <- 
dt_all[year %in% c(2010, 2019) & code_dirog == 31]


# הוספת סיווג לפריפריה או מרכז
dt_107$ezor_agaf_for_107 <-
  fcase(dt_107$code_agaf %in% c(413,507,313,113, 207,107, 307,643,137,307) , "פריפריה",
        default = "מרכז")

dt_for_107 <- 
dt_107[, .(id, sal_bruto_shotef_hefrshim, year, ezor_agaf_for_107)]

dt_for_107[, mean_sal := mean(sal_bruto_shotef_hefrshim), by = id]



dt_for_107$type <- paste0(dt_for_107$year, dt_for_107$ezor_agaf_for_107)

dt_for_107_by_id <- 
  dt_for_107[,
        .SD[c(1)],
        by = id]


ggplot(dt_for_107_by_id[sal_bruto_shotef_hefrshim %between% c(5000, 110000)]) +
  geom_density(aes(sal_bruto_shotef_hefrshim, color = type),
               size = 2, adjust = 2.5) 
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.00007))
                     
                     

#------------------------------------------------------------------------------#

# 108 ----


#------------------------------------------------------------------------------#

  
# שילוב תאגידי בריאות ----
  
# טעינת קובץ הנתונים:
dt_for_gam_and_gam <- fread("health_2019_data_for_merge_21_07_26_R_SCRIPT.csv", 
            encoding = "UTF-8")

# מבט ויזאולי ראשוני על קובץ הנתונים
#####vis_dat(dt, warn_large_data = F)


# נשנה את עמודת התאריכים למשתנה מסוג
# DATE
dt_for_gam_and_gam$`תאריך שכר` <- as_date(dt_for_gam_and_gam$`תאריך שכר`)

# קצת סטטיסטיקה ראשונית על הנתונים
#view(descr(dt)) 
#view(dfSummary(dt)) 
#------------------------------------------------------------------------------#

# עידכון שמות העמודות של סט הנתונים
old_names <- copy(names(dt_for_gam_and_gam))
names_dict <- data.frame(old = old_names,
                         new = old_names)
names_dict %>% datatable()

setnames(dt_for_gam_and_gam, "מספר עובדים", "num_ovedim")
setnames(dt_for_gam_and_gam, "ת.ז. של עובד", "id")
setnames(dt_for_gam_and_gam, "תאריך שכר", "date")
setnames(dt_for_gam_and_gam, "מגדר", "sex")
setnames(dt_for_gam_and_gam, "אגף", "agaf")
setnames(dt_for_gam_and_gam, "קוד אגף", "code_agaf")

setnames(dt_for_gam_and_gam, "משכורת ברוטו שוטף", "sal_bruto_shotef")
setnames(dt_for_gam_and_gam, "משכורת ברוטו שוטף והפרשים", "sal_bruto_shotef_hefrshim")
setnames(dt_for_gam_and_gam, "יסוד משולב", "isod_msolav")
setnames(dt_for_gam_and_gam, "עבודה נוספת", "avoda_noseft")
setnames(dt_for_gam_and_gam, "החזר הוצאות", "ehzer_hozaot")
setnames(dt_for_gam_and_gam, "תשלומים אחרים", "taclomim_aharim")

setnames(dt_for_gam_and_gam, "פרישה עשירונים סכום", "asironim")
setnames(dt_for_gam_and_gam, "מענקים סכום", "mankim")

setnames(dt_for_gam_and_gam, "חלקיות חודש", "halki_hodesh")
setnames(dt_for_gam_and_gam, "חלקיות חודש משרה", "halki_hodesh_misra")
setnames(dt_for_gam_and_gam, "חלקיות משרה", "halki_misra")

setnames(dt_for_gam_and_gam, "קוד דירוג", "code_dirog")
setnames(dt_for_gam_and_gam, "דירוג", "dirog")
setnames(dt_for_gam_and_gam, "קוד דרגה", "code_darga")
setnames(dt_for_gam_and_gam, "דרגה", "darga")


names_dict$new <- names(dt_for_gam_and_gam)
names_dict %>% datatable()
#------------------------------------------------------------------------------#
# הוספת עמודות רלוונטיות לניתוח

# בחלק זה של הסקריפט אני הולך להוסיף עמודות
# שיהיו רלוונטיות בניתוח

# עמודות שאני הולך להוסיף הם:
# 1. איחודי קטגוריות
# התאמות למשרה מלאה .2 
# סכימות שכר לעובד .3 
# בהתאם לקובץ האיפיון
# שאותו אפשר לפתוח:
# "אפיון פרק בריאות - דוח הממונה.docx"


# קיבוץ דירוגים
dt_for_gam_and_gam$dirog_kiboz <- fcase(
  dt_for_gam_and_gam$code_dirog == 1, "מנהלי",
  dt_for_gam_and_gam$code_dirog %in% c(7, 8), "רנטגנאים",
  dt_for_gam_and_gam$code_dirog %in% c(33, 43), "עובדי מעבדה",
  dt_for_gam_and_gam$code_dirog %in% c(47, 48, 548), "פארה רפואיים",
  dt_for_gam_and_gam$code_dirog %in% c(87, 587), "אקדמאים בחוזים מיוחדים",
  dt_for_gam_and_gam$code_dirog %in% c(148, 948), "מינהל ומשק",
  dt_for_gam_and_gam$code_dirog %in% c(557, 57, 566, 568, 569), "בכירים",
  dt_for_gam_and_gam$code_dirog %in% c(964, 164, 555), "מומחים",
  dt_for_gam_and_gam$code_dirog == 11, "אקדמאים בהסכם קיבוצי",
  dt_for_gam_and_gam$code_dirog == 12, "מהנדסים",
  dt_for_gam_and_gam$code_dirog == 13, "הנדסאים וטכנאים",
  dt_for_gam_and_gam$code_dirog == 16, "פסיכולוגים",
  dt_for_gam_and_gam$code_dirog == 20, "פזיותרפיסתים",
  dt_for_gam_and_gam$code_dirog == 24, "עובדים סוציאלים",
  dt_for_gam_and_gam$code_dirog == 31 & dt_for_gam_and_gam$code_darga == 40, "רופאים סטאזרים",
  dt_for_gam_and_gam$code_dirog == 31 & dt_for_gam_and_gam$code_darga != 40, "רופאים ללא סטאזרים",
  dt_for_gam_and_gam$code_dirog == 34, "רוקחים",
  dt_for_gam_and_gam$code_dirog %in% c(38, 39 ), "אחים ואחיות כולל",
  dt_for_gam_and_gam$code_dirog == 49, "מרפאים בעיסוק",
  default = "אחר")


# מספר הופעות של העובד בסט הנתונים
dt_for_gam_and_gam[,
   count_rows_for_id := .N,
   by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
   sum_num_ovedim := sum(num_ovedim),
   by = .(id, dirog_kiboz)]

# סכום המשכורת של העובד
dt_for_gam_and_gam[,
   sum_sal_bruto_shotef := sum(sal_bruto_shotef),
   by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
     sum_sal_bruto_shotef_hefrshim := sum(sal_bruto_shotef_hefrshim),
     by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
   sum_isod_msolav := sum(isod_msolav),
   by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
   sum_avoda_noseft := sum(avoda_noseft),
   by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
   sum_ehzer_hozaot := sum(ehzer_hozaot),
   by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
   sum_taclomim_aharim := sum(taclomim_aharim),
   by = .(id, dirog_kiboz)]

# משכורות מתוקננת
dt_for_gam_and_gam$sal_bruto_shotef_hefrshim_for_full_misra <- 
  dt_for_gam_and_gam$sal_bruto_shotef_hefrshim / dt_for_gam_and_gam$halki_misra
dt_for_gam_and_gam[,
   sum_sal_bruto_shotef_hefrshim_for_full_misra := sum(sal_bruto_shotef_hefrshim_for_full_misra),
   by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
   mean_sal_bruto_shotef_hefrshim_for_full_misra := mean(sal_bruto_shotef_hefrshim_for_full_misra),
   by = .(id, dirog_kiboz)]

# משכורת נקיה
dt_for_gam_and_gam %>% names()

dt_for_gam_and_gam$asironim <- ifelse(is.na(dt_for_gam_and_gam$asironim), 0, dt_for_gam_and_gam$asironim)
dt_for_gam_and_gam$mankim <- ifelse(is.na(dt_for_gam_and_gam$mankim), 0, dt_for_gam_and_gam$mankim)

dt_for_gam_and_gam$clean_sal <- dt_for_gam_and_gam$sal_bruto_shotef_hefrshim - dt_for_gam_and_gam$asironim - dt_for_gam_and_gam$mankim

dt_for_gam_and_gam[,
   mean_clean := ((sum(sal_bruto_shotef_hefrshim) - sum(asironim) - sum(mankim)) / sum(halki_misra)),
     by = .(id, dirog_kiboz)]
  
dt_for_gam_and_gam[,
   sum_clean := ((sum(sal_bruto_shotef_hefrshim) - sum(asironim) - sum(mankim))),
   by = .(id, dirog_kiboz)]



# סכום היקף המשרה של העובד
dt_for_gam_and_gam[,
   sum_halki_hodesh := sum(halki_hodesh),
   by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
   mean_halki_hodesh := mean(halki_hodesh),
   by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
   sum_halki_hodesh_misra := sum(halki_hodesh_misra),
   by = .(id, dirog_kiboz)]

dt_for_gam_and_gam[,
   sum_halki_misra := sum(halki_misra),
   by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
   mean_halki_misra := mean(halki_misra),
   by = .(id, dirog_kiboz)]
dt_for_gam_and_gam[,
   sum_num_ovedim := sum(num_ovedim),
   by = .(id, dirog_kiboz)]


# תקנון משכורות לעשירונים
dt_for_gam_and_gam$asironim <- ifelse(is.na(dt_for_gam_and_gam$asironim), 0, dt_for_gam_and_gam$asironim)

dt_for_gam_and_gam$sal_bruto_shotef_hefrshim_sub_asironim <- 
  dt_for_gam_and_gam$sal_bruto_shotef_hefrshim - dt_for_gam_and_gam$asironim

dt_for_gam_and_gam$sal_bruto_shotef_hefrshim_sub_asironim_for_full_misra <- 
  dt_for_gam_and_gam$sal_bruto_shotef_hefrshim_sub_asironim / dt_for_gam_and_gam$halki_misra

dt_for_gam_and_gam[,
   mean_sal_bruto_shotef_hefrshim_sub_asironim_for_full_misra := 
     mean(sal_bruto_shotef_hefrshim_sub_asironim_for_full_misra),
   by = .(id, dirog_kiboz)]

# ניסיון נוסף
dt_for_gam_and_gam[,
   sum_asironim := 
     sum(asironim),
   by = id]


dt_for_gam_and_gam[,
   mean_sal_bruto_shotef_hefrshim_sub_asironim_for_full_misra :=
     (sum_sal_bruto_shotef_hefrshim - sum_asironim) / sum_halki_misra]


# שמירת סט נתונים חדש, אם השורה הראשונה בלבד לכל תעודת זהות
dt_for_gam_and_gam_by_id <- 
  dt_for_gam_and_gam[,
     .SD[c(1)],
     by = .(id, dirog_kiboz)]



# תאגידים

tagid <- fread("tagidim\\tagidim_2019_data_21_08_04.csv", 
               encoding = "UTF-8")

names(tagid)
setnames(tagid, "ת\"\"ז עובד", "id")
setnames(tagid, "תאריך שכר", "date")
setnames(tagid, "משכורת ברוטו שוטף והפרשים", "sal_bruto_shotef_hefrshim_tagid")
setnames(tagid, "משכורת ברוטו שוטף", "sal_bruto_shotef_tagid")

tagid$date <- as_date(tagid$date)
# מספר שורות
# 184994

#------------------------------------------------------------------------------#

# ניקוי סעיפים 1 עד 4
tagid$sum_sal_bruto_shotef_hefrshim_tagid

tagid[,sum_sal_bruto_shotef_hefrshim_tagid := sum(sal_bruto_shotef_hefrshim_tagid),
      by = id]

tagid[,sum_sal_bruto_shotef_tagid := sum(sal_bruto_shotef_tagid),
      by = id]

tagid <- tagid[sum_sal_bruto_shotef_tagid > 100 & sum_sal_bruto_shotef_hefrshim_tagid > 100]
# מספר שורות
# 168073


# ממשלתיים
# מספר שורות לפני,
# 448154
nrow(dt)

dt_for_gam_and_gam <- dt_for_gam_and_gam[sum_sal_bruto_shotef > 1000 & sum_clean > 1000]
# מספר שורות אחרי,
# 442327

#------------------------------------------------------------------------------#

# איחוד ממשלתי ותאגידים ----
mem_with_tagid <- 
  merge(dt_for_gam_and_gam, tagid, 
        all.x = T,
        by = c("id", "date"))


#------------------------------------------------------------------------------#

# ניקוי 5 עד 8

# מספר שורות לפני
# 442844
nrow(mem_with_tagid)
mem_with_tagid_clean <- 
  mem_with_tagid[sal_bruto_shotef > 1000 &
                   clean_sal > 1000 &
                   sal_bruto_shotef_tagid > 100 &
                   sal_bruto_shotef_hefrshim_tagid > 100]
# מספר שורות אחרי
# 41855

# כעת אני רוצה לאחד את השכר של האנשים שעובדים גם בתאגיד וגם בממשלתיים



# 109 ----

mem_with_tagid_clean$sal_with_tagid <- 
  mem_with_tagid_clean$sal_bruto_shotef_hefrshim + 
  mem_with_tagid_clean$sal_bruto_shotef_hefrshim_tagid

data_for_109 <- 
  mem_with_tagid_clean[, .(sal = (sum(sal_with_tagid) / .N),
                           sal_tagid = (sum(sal_bruto_shotef_hefrshim_tagid) / .N),
                           sal_mem = (sum(sal_bruto_shotef_hefrshim) / .N),
                           N = .N / 12),
                       by = dirog_kiboz]

data_for_109$dirog_kiboz <- 
  fct_reorder(data_for_109$dirog_kiboz, data_for_109$sal)



#

plot_for_109 <- 
data_for_109[, .(dirog_kiboz, sal_tagid, sal_mem)] %>% 
  melt("dirog_kiboz") %>% 
  group_by(dirog_kiboz) %>% 
  mutate(sum_sal = sum(value)) %>% 
  merge(data_for_109[, .(dirog_kiboz, N)], "dirog_kiboz") %>% 
    ggplot() +
    aes(dirog_kiboz, value, fill = variable) +
    geom_bar(stat = "identity",
             position = "stack") + 
    geom_text(aes(x = dirog_kiboz, label = scales::comma(value, accuracy = 1)),
              position = position_stack(vjust = .0),
              vjust = -0.5) + 
    geom_text(aes(x = dirog_kiboz, y = sum_sal, label = scales::comma(sum_sal, accuracy = 1)),
              vjust = -0.5) +
    geom_text(aes(x = dirog_kiboz, y = 0, label = scales::comma(N, accuracy = 1)),
              vjust = 1,
              color = "green")
  
#
  
# 110 ----

# נוסיף לכל עובד את סכום חלקיות המשרה השנתית שלו
mem_with_tagid_clean[, sum_halki_misra := sum(halki_misra), by = id]


# נוסיף עמודה לכל אדם של השכר הנקי שלו, על פי הניקוי שמופיע באפיון הדוח
mem_with_tagid_clean$asironim <- ifelse(is.na(mem_with_tagid_clean$asironim),
                                        0,
                                        mem_with_tagid_clean$asironim)

mem_with_tagid_clean[,
                     sal_per_oved := ((sum(sal_bruto_shotef_hefrshim) + 
                                         sum(sal_bruto_shotef_hefrshim_tagid) - 
                                         sum(asironim)) / 
                                        sum(num_ovedim)),
                     by = .(id, dirog_kiboz)]


# נעבור לקיבוץ על פי תעודת זהות
mem_with_tagid_clean_by_id <- 
  mem_with_tagid_clean[,
                       .SD[c(.N)],
                       by = .(id)]

# נסנן חלקיות משרה קטנה מ 2
mem_with_tagid_clean_by_id <- mem_with_tagid_clean_by_id[sum_halki_misra > 2]

# נוסיף חיווי לעשירון שכר
mem_with_tagid_clean_by_id[, ntile := ntile(sal_per_oved, 10), by = dirog_kiboz]

# ניצור את הנתונים
data_for_110 <- 
  merge(
    merge(
      merge(
        mem_with_tagid_clean_by_id[ntile == 10 & dirog_kiboz != "בכירים",
                                   .(high_mean = round(mean(sal_per_oved))),
                                   by = dirog_kiboz],
        mem_with_tagid_clean_by_id[ntile == 1 & dirog_kiboz != "בכירים",
                                   .(low_mean = round(mean(sal_per_oved))),
                                   by = dirog_kiboz],
        by = "dirog_kiboz"),
      mem_with_tagid_clean_by_id[dirog_kiboz != "בכירים",
                                 .(mean = round(mean(sal_per_oved))),
                                 by = dirog_kiboz],
     by = "dirog_kiboz"),
   mem_with_tagid_clean_by_id[dirog_kiboz != "בכירים",
                              .(median = round(median(sal_per_oved))),
                              by = dirog_kiboz],
   by = "dirog_kiboz")



# סידור הלייבלים
# נמיין את הלבליים של המשתנה שלנו מהגדול לקטן
data_for_110$dirog_kiboz <- fct_reorder(data_for_110$dirog_kiboz,
                                        data_for_110$mean)



# יצירת הגררף

plot_for_110 <-
  data_for_110[dirog_kiboz %in% c("רופאים ללא סטאזרים",
                                "מהנדסים",
                                "אחים ואחיות כולל",
                                "רטגנאים",
                                "מנהל ומשק",
                                "אקדמאים בהסכם קיבוצי",
                                "רוקחים",
                                "עובדי מעבדה",
                                "מנהלי")] %>% 
  ggplot() +
  geom_segment(aes(y = dirog_kiboz,
                   yend = dirog_kiboz,
                   x = low_mean,
                   xend = high_mean)) +
  geom_point(aes(y = dirog_kiboz,
                 x = mean),
             color = "yellow") +
  geom_point(aes(y = dirog_kiboz,
                 x = median),
             color = "blue") +
  geom_text(aes(y = dirog_kiboz,x = low_mean, label = low_mean),
            hjust = 1.1) +
  geom_text(aes(y = dirog_kiboz,x = high_mean, label = high_mean),
            hjust = -0.1) +
  theme_classic()
#------------------------------------------------------------------------------#

# 111 ----

#------------------------------------------------------------------------------#

# 112 ----


#------------------------------------------------------------------------------#

# 113 ----


#------------------------------------------------------------------------------#

# 114 ----
  
data_for_114_2 <-  
    dt_19[, .(ratio = sum(num_ovedim) / tot_num_ovdim / 12),
          by = sex]

data_for_114_1 <-  
  dt_19[, .(mean_sal = sum(sal_bruto_shotef_hefrshim) / sum(num_ovedim)),
        by = sex]
  
plot_for_114_2 <- 
  data_for_114_2 %>% 
  ggplot() +
  aes(x = sex, y = ratio) +
  geom_bar(stat = "identity",
           width = 0.4,
           fill = c("#273746", "#D68910")) +
  geom_text(aes(x = sex, label = scales::percent(ratio)),
            position = position_stack(vjust = 0.5),
            size = 5,
            color = "#FBFCFC") +
  geom_text(aes(x = sex, label = sex),
            position = position_stack(vjust = 0.5),
            vjust = -1.2,
            size = 5,
            color = "#FBFCFC") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.72),
                     breaks = seq(0, 0.7, 0.1),
                     labels= scales::percent(seq(0, 0.7, 0.1),
                                             accuracy = 1)) + 
  labs(title = "שיעור עובדים לפי מגדר",
       subtitle = "מערכת הבריאות 2019",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme_classic() +
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.x = element_line(size=0.5, colour = "#939393")) +
  coord_flip() +
  easy_remove_x_axis(what = c("line"))



plot_for_114_1 <- 
  data_for_114_1 %>% 
  ggplot() +
  aes(x = sex, y = mean_sal) +
  geom_bar(stat = "identity",
           width = 0.4,
           fill = c("#273746", "#D68910")) +
  geom_text(aes(x = sex, label = floor(mean_sal)),
            position = position_stack(vjust = 0.5),
            size = 5,
            color = "#FBFCFC") +
  geom_text(aes(x = sex, label = sex),
            position = position_stack(vjust = 0.5),
            vjust = -1.2,
            size = 5,
            color = "#FBFCFC") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 28000),
                     breaks = seq(0, 25000, 5000)) + 
  labs(title = "שכר ממוצע למגדר",
       subtitle = "מערכת הבריאות 2019",
       y = "",
       x = "",
       caption = "") +
  theme_classic() +
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.x = element_line(size=0.5, colour = "#939393")) +
  coord_flip() + 
  easy_remove_x_axis(what = c("line"))

plot_for_114_1 + plot_for_114_2



#------------------------------------------------------------------------------#

# 115 ----
dt_for_115 <- 
  dt_19[dirog_kiboz != "אחר",
     .(count = .N),
     by = .(dirog_kiboz, sex)]

dt_for_115$sex <- factor(dt_for_115$sex,
                         levels = c("נשים",
                                    "גברים"))

dt_for_115$dirog_kiboz <- 
  factor(dt_for_115$dirog_kiboz, 
         levels = c("רופאים ללא סטאזרים",
                    "בכירים",
                    "אחים ואחיות כולל",
                    "מינהל ומשק",
                    "מהנדסים",
                    "אקדמאים בחוזים מיוחדים",
                    "עובדי מעבדה",
                    "הנדסאים וטכנאים",
                    "רוקחים",
                    "מומחים",
                    "רנטגנאים",
                    "אקדמאים בהסכם קיבוצי",
                    "מנהלי",
                    "רופאים סטאזרים",
                    "עובדים סוציאלים",
                    "פזיותרפיסתים",
                    "פארה רפואיים",
                    "פסיכולוגים",
                    "מרפאים בעיסוק"))

dt_for_115[, tot_count := sum(count),
           by = dirog_kiboz][, ratio := count / tot_count]


dt_for_115 <- 
  merge.data.table(dt_for_115,
                   dt_for_98[dirog_kiboz != "אחר",
                             .(dirog_kiboz, ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim)],
                   by = "dirog_kiboz")

dt_for_115$one <- 1


data_for_115 <- dt_for_115

plot_for_115 <- 
  data_for_115 %>% 
  ggplot() + 
  aes(x = dirog_kiboz, y = ratio, fill = sex) +
  geom_bar(stat = "identity",
           position = "stack") +
  geom_text(aes(x = dirog_kiboz, label = scales::percent(ratio,
                                                         accuracy = 1)),
            position=position_fill(vjust = .5),
            size = 4,
            color = "white") +
  geom_text(aes(x = dirog_kiboz,
                y = one,
                label = round(ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim)),
            vjust = 1,
            size = 4,
            color = "black") +
  
  scale_fill_manual(values = c("#D68910", "#273746")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.01), 
                     breaks = seq(0, 1, 0.1),
                     labels= scales::percent(seq(0, 1, 0.1),
                                             accuracy = 1,
                                             suffix = "%")) +
  labs(title = "התפלגות מגדרית לפי דירוגים שונים",
       subtitle = "מערכת הבריאות הממשלתית, 2019",
       y = "",
       x = "",
       caption = "") +
  theme_classic() +
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_text(face="bold", size = 12, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(linetype = "blank"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position='top',
        legend.title=element_blank())



#------------------------------------------------------------------------------#

# 116 ----

dt_for_116 <- 
  dt_19[,
     .(ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra),
       ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim = sum(sal_bruto_shotef_hefrshim) / sum(num_ovedim),
       mid = median(sal_bruto_shotef_hefrshim_for_full_misra)),
     by = .(dirog_kiboz, sex)] %>% 
  dcast(dirog_kiboz ~ sex,
        value.var = c("ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra",
                      "ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim")) 


dt_for_116[, ":="(dist_misra =
                    ((ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra_גברים - 
                        ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra_נשים) /
                       ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra_גברים),
                  dist_ovedim =
                    ((ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim_גברים - 
                        ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim_נשים) /
                       ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim_גברים))]

dt_for_116$dirog_kiboz <- fct_reorder(dt_for_116$dirog_kiboz,
                                      -dt_for_116$dist_misra)


dt_for_116 <- 
  dt_for_116[, .(dirog_kiboz, dist_misra, dist_ovedim)] %>%
  melt("dirog_kiboz")


data_for_116 <- dt_for_116

plot_for_116 <- 
  data_for_116[dirog_kiboz != "אחר"] %>% ggplot() +
  aes(x = dirog_kiboz, y = value, fill = variable) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = scales::percent(value, accuracy = 1)),
            position=position_dodge(width=0.9),
            vjust=-0.25,
            size = 2.8) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.21, 0.41),
                     breaks = seq(-0.2, 0.4, 0.1),
                     labels= scales::percent(seq(-0.2, 0.4, 0.1),
                                             accuracy = 1)) +
  theme_classic() + 
  scale_fill_manual(values = c("#273746", "#45B39D")) +
  labs(title = "פערי שכר מגדריים",
       subtitle = "מערכת הבריאות הממשלתית, 2019",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") + 
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black", angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "none") +
  easy_remove_x_axis(what = c("line")) +
  easy_remove_y_axis(what = c("line"))



#------------------------------------------------------------------------------#
# 117 ----

# אני מוסיף עמודה שחסרה לי בשליפות
# בהמשך ממולץ לעדכן את כל השליפות
dt_19_2 <- fread("mem\\health_2019_data_21_08_02.csv", 
            encoding = "UTF-8")

setnames(dt_19_2, "תוספות", "tosef")
dt_19$tosef <- dt_19_2$tosef

# ומתחיל ניתוח
dt_19$hefrshim <- 
  (dt_19$sal_bruto_shotef_hefrshim - dt_19$sal_bruto_shotef)

dt_for_117 <- 
  dt_19[dirog_kiboz %in% c("רופאים ללא סטאזרים", "אחים ואחיות כולל"), 
     .(mean_isod_and_tosef = (sum(isod_msolav) + sum(tosef)) / sum(halki_misra),
       mean_avoda_noseft = sum(avoda_noseft) / sum(halki_misra),
       mean_ehzer_hozaot = sum(ehzer_hozaot) / sum(halki_misra),
       mean_hefrshim = sum(hefrshim) / sum(halki_misra),
       mean_taclomim_aharim = sum(taclomim_aharim) / sum(halki_misra)),
     by = .(dirog_kiboz, sex)]


dt_for_117 <- 
  dt_for_117 %>% 
  melt.data.table(id=c("dirog_kiboz", "sex"))

dt_for_117$variable <- factor(dt_for_117$variable,
                              levels = c("mean_hefrshim",
                                         "mean_taclomim_aharim",
                                         "mean_ehzer_hozaot",
                                         "mean_avoda_noseft",
                                         "mean_isod_and_tosef"))


dt_for_117$sex <- factor(dt_for_117$sex,
                         levels = c("נשים",
                                    "גברים"))

data_for_117 <- dt_for_117


plot_for_117 <- 
  data_for_117 %>% 
  ggplot() +
  aes(sex, value, fill = variable) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = floor(value)),
            position = position_stack(vjust = 0.5),
            size = 2.8) +
  facet_grid(.~ dirog_kiboz)





















# יצוא נתונים ----
OUT <- createWorkbook()
options("openxlsx.numFmt" = "#,#0")


# 96
addWorksheet(OUT, "96")
writeDataTable(OUT, "96", data_for_96)
print(plot_for_96)
insertPlot(OUT, "96", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)


# 97
addWorksheet(OUT, "97")
writeDataTable(OUT, "97", data_for_97)
print(plot_for_97)
insertPlot(OUT, "97", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 98
addWorksheet(OUT, "98")
writeDataTable(OUT, "98", data_for_98)
print(plot_for_98)
insertPlot(OUT, "98", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 99
addWorksheet(OUT, "99")
writeDataTable(OUT, "99", data_for_99)
print(plot_for_99_1)
insertPlot(OUT, "99", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)
print(plot_for_99_2)
insertPlot(OUT, "99", xy = c("J", 20), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 100
addWorksheet(OUT, "100")
writeDataTable(OUT, "100", data_for_100)
print(plot_for_100)
insertPlot(OUT, "100", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 101
# 
addWorksheet(OUT, "101")
writeDataTable(OUT, "101", data_for_101)
print(plot_for_101)
insertPlot(OUT, "101", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 102
addWorksheet(OUT, "102")
writeDataTable(OUT, "102", data_for_102)
print(plot_for_102)
insertPlot(OUT, "102", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 103
addWorksheet(OUT, "103")
writeData(OUT, "103", data_for_103)
print(plot_for_103)
insertPlot(OUT, "103", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 104
addWorksheet(OUT, "104")
writeDataTable(OUT, "104", data_for_104)
print(plot_for_104)
insertPlot(OUT, "104", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 105
# עמוד שאני לא מפיק
addWorksheet(OUT, "105")
#writeDataTable(OUT, "105", data_for_105)
#print(plot_for_105)
#insertPlot(OUT, "105", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 106
addWorksheet(OUT, "106")
writeDataTable(OUT, "106", data_for_106)
print(plot_for_106)
insertPlot(OUT, "106", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 107
# לא מוכן
# addWorksheet(OUT, "107")
# writeDataTable(OUT, "107", data_for_107)
# print(plot_for_107)
# insertPlot(OUT, "107", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 108
addWorksheet(OUT, "108")
writeDataTable(OUT, "108", data_for_108)
print(plot_for_108)
insertPlot(OUT, "108", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 109
addWorksheet(OUT, "109")
writeDataTable(OUT, "109", data_for_109)
print(plot_for_109)
insertPlot(OUT, "109", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 110
addWorksheet(OUT, "110")
writeDataTable(OUT, "110", data_for_110)
print(plot_for_110)
insertPlot(OUT, "110", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)


# 111
addWorksheet(OUT, "111")
writeDataTable(OUT, "111", data_for_111)
print(plot_for_111)
insertPlot(OUT, "111", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 112
addWorksheet(OUT, "112")
writeDataTable(OUT, "112", data_for_112)
print(plot_for_112)
insertPlot(OUT, "112", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 113
addWorksheet(OUT, "113")
writeDataTable(OUT, "113", data_for_113)
print(plot_for_113)
insertPlot(OUT, "113", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 114
addWorksheet(OUT, "114")
writeDataTable(OUT, "114", data_for_114_1, xy = c("A", 1))
writeDataTable(OUT, "114", data_for_114_2, xy = c("A", 10))
print(plot_for_114_1)
insertPlot(OUT, "114", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)
print(plot_for_114_2)
insertPlot(OUT, "114", xy = c("J", 10), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)


# 115
addWorksheet(OUT, "115")
writeDataTable(OUT, "115", data_for_115)
print(plot_for_115)
insertPlot(OUT, "115", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 116
addWorksheet(OUT, "116")
writeDataTable(OUT, "116", data_for_116)
print(plot_for_116)
insertPlot(OUT, "116", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 117
addWorksheet(OUT, "117")
writeDataTable(OUT, "117", data_for_117)
print(plot_for_117)
insertPlot(OUT, "117", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# Export the file ----
saveWorkbook(OUT, "O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\macro_for_19_21_08_16.xlsx",
             overwrite = FALSE)



