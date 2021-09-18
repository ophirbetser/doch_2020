options(scipen=999) # Disables scientific notation          
options(digits=6)   # Limits the number of digits printed       

Sys.setlocale("LC_ALL", locale = "Hebrew")
#------------------------------------------------------------------------------#
# setup ----
if (!require("pacman")){                                  
  install.packages("pacman")}                            
pacman::p_load(pacman, data.table, patchwork,forcats, ggeasy,
               ggplot2, dplyr, visdat, openxlsx, DT, glue,
               lubridate, shiny, esquisse)

#------------------------------------------------------------------------------#
# תקיית העבודה של נתוני הפרויקט היא:
setwd("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA")

rm(list = ls())

# mem ----
mem_20 <- fread("mem\\mem_2020_data_21_08_18.csv", encoding = "UTF-8")
mem_19 <- fread("mem\\mem_2019_data_21_08_18.csv", encoding = "UTF-8")
mem_18 <- fread("mem\\mem_2018_data_21_08_18.csv", encoding = "UTF-8")
mem_17 <- fread("mem\\mem_2017_data_21_08_18.csv", encoding = "UTF-8")
mem_16 <- fread("mem\\mem_2016_data_21_08_18.csv", encoding = "UTF-8")
mem_15 <- fread("mem\\mem_2015_data_21_08_18.csv", encoding = "UTF-8")
mem_14 <- fread("mem\\mem_2014_data_21_08_18.csv", encoding = "UTF-8")
mem_13 <- fread("mem\\mem_2013_data_21_08_18.csv", encoding = "UTF-8")
mem_12 <- fread("mem\\mem_2012_data_21_08_18.csv", encoding = "UTF-8")

# עמודה 16 מכילה תאריכי לידה, אני מסיר אותה
# כי היא עושה לי בעיות באיחוד
# של הנתונים לסט נתונים אחד

mem_12_to_20 <- rbind(
  mem_12[, -16], 
  mem_13[, -16], 
  mem_14[, -16], 
  mem_15[, -16], 
  mem_16[, -16], 
  mem_17[, -16], 
  mem_18[, -16], 
  mem_19[, -16], 
  mem_20[, -16])

# שינוי פורמט התאריך לפורמט נכון
mem_12_to_20$`תאריך שכר` <- as_date(mem_12_to_20$`תאריך שכר`)


# שינוי שמות עמודות לאנגלית

setnames(mem_12_to_20, "מספר עובדים", "num_ovedim")
setnames(mem_12_to_20, "ת.ז. של עובד", "id")
setnames(mem_12_to_20, "תאריך שכר", "date")
setnames(mem_12_to_20, "מגדר", "sex")
setnames(mem_12_to_20, "אגף", "agaf")
setnames(mem_12_to_20, "קוד אגף", "code_agaf")

setnames(mem_12_to_20, "משכורת ברוטו שוטף", "sal_bruto_shotef")
setnames(mem_12_to_20, "משכורת ברוטו שוטף והפרשים", "sal_bruto_shotef_hefrshim")
setnames(mem_12_to_20, "יסוד משולב", "isod_msolav")
setnames(mem_12_to_20, "עבודה נוספת", "avoda_noseft")
setnames(mem_12_to_20, "החזר הוצאות", "ehzer_hozaot")
setnames(mem_12_to_20, "תשלומים אחרים", "taclomim_aharim")
setnames(mem_12_to_20, "תוספות", "tosef")

setnames(mem_12_to_20, "פרישה עשירונים סכום", "asironim")
setnames(mem_12_to_20, "מענקים סכום", "mankim")

setnames(mem_12_to_20, "חלקיות חודש", "halki_hodesh")
setnames(mem_12_to_20, "חלקיות חודש משרה", "halki_hodesh_misra")
setnames(mem_12_to_20, "חלקיות משרה", "halki_misra")

setnames(mem_12_to_20, "קוד דירוג", "code_dirog")
setnames(mem_12_to_20, "דירוג", "dirog")
setnames(mem_12_to_20, "קוד דרגה", "code_darga")
setnames(mem_12_to_20, "דרגה", "darga")

setnames(mem_12_to_20, "כמות תורנויות מתמחים", "cmut_toran_mitmaha")
setnames(mem_12_to_20, "סכום תורנויות מתמחים", "schum_toran_mitmaha")

# ניקוי ערכים חסרים בתורנויות
mem_12_to_20$cmut_toran_mitmaha <- ifelse(is.na(mem_12_to_20$cmut_toran_mitmaha),
                                          0,
                                          mem_12_to_20$cmut_toran_mitmaha)


# הוספת עמודה שמציינת מאיזה שנה השורה
mem_12_to_20$year <- year(mem_12_to_20$date)
# מאיפה הנתונים
mem_12_to_20$from <- "mem"


# הוספת דירוג מקובץ
mem_12_to_20$dirog_kiboz <- fcase(
  mem_12_to_20$code_dirog == 1, "מנהלי",
  mem_12_to_20$code_dirog %in% c(7, 8), "רנטגנאים",
  mem_12_to_20$code_dirog %in% c(33, 43), "עובדי מעבדה",
  mem_12_to_20$code_dirog %in% c(47, 48, 548), "פארה רפואיים",
  mem_12_to_20$code_dirog %in% c(964, 164, 555, 557, 57, 566, 568, 569, 148, 948, 87, 587), "חוזים אישים בבתי חולים",
  mem_12_to_20$code_dirog == 11, "אקדמאים בהסכם קיבוצי",
  mem_12_to_20$code_dirog == 12, "מהנדסים",
  mem_12_to_20$code_dirog == 13, "הנדסאים וטכנאים",
  mem_12_to_20$code_dirog == 16, "פסיכולוגים",
  mem_12_to_20$code_dirog == 20, "פזיותרפיסתים",
  mem_12_to_20$code_dirog == 24, "עובדים סוציאלים",
  mem_12_to_20$code_dirog == 31 & mem_12_to_20$code_darga == 40, "רופאים סטאזרים",
  mem_12_to_20$code_dirog == 31 & mem_12_to_20$code_darga != 40, "רופאים ללא סטאזרים",
  mem_12_to_20$code_dirog == 34, "רוקחים",
  mem_12_to_20$code_dirog %in% c(38, 39 ), "אחים ואחיות כולל",
  mem_12_to_20$code_dirog == 49, "מרפאים בעיסוק",
  default = "אחר")

# יצירת רמת מומחיות רופאים

top_menel_mem <- 
  c(52046935,
    57258436,
    59284067,
    29478708,
    316829738,
    54154596,
    55437115,
    53420915,
    56126295,
    56203268,
    57698490,
    22092779,
    26043711,
    59680421,
    17511312,
    56284532,
    55976070,
    308708999,
    53935318,
    31868904,
    59004911,
    54003702,
    24224842)

mem_12_to_20$level_doc <-
  fcase(mem_12_to_20$code_darga %in% c(40) , "סטזר",
        mem_12_to_20$code_darga %in% c(50, 70, 75) , "מתמחה",
        mem_12_to_20$code_darga %in% c(85, 80), "מומחה צעיר",
        mem_12_to_20$code_darga %in% c(90, 91, 95, 96), "מומחה בכיר",
        mem_12_to_20$code_darga %in%  c(100, 102, 110, 112, 120, 122, 130, 132, 140, 142) & !(mem_12_to_20$id %in% top_menel_mem), "מנהל",
        mem_12_to_20$code_darga %in%  c(100, 102, 110, 112, 120, 122, 130, 132, 140, 142) & (mem_12_to_20$id %in% top_menel_mem), "מנהל בית חולים",
        mem_12_to_20$code_darga %in% c(210, 220, 230, 240, 250, 260), "תחומי",
        default = "")



# הוספת אזור אגף

mem_12_to_20$ezor_agaf <-
  fcase(mem_12_to_20$code_agaf %in% c(207,107) , "פריפריה 1",
        mem_12_to_20$code_agaf %in% c(643,507,313,307,413,410,137), "פריפריה 2+3",
        mem_12_to_20$code_agaf %in% c(543,343,807,27,210,113,407,910,813,613,510,710,707), "מרכז",
        default = "")


# הוספת סוג בית חולים

mem_12_to_20$hos_type <-
  fcase(mem_12_to_20$code_agaf %in% c(807, 507, 27, 307, 407, 137, 707, 107, 207), "כלליים",
        mem_12_to_20$code_agaf %in% c(210, 910, 410, 510, 710), "גריאטריים",
        mem_12_to_20$code_agaf %in% c(643, 543, 343, 313, 113, 813, 613, 413), "פסיכיאטריים",
        mem_12_to_20$code_agaf %in% c(146, 346, 616, 716, 816, 916), "לשכות",
        mem_12_to_20$code_agaf %in%  c(106), "מטה",
        default = "")



# clalit ----

# טעינת נתונים
clalit_20 <- fread("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\clalit\\ניסיון לתיקון שליפה\\clalit_20.csv", encoding = "UTF-8")
clalit_19 <- fread("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\clalit\\ניסיון לתיקון שליפה\\clalit_19.csv", encoding = "UTF-8")
clalit_18 <- fread("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\clalit\\ניסיון לתיקון שליפה\\clalit_18.csv", encoding = "UTF-8")
clalit_17 <- fread("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\clalit\\ניסיון לתיקון שליפה\\clalit_17.csv", encoding = "UTF-8")
clalit_16 <- fread("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\clalit\\ניסיון לתיקון שליפה\\clalit_16.csv", encoding = "UTF-8")
clalit_15 <- fread("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\clalit\\ניסיון לתיקון שליפה\\clalit_15.csv", encoding = "UTF-8")
clalit_14 <- fread("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\clalit\\ניסיון לתיקון שליפה\\clalit_14.csv", encoding = "UTF-8")
clalit_13 <- fread("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\clalit\\ניסיון לתיקון שליפה\\clalit_13.csv", encoding = "UTF-8")
clalit_12 <- fread("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\clalit\\ניסיון לתיקון שליפה\\clalit_12.csv", encoding = "UTF-8")

# איחוד
clalit_12_to_20 <- rbind(
  clalit_12,
  clalit_13,
  clalit_14,
  clalit_15,
  clalit_16,
  clalit_17,
  clalit_18,
  clalit_19,
  clalit_20)


# שינוי פורמט התאריך לפורמט נכון
clalit_12_to_20$`תאריך שכר` <- as_date(clalit_12_to_20$`תאריך שכר`)

names(clalit_12_to_20)
# שינוי שמות עמודות לאנגלית

setnames(clalit_12_to_20, "מספר עובדים", "num_ovedim")
setnames(clalit_12_to_20, "ת\"\"ז עובד", "id")
setnames(clalit_12_to_20, "תאריך שכר", "date")
setnames(clalit_12_to_20, "מגדר", "sex")
setnames(clalit_12_to_20, "אגף", "agaf")
setnames(clalit_12_to_20, "קוד אגף", "code_agaf")

setnames(clalit_12_to_20, "משכורת ברוטו שוטף רע", "sal_bruto_shotef_bad")
setnames(clalit_12_to_20, "משכורת ברוטו הפרשים רע", "sal_hefrshim_bad")
setnames(clalit_12_to_20, "משכורת ברוטו שוטף והפרשים", "sal_bruto_shotef_hefrshim")
setnames(clalit_12_to_20, "יסוד משולב", "isod_msolav")
setnames(clalit_12_to_20, "עבודה נוספת", "avoda_noseft")
setnames(clalit_12_to_20, "החזר הוצאות", "ehzer_hozaot")
setnames(clalit_12_to_20, "תשלומים אחרים", "taclomim_aharim")
setnames(clalit_12_to_20, "סכום כוננויות רופאים", "conenion_doc")
setnames(clalit_12_to_20, "סכום תורנויות רופאים", "toran_doc")
setnames(clalit_12_to_20, "קצת סכום", "kzat")
setnames(clalit_12_to_20, "סכום תוספת מתמחה", "extra_for_mitmaha")

setnames(clalit_12_to_20, "עיסוק", "esok")


# תוספות
clalit_12_to_20$tosef <- ifelse(is.na(clalit_12_to_20$`סה""כ תוספות שכר` ),
                                0,
                                clalit_12_to_20$`סה""כ תוספות שכר`)

# יש ערכים חסרים בעבודה נוספת
clalit_12_to_20$avoda_noseft <- ifelse(is.na(clalit_12_to_20$avoda_noseft),
                                       0,
                                       clalit_12_to_20$avoda_noseft)

# יש ערכים חסרים בהחזר הוצארות
clalit_12_to_20$ehzer_hozaot <- ifelse(is.na(clalit_12_to_20$ehzer_hozaot),
                                       0,
                                       clalit_12_to_20$ehzer_hozaot)

# כנל בתשלומים אחרים
clalit_12_to_20$taclomim_aharim <- ifelse(is.na(clalit_12_to_20$taclomim_aharim),
                                          0,
                                          clalit_12_to_20$taclomim_aharim)


# ובתוספות מתמחים
clalit_12_to_20$extra_for_mitmaha <- ifelse(is.na(clalit_12_to_20$extra_for_mitmaha), 
                                            0, 
                                            clalit_12_to_20$extra_for_mitmaha)


setnames(clalit_12_to_20, "חלקיות חודש", "halki_hodesh")
setnames(clalit_12_to_20, "חלקיות משרה", "halki_misra")

setnames(clalit_12_to_20, "פרישה עשירונים סכום", "asironim")
setnames(clalit_12_to_20, "מענקים סכום", "mankim")


setnames(clalit_12_to_20, "קוד דרוג מקובץ", "code_dirog")
setnames(clalit_12_to_20, "שם דרוג מקובץ", "dirog")
setnames(clalit_12_to_20, "קוד דרגה", "code_darga")
setnames(clalit_12_to_20, "דרגה", "darga")

setnames(clalit_12_to_20, "כמות תורנויות מתמחים", "cmut_toran_mitmaha")
setnames(clalit_12_to_20, "סכום תורנויות מתמחים", "schum_toran_mitmaha")

# ניקוי ערכים חסרים בתורנויות
clalit_12_to_20$cmut_toran_mitmaha <- ifelse(is.na(clalit_12_to_20$cmut_toran_mitmaha),
                                             0,
                                             clalit_12_to_20$cmut_toran_mitmaha)



# ונוריד את הקצת מהשכר בכללית
clalit_12_to_20$kzat <- ifelse(is.na(clalit_12_to_20$kzat),
                               0,
                               clalit_12_to_20$kzat)

clalit_12_to_20$sal_bruto_shotef_hefrshim <- 
  clalit_12_to_20$sal_bruto_shotef_hefrshim - clalit_12_to_20$kzat


# יש בעיה בכללית עם חלקיות המשרה של רופאים בסטג.
# נשנה אותה ל 1
clalit_12_to_20[code_dirog == 31 & code_darga == 1, halki_misra := 1] 


# בכללית אנו משאירים רק את העובדים שהחלקיות 
# חודש שלהם היא 1
# ושיש להם שכר יסוד חיובי
clalit_12_to_20 <- 
  clalit_12_to_20[#halki_hodesh == 1 &
    halki_misra > 0 &
      isod_msolav > 0]



#d <- 
#clalit_12_to_20[year == 2012, .(halki_hodesh,
#            halki_misra,
#            isod_msolav)]
#
#d[, .N, by = halki_hodesh]


# שנה
clalit_12_to_20$year <- year(clalit_12_to_20$date)
# מאיפה הנתונים
clalit_12_to_20$from <- "clalit"

# קיבוץ דירוגים

clalit_12_to_20$dirog_kiboz <- fcase(
  clalit_12_to_20$code_dirog == 1, "מנהלי",
  clalit_12_to_20$code_dirog %in% c(8), "רנטגנאים",
  clalit_12_to_20$code_dirog %in% c(44), "עובדי מעבדה",
  clalit_12_to_20$code_dirog %in% c(47), "פארה רפואיים",
  clalit_12_to_20$code_dirog %in% c(87), "אקדמאים בהסכם קיבוצי",
  clalit_12_to_20$code_dirog %in% c(1090), "חוזים אישים בבתי חולים",
  clalit_12_to_20$code_dirog == 12, "מהנדסים",
  clalit_12_to_20$code_dirog == 13, "הנדסאים וטכנאים",
  clalit_12_to_20$code_dirog == 16, "פסיכולוגים",
  clalit_12_to_20$code_dirog == 20, "פזיותרפיסתים",
  clalit_12_to_20$code_dirog == 24, "עובדים סוציאלים",
  clalit_12_to_20$code_dirog == 31 & clalit_12_to_20$code_darga == 1, "רופאים סטאזרים",
  clalit_12_to_20$code_dirog == 31 & clalit_12_to_20$code_darga != 1, "רופאים ללא סטאזרים",
  clalit_12_to_20$code_dirog == 34, "רוקחים",
  clalit_12_to_20$code_dirog %in% c(39), "אחים ואחיות כולל",
  clalit_12_to_20$code_dirog == 49, "מרפאים בעיסוק",
  default = "אחר")


# הוספת רמת מומחיות של רופאים

top_menel_clalit <-
  c(13061189,
    22112726,
    55976021,
    58027343,
    58877119,
    59104760,
    59064659,
    54967096,
    12563938,
    59688986,
    54501028,
    57953507)




clalit_12_to_20$level_doc <-
  fcase(clalit_12_to_20$code_darga %in% c(1) , "סטזר",
        clalit_12_to_20$code_darga %in% c(2, 3, 13) , "מתמחה",
        clalit_12_to_20$code_darga %in% c(4, 5), "מומחה צעיר",
        clalit_12_to_20$code_darga %in% c(6, 7, 16, 17), "מומחה בכיר",
        clalit_12_to_20$code_darga %in%  c(8,9,10,11,12, 18,19,20,21,22) & !(clalit_12_to_20$id %in% top_menel_clalit), "מנהל",
        clalit_12_to_20$code_darga %in%  c(8,9,10,11,12, 18,19,20,21,22) & (clalit_12_to_20$id %in% top_menel_clalit), "מנהל בית חולים",
        clalit_12_to_20$code_darga %in% c(31,32,33,34), "תחומי",
        default = "")

# הוספת אזור אגף

clalit_12_to_20$ezor_agaf <-
  fcase(clalit_12_to_20$code_agaf %in% c() , "פריפריה 1",
        clalit_12_to_20$code_agaf %in% c(30, 24, 26), "פריפריה 2+3",
        clalit_12_to_20$code_agaf %in% c(61, 64, 60, 22, 65, 69, 21, 28, 23, 20), "מרכז",
        default = "")


# הוספת סוג בית חולים

clalit_12_to_20$hos_type <-
  fcase(clalit_12_to_20$code_agaf %in% c(30, 22, 21, 28, 24, 26, 23, 20), "כלליים",
        clalit_12_to_20$code_agaf %in% c(61, 64, 60), "גריאטריים",
        clalit_12_to_20$code_agaf %in% c(65, 69), "פסיכיאטריים",
        clalit_12_to_20$code_agaf %in% c(), "לשכות",
        clalit_12_to_20$code_agaf %in%  c(), "מטה",
        default = "")


# bnizion ----
bnizion_09_20 <- fread("bnizion\\bnizion_2009_to_2020_data_21_08_15.csv", encoding = "UTF-8")

# סידור פורמט התאריך
bnizion_09_20$`תאריך שכר` <- as_date(bnizion_09_20$`תאריך שכר`)

# שינוי שמות עמודות

setnames(bnizion_09_20, "ת\"\"ז", "id")
setnames(bnizion_09_20, "תאריך שכר", "date")
setnames(bnizion_09_20, "מספר עובדים", "num_ovedim")
setnames(bnizion_09_20, "מגדר", "sex")

setnames(bnizion_09_20, "גוף", "agaf")

setnames(bnizion_09_20, "סמל דירוג", "code_dirog")
setnames(bnizion_09_20, "דירוג", "dirog")
setnames(bnizion_09_20, "סמל דרגה", "code_darga")
setnames(bnizion_09_20, "דרגה", "darga")

# צריך לייצר את העמודות האלה
# עבור בני ציון
# כי הם לא מופיעות באופן הרגיל ב
# BI
bnizion_09_20$tosef <- bnizion_09_20$`(2) תוספות שכר + 1` - bnizion_09_20$`(1) שכר משולב`
bnizion_09_20$avoda_noseft <- bnizion_09_20$`(3) עבודה נוספת + 2` - bnizion_09_20$`(2) תוספות שכר + 1`
bnizion_09_20$ehzer_hozaot <- bnizion_09_20$`(4) החזר הוצאות + 3` - bnizion_09_20$`(3) עבודה נוספת + 2`
bnizion_09_20$taclomim_aharim <- bnizion_09_20$`(5) תשלומים אחרים + 4` - bnizion_09_20$`(4) החזר הוצאות + 3`

setnames(bnizion_09_20, "(5) תשלומים אחרים + 4", "sal_bruto_shotef")
setnames(bnizion_09_20, "סכום לתשלום", "sal_bruto_shotef_hefrshim")
setnames(bnizion_09_20, "(1) שכר משולב", "isod_msolav")

setnames(bnizion_09_20, "חלקיות משרה", "halki_misra")

# נוסיף עמודה זהותית שווה לאחד עבור מספר עובדים
# num_ovdim
bnizion_09_20$num_ovedim <- 1

# נבצע סינונים
# אני פשוט מעיף תלושים עם חלקיות משרה אפס
bnizion_09_20 <- 
  bnizion_09_20[halki_misra > 0]

# שנה
bnizion_09_20$year <- year(bnizion_09_20$date)
# מאיפה הנתונים
bnizion_09_20$from <- "bnizion"

# קיבוץ דירוגים

bnizion_09_20$dirog_kiboz <- fcase(
  bnizion_09_20$code_dirog == 5, "מנהלי",
  bnizion_09_20$code_dirog %in% c(40), "רנטגנאים",
  bnizion_09_20$code_dirog %in% c(64, 65), "עובדי מעבדה",
  bnizion_09_20$code_dirog %in% c(15), "פארה רפואיים",
  bnizion_09_20$code_dirog %in% c(24), "חוזים אישים בבתי חולים",
  bnizion_09_20$code_dirog == 72, "אקדמאים בהסכם קיבוצי",
  bnizion_09_20$code_dirog == 74, "מהנדסים",
  bnizion_09_20$code_dirog == 77, "הנדסאים וטכנאים",
  bnizion_09_20$code_dirog == 81, "פסיכולוגים",
  bnizion_09_20$code_dirog == 13, "פזיותרפיסתים",
  bnizion_09_20$code_dirog == 86, "עובדים סוציאלים",
  bnizion_09_20$code_dirog == 41, "רופאים סטאזרים",
  bnizion_09_20$code_dirog == 68, "רופאים ללא סטאזרים",
  bnizion_09_20$code_dirog == 82, "רוקחים",
  bnizion_09_20$code_dirog %in% c(80), "אחים ואחיות כולל",
  bnizion_09_20$code_dirog == 14, "מרפאים בעיסוק",
  default = "אחר")

# רמת מומחיות של רופאים

top_menel_bnizion <- c(24892481)

bnizion_09_20$level_doc <-
  fcase(bnizion_09_20$code_darga %in% c(1), "סטזר",
        bnizion_09_20$code_darga %in% c(2, 3, 13) , "מתמחה",
        bnizion_09_20$code_darga %in% c(4, 5), "מומחה צעיר",
        bnizion_09_20$code_darga %in% c(6, 16, 7, 17), "מומחה בכיר",
        bnizion_09_20$code_darga %in%  c(10, 20, 22, 8, 18, 9, 19) & !(bnizion_09_20$id %in% top_menel_bnizion), "מנהל",
        bnizion_09_20$code_darga %in%  c(10, 20, 22, 8, 18, 9, 19) & (bnizion_09_20$id %in% top_menel_bnizion), "מנהל בית חולים",
        bnizion_09_20$code_darga %in% c(43, 53, 32, 33), "תחומי",
        default = "")

# הוספת אזור אגף
# תכלס רק מרכז.....
bnizion_09_20$ezor_agaf <-
  fcase(bnizion_09_20$agaf %in% c() , "פריפריה 1",
        bnizion_09_20$agaf %in% c(), "פריפריה 2+3",
        bnizion_09_20$agaf %in% unique(bnizion_09_20$agaf), "מרכז",
        default = "")

# סוג בית חולים

bnizion_09_20$hos_type <- "כלליים"


# ehilov ----
ehilov_20 <- fread("ehilov\\ehilov_2020_data_21_08_18.csv", encoding = "UTF-8")
ehilov_19 <- fread("ehilov\\ehilov_2019_data_21_08_18.csv", encoding = "UTF-8")
ehilov_18 <- fread("ehilov\\ehilov_2018_data_21_08_18.csv", encoding = "UTF-8")
ehilov_17 <- fread("ehilov\\ehilov_2017_data_21_08_18.csv", encoding = "UTF-8")
ehilov_16 <- fread("ehilov\\ehilov_2016_data_21_08_18.csv", encoding = "UTF-8")
ehilov_15 <- fread("ehilov\\ehilov_2015_data_21_08_18.csv", encoding = "UTF-8")
ehilov_14 <- fread("ehilov\\ehilov_2014_data_21_08_18.csv", encoding = "UTF-8")
ehilov_13 <- fread("ehilov\\ehilov_2013_data_21_08_18.csv", encoding = "UTF-8")
ehilov_12 <- fread("ehilov\\ehilov_2012_data_21_08_18.csv", encoding = "UTF-8")

# איחוד
ehilov_12_to_20 <- rbind(
  ehilov_12,
  ehilov_13,
  ehilov_14,
  ehilov_15,
  ehilov_16,
  ehilov_17,
  ehilov_18,
  ehilov_19,
  ehilov_20)

# שינוי פורמט תאריך
ehilov_12_to_20$`תאריך שכר` <- as_date(ehilov_12_to_20$`תאריך שכר`)

# שינוי שמות העמודות

setnames(ehilov_12_to_20, "מספר עובדים", "num_ovedim")
setnames(ehilov_12_to_20, "ת.ז. של עובד", "id")
setnames(ehilov_12_to_20, "תאריך שכר", "date")
setnames(ehilov_12_to_20, "מגדר", "sex")
setnames(ehilov_12_to_20, "אגף", "agaf")
setnames(ehilov_12_to_20, "קוד אגף", "code_agaf")

setnames(ehilov_12_to_20, "משכורת ברוטו שוטף", "sal_bruto_shotef")
setnames(ehilov_12_to_20, "משכורת ברוטו שוטף והפרשים", "sal_bruto_shotef_hefrshim")
setnames(ehilov_12_to_20, "יסוד משולב", "isod_msolav")
setnames(ehilov_12_to_20, "עבודה נוספת", "avoda_noseft")
setnames(ehilov_12_to_20, "החזר הוצאות", "ehzer_hozaot")
setnames(ehilov_12_to_20, "תשלומים אחרים", "taclomim_aharim")

setnames(ehilov_12_to_20, "פרישה עשירונים סכום", "asironim")
setnames(ehilov_12_to_20, "מענקים סכום", "mankim")

setnames(ehilov_12_to_20, "חלקיות חודש", "halki_hodesh")
setnames(ehilov_12_to_20, "חלקיות משרה", "halki_misra")

setnames(ehilov_12_to_20, "קוד דירוג", "code_dirog")
setnames(ehilov_12_to_20, "דירוג", "dirog")
setnames(ehilov_12_to_20, "קוד דרגה", "code_darga")
setnames(ehilov_12_to_20, "דרגה", "darga")

setnames(ehilov_12_to_20, "כמות תורנויות מתמחים", "cmut_toran_mitmaha")
setnames(ehilov_12_to_20, "סכום תורנויות מתמחים", "schum_toran_mitmaha")

# ניקוי ערכים חסרים בתורנויות
ehilov_12_to_20$cmut_toran_mitmaha <- ifelse(is.na(ehilov_12_to_20$cmut_toran_mitmaha),
                                             0,
                                             ehilov_12_to_20$cmut_toran_mitmaha)

# אנחנו שומרים באיכלוב רק את העובדיםפ שהחלקיות חודש שלהם
# היא 1
ehilov_12_to_20 <- 
  ehilov_12_to_20[halki_hodesh == 1]


# הוספת עמודה שמציינת מאיזה שנה השורה
ehilov_12_to_20$year <- year(ehilov_12_to_20$date)
# מאיפה הנתונים
ehilov_12_to_20$from <- "ehilov"

# הוספת דירוג מקובץ

ehilov_12_to_20$dirog_kiboz <- fcase(
  ehilov_12_to_20$code_dirog == 2, "מנהלי",
  ehilov_12_to_20$code_dirog %in% c(7, 8), "רנטגנאים",
  ehilov_12_to_20$code_dirog %in% c(33, 43), "עובדי מעבדה",
  ehilov_12_to_20$code_dirog %in% c(47, 48, 548), "פארה רפואיים",
  ehilov_12_to_20$code_dirog %in% c(964, 164, 555, 557, 57, 566, 568, 569, 148, 948, 87, 587), "חוזים אישים בבתי חולים",
  ehilov_12_to_20$code_dirog == 11, "אקדמאים בהסכם קיבוצי",
  ehilov_12_to_20$code_dirog == 12, "מהנדסים",
  ehilov_12_to_20$code_dirog == 13, "הנדסאים וטכנאים",
  ehilov_12_to_20$code_dirog == 16, "פסיכולוגים",
  ehilov_12_to_20$code_dirog == 20, "פזיותרפיסתים",
  ehilov_12_to_20$code_dirog == 24, "עובדים סוציאלים",
  ehilov_12_to_20$code_dirog == 31 & ehilov_12_to_20$code_darga == 40, "רופאים סטאזרים",
  ehilov_12_to_20$code_dirog == 31 & ehilov_12_to_20$code_darga != 40, "רופאים ללא סטאזרים",
  ehilov_12_to_20$code_dirog == 34, "רוקחים",
  ehilov_12_to_20$code_dirog %in% c(38, 39 ), "אחים ואחיות כולל",
  ehilov_12_to_20$code_dirog == 49, "מרפאים בעיסוק",
  default = "אחר")

# יצירת רמת מומחיות רופאים

top_menel_ehiloc <- c(22269963)

ehilov_12_to_20$level_doc <-
  fcase(ehilov_12_to_20$code_darga %in% c(40), "סטזר",
        ehilov_12_to_20$code_darga %in% c(50, 70, 75) , "מתמחה",
        ehilov_12_to_20$code_darga %in% c(80, 85), "מומחה צעיר",
        ehilov_12_to_20$code_darga %in% c(90, 91, 95, 96), "מומחה בכיר",
        ehilov_12_to_20$code_darga %in%  c(100, 102, 110, 112, 120, 122, 130, 132, 142) & !(ehilov_12_to_20$id %in% top_menel_ehiloc), "מנהל",
        ehilov_12_to_20$code_darga %in%  c(100, 102, 110, 112, 120, 122, 130, 132, 142) & (ehilov_12_to_20$id %in% top_menel_ehiloc), "מנהל בית חולים",
        ehilov_12_to_20$code_darga %in% c(210, 220, 230, 240, 250, 260), "תחומי",
        default = "")


# הוספת אזור אגף

ehilov_12_to_20$ezor_agaf <-
  fcase(ehilov_12_to_20$code_agaf %in% c() , "פריפריה 1",
        ehilov_12_to_20$code_agaf %in% c(), "פריפריה 2+3",
        ehilov_12_to_20$code_agaf %in% c(45), "מרכז",
        default = "")

# סוג בתי חולים

ehilov_12_to_20$hos_type <-
  fcase(ehilov_12_to_20$code_agaf %in% c(45), "כלליים",
        default = "")



# rbind ----
# הכנת הנתונים לאיחוד

# נוסיף לאיכילוב
#ולממשלתיים ולבניציון עמודה 
# זהותית 0
# עבור קצתים
# כדי שנוכל לאחד
mem_12_to_20$kzat <- 0
ehilov_12_to_20$kzat <- 0
bnizion_09_20$kzat <- 0

# עשירונים
# בבני ציון אין לנו מידע על הרכיבי
# שכר שאנחנו מורידים כאשר אנחנו מחשבים את גרפי העשירונים
# לכן אני אוסיף עמודה של NA
# בתור עשירונים לבני ציון
bnizion_09_20$asironim <- NA


# אין תוספות באיכילוב,
# נוסיף
# NA
ehilov_12_to_20$tosef <- NA

# אין בבני ציון כמות ותוספות כוננויות מתמחים
bnizion_09_20$cmut_toran_mitmaha <- NA
bnizion_09_20$schum_toran_mitmaha <- NA

# השכר הפרשים בכללית לא טוב
# נוסיף עמודה זהותית 0
# באותו השם
clalit_12_to_20$sal_bruto_shotef <- 0


# וקטור של שמירת השמות של העמודות שאחנו ורצים
# שיהיו באיחוד
# ויצרו לנו גם סדר אחיד של העמודות באיחוד
keep_cols <- c("id",  # כללי
               "date",
               "sex",
               "dirog_kiboz", # דרגות ודירוגים
               "code_dirog",
               "dirog",
               "code_darga",               
               "darga",
               # "halki_hodesh", # חלקיות העסקה           אין בבני ציון
               # "halki_hodesh_misra", אין לי בכללית
               "halki_misra",
               "sal_bruto_shotef_hefrshim", # שכר
               "sal_bruto_shotef",
               "isod_msolav",
               "avoda_noseft",
               "ehzer_hozaot",
               "tosef", # אין לי כרגע באיכילוב
               #"mankim", # אין לי בכללית
               #"asironim" # אין לי בכללית
               "taclomim_aharim",
               "from",
               "year",
               "asironim", #אין לי בבני ציון
               
               "cmut_toran_mitmaha",
               "schum_toran_mitmaha",
               "hos_type",
               "ezor_agaf",
               "num_ovedim",
               "kzat",
               "level_doc")


all_health_12_to_20 <- 
  rbind(
    mem_12_to_20[, ..keep_cols],
    ehilov_12_to_20[, ..keep_cols],
    clalit_12_to_20[, ..keep_cols],
    bnizion_09_20[, ..keep_cols])

all_health_12_to_20 <- 
  all_health_12_to_20[year >= 2012]


# נוסיף מספר עמודות שימושיות
all_health_12_to_20$from_type <-
  fcase(all_health_12_to_20$from == "mem", "ממשלתיים",
        all_health_12_to_20$from == "clalit", "כללית",
        default = "עירוניים")


all_health_12_to_20$dirog_kiboz_for_97 <- fcase(
  all_health_12_to_20$dirog_kiboz %in% c("מנהלי", "אקדמאים בהסכם קיבוצי", "מהנדסים", "הנדסאים וטכנאים", "חוזים אישים בבתי חולים"),
  "מנהל ומשק",
  all_health_12_to_20$dirog_kiboz %in% c("רוקחים", "פארה רפואיים", "פזיותרפיסתים", "עובדים סוציאלים", "פסיכולוגים", "מרפאים בעיסוק"),
  "פארה רפואיים",
  all_health_12_to_20$dirog_kiboz %in% c("רופאים סטאזרים", "רופאים ללא סטאזרים"),
  "רופאים",
  all_health_12_to_20$dirog_kiboz %in% c("אחים ואחיות כולל"),
  "אחים ואחיות",
  default = "אחר")

all_health_12_to_20$level_doc_for_104 <- 
  ifelse(all_health_12_to_20$level_doc %in% c("מנהל בית חולים", "מנהל"),
         "מנהל",
         all_health_12_to_20$level_doc)



# שמירת הנתונים
# מאוחד
readr::write_excel_csv(all_health_12_to_20, 
                       glue("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\merge_data\\full_date_{Sys.Date()}.csv"))
# של כללית
readr::write_excel_csv(clalit_12_to_20, 
                       glue("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\merge_data\\clalit_12_to_20_{Sys.Date()}.csv"))


#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#




#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#




#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

# START HERE ----
#rm(list = ls())
`%not_in%` <- purrr::negate(`%in%`)
setwd("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA")
options(scipen=999) # Disables scientific notation          
options(digits=6)   # Limits the number of digits printed       
Sys.setlocale("LC_ALL", locale = "Hebrew")
if (!require("pacman")){                                  
  install.packages("pacman")}                            
pacman::p_load(pacman, data.table, patchwork,forcats, ggeasy,
               ggplot2, dplyr, visdat, openxlsx, DT, glue,
               lubridate, shiny, esquisse)


# טעינת הנתונים המאוחדים השמורים ----
all_health_12_to_20 <- 
  fread("merge_data\\full_date_2021-08-26.csv", 
        encoding = "UTF-8")

clalit_12_to_20 <- 
  fread("merge_data\\clalit_12_to_20_2021-08-26.csv", 
        encoding = "UTF-8")


# בנוסף נשמור סט נתונים נפרד עבור 
# 2020
# ועבור
# 2019
all_health_20 <- all_health_12_to_20[year == 2020]
all_health_19 <- all_health_12_to_20[year == 2019]


#-------------------------------------------------------------------------------#
# start analysis ----

# 96 ----

code_for_96 <- function(YEAR){
  return(glue::glue('
# התפלגות לפי רמת שכר

# משכורת מתוקננת
all_health_{YEAR}$sal_bruto_shotef_hefrshim_for_full_misra <- 
  all_health_{YEAR}$sal_bruto_shotef_hefrshim / all_health_{YEAR}$halki_misra

# משכורת מתוקננת שנתית
all_health_{YEAR}[,
           mean_sal_bruto_shotef_hefrshim_for_full_misra := 
             sum(sal_bruto_shotef_hefrshim) / sum(halki_misra),
           by = id]

# הוספת מספר ההופעות של העובד בנתונים
all_health_{YEAR}[,
           sum_num_ovedim := sum(num_ovedim),
           by = id]


# מעבר לנתונים ברמת תעודת הזהות
all_health_by_id_20{YEAR} <- 
  all_health_{YEAR}[,
             .SD[c(1)],
             by = id]



all_health_by_id_20{YEAR}$sal_chank_mean <- 
  fcase(all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra < 6000, "6K-",
        all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(6000, 9000), "6K-9K",
        all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(9000, 12000), "9K-12K",
        all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(12000, 15000), "12K-15K",
        all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(15000, 20000), "15K-20K",
        all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(20000, 25000), "20K-25K",
        all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(25000, 30000), "25K-30K",
        all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(30000, 40000), "30K-40K",
        all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(40000, 50000), "40K-50K",
        all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra %between% c(50000, 60000), "50K-60K",
        all_health_by_id_20{YEAR}$mean_sal_bruto_shotef_hefrshim_for_full_misra > 60000, "60k+")


all_health_by_id_20{YEAR}$sal_chank_mean <- factor(all_health_by_id_20{YEAR}$sal_chank_mean,
                                          levels = c("6K-", 
                                                     "6K-9K",
                                                     "9K-12K",
                                                     "12K-15K",
                                                     "15K-20K",
                                                     "20K-25K",
                                                     "25K-30K",
                                                     "30K-40K",
                                                     "40K-50K",
                                                     "50K-60K",
                                                     "60k+"))



# גרף ונתונים ל 96
data_for_96_20{YEAR} <- 
  all_health_by_id_20{YEAR}[,
              .(.N, sum_ovedim = sum(sum_num_ovedim) / 12), 
              by = sal_chank_mean][, sum_ovedim_p := (sum_ovedim / sum(sum_ovedim))]

plot_for_96_20{YEAR} <- 
  data_for_96_20{YEAR} %>% 
  ggplot() + 
  aes(x = sal_chank_mean, y = sum_ovedim_p) +
  geom_bar(stat = "identity",
           width = 1,
           fill = "#212F3D",
           color = "white") +
  geom_text(aes(x = sal_chank_mean, label = scales::percent(sum_ovedim_p)),
            vjust=-0.25,
            size = 4) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.31), 
                     breaks = seq(0, 0.5, 0.05),
                     labels= scales::percent(seq(0, 0.5, 0.05),
                                             accuracy = 1,
                                             suffix = "%")) +
  scale_x_discrete(guide = guide_axis(angle = 90))  +
  theme_classic() +
  labs(title = "התפלגות העובדים לפי רמות שכר",
       subtitle = "שכר למשרה מלאה, נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 20{YEAR}",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_text(face="bold", size = 12, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")) '))
}

#  הרצת הקוד
eval(parse(text = code_for_96(19)))
eval(parse(text = code_for_96(20)))



# 97 ----


code_for_97 <- function(YEAR){
  return(glue::glue('
  data_for_97_20{YEAR} <- 
  all_health_12_to_20[year %between% c(2012, 20{YEAR}),
         .(sum_halki_misra = sum(halki_misra)),
         by = .(year, dirog_kiboz_for_97)][,
                                           sum_2011 := min(sum_halki_misra),
                                           by = dirog_kiboz_for_97][,
                                                                    ":="(ratio = (sum_halki_misra / sum_2011),
                                                                         ratio_sub_1 = (sum_halki_misra / sum_2011) - 1)][
                                                                           , sum_12 := sum_halki_misra/12]




data_for_97_20{YEAR}$dirog_kiboz_for_97 <- factor(data_for_97_20{YEAR}$dirog_kiboz_for_97, 
                                         levels = c("אחר",
                                                    "פארה רפואיים",
                                                    "אחים ואחיות",
                                                    "רופאים",
                                                    "מנהל ומשק"))



data_for_97_20{YEAR}$label_p_20{YEAR} <- 
  ifelse(data_for_97_20{YEAR}$year == 20{YEAR},
         data_for_97_20{YEAR}$ratio_sub_1,
         NA)

data_for_97_20{YEAR}$label_sum_misra_20{YEAR} <- 
  ifelse(data_for_97_20{YEAR}$year == 20{YEAR},
         data_for_97_20{YEAR}$sum_halki_misra / 12,
         NA)


plot_for_97_20{YEAR} <- 
  data_for_97_20{YEAR} %>% 
  ggplot() +
  aes(x = year, y = ratio_sub_1, color = dirog_kiboz_for_97) +
  geom_line(size = 1.5) +
  scale_x_continuous(limits = c(2012, 20{YEAR}.3),
                     breaks = seq(2012, 20{YEAR}, 1),
                     labels = seq(2012, 20{YEAR}, 1)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-0.0, max(data_for_97_20{YEAR}$ratio_sub_1) + 0.01),
                     breaks = seq(0, max(data_for_97_20{YEAR}$ratio_sub_1), 0.05),
                     labels= scales::percent(seq(0, max(data_for_97_20{YEAR}$ratio_sub_1), 0.05),
                                             accuracy = 1)) +
  geom_text(aes(label=scales::percent(label_p_20{YEAR},
                                      accuracy = 1)),
            hjust = -0.1) +
  geom_text(aes(label=scales::comma(label_sum_misra_20{YEAR},
                                    accuracy = 1,
                                    prefix = " | ")),
            hjust = -0.6) +
  
  theme_classic() + 
  scale_fill_manual(values = c("#273746", "#45B39D")) +
  labs(title = "שיעור גידול המשרות לפי שנים",
       subtitle = ", מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, אייכלוב ובניציון), 2012 - 20{YEAR}",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") + 
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "bottom",
        legend.title = element_blank()) +
  easy_remove_y_axis(what = c("line")) +
  scale_color_manual(values = c("#707B7C", "#58D68D", "#239B56", "#2874A6", "#154360"))'))
}


#  הרצת הקוד
eval(parse(text = code_for_97(19)))
eval(parse(text = code_for_97(20)))


# 98 ----

code_for_98 <- function(YEAR){
  return(glue::glue('
# שכר לפי דירוגים

num_misrot_{YEAR} <- sum(all_health_{YEAR}$halki_misra) / 12 # 55069.5
tot_num_ovdim_{YEAR} <- sum(all_health_{YEAR}$num_ovedim) / 12 # 61603.4


dt_for_98_20{YEAR} <- 
  all_health_{YEAR}[,
             .(ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra),
               ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim = sum(sal_bruto_shotef_hefrshim) / sum(num_ovedim),
               mid = median(sal_bruto_shotef_hefrshim / halki_misra)),
             by = dirog_kiboz]

#dt_for_98 %>% View()

# נמיין את הלבליים של המשתנה שלנו מהגדול לקטן
dt_for_98_20{YEAR}$dirog_kiboz <- fct_reorder(dt_for_98_20{YEAR}$dirog_kiboz,
                                     -dt_for_98_20{YEAR}$ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra)

# הנתונים נראים טוב
dt_for_98_melt <- 
  melt.data.table(dt_for_98_20{YEAR}[dirog_kiboz != "בכירים" & dirog_kiboz != "אחר",
                            .(dirog_kiboz,
                              ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra,
                              ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim)],
                  id=1)

dt_for_98_melt_20{YEAR} <- merge(dt_for_98_melt, dt_for_98_20{YEAR}[, .(dirog_kiboz, mid)],
                        by = "dirog_kiboz")

# נוסיף את ממוצאי השכר של כלל המערכת:
dt_for_98_melt_20{YEAR} <- 
  rbind(dt_for_98_melt_20{YEAR}, list("מערכת הבריאות",
                             "ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra",
                             (sum(all_health_{YEAR}$sal_bruto_shotef_hefrshim) / num_misrot_{YEAR} / 12),
                             NA))

data_for_98_20{YEAR} <- 
  rbind(dt_for_98_melt_20{YEAR}, list("מערכת הבריאות",
                             "ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim",
                             (sum(all_health_{YEAR}$sal_bruto_shotef_hefrshim) / tot_num_ovdim_{YEAR} / 12),
                             NA))

data_for_98_20{YEAR}$labels_in_plot <- 
  ifelse(data_for_98_20{YEAR}$variable == "ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra",
         data_for_98_20{YEAR}$value,
         NA)

plot_for_98_20{YEAR} <- 
  data_for_98_20{YEAR} %>% 
  ggplot() + 
  aes(x=dirog_kiboz, y=value, fill=variable) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.5) +
  geom_point(aes(x = dirog_kiboz,
                 y = mid), 
             color = "orange",
             size = 10,
             shape = 95) +
  geom_text(aes(label = scales::comma(labels_in_plot,
                                      accuracy = 1,
                                      prefix = "\244")),
            position=position_dodge(width=0.75),
            vjust=-0.25,
            size = 2.8) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(40000)),
                     breaks = seq(0, 40000, 5000),
                    labels = scales::comma(seq(0, 40000, 5000),
                                            accuracy = 1,
                                            prefix = "\244")) +
  theme_classic() + 
  scale_fill_manual(values = c("#273746", "#45B39D")) +
  labs(title = "שכר ממוצע וחציוני בדירוגים נבחרים",
       subtitle = "שכר ממוצע לעובד ולמשרה מלאה, מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, אייכלוב ובניציון), 20{YEAR}",
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
        legend.position = "none")'))
}


#  הרצת הקוד
eval(parse(text = code_for_98(19)))
eval(parse(text = code_for_98(20)))

# 99 ----

code_for_99 <- function(YEAR){
  return(glue::glue('
# יש לעשות סינון של הקבוצות הדירוגים שמופיעות בדוח
# בעמוד הזה
dt_for_99_20{YEAR} <- 
  all_health_{YEAR}[! dirog_kiboz %in% c("בכירים"),
        .(sum_sal_bruto_shotef_hefrshim = sum(sal_bruto_shotef_hefrshim),
          sum_num_ovedim = sum(num_ovedim)),
        by = dirog_kiboz]

dt_for_99_20{YEAR}$ratio_sum_sal_bruto_shotef_hefrshim <- 
  dt_for_99_20{YEAR}$sum_sal_bruto_shotef_hefrshim / sum(dt_for_99_20{YEAR}$sum_sal_bruto_shotef_hefrshim)

dt_for_99_20{YEAR}$ratio_sum_num_ovedim <- 
  dt_for_99_20{YEAR}$sum_num_ovedim / sum(dt_for_99_20{YEAR}$sum_num_ovedim)

dt_for_99_20{YEAR}$dirog_kiboz <- fct_reorder(dt_for_99_20{YEAR}$dirog_kiboz,
                                     dt_for_99_20{YEAR}$ratio_sum_num_ovedim)

# שמירת הנתונים
data_for_99_20{YEAR} <- dt_for_99_20{YEAR}


plot_for_99_1_20{YEAR} <- 
  data_for_99_20{YEAR} %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = ratio_sum_sal_bruto_shotef_hefrshim) +
  geom_bar(stat = "identity", fill = "#2E86C1") +
  geom_text(aes(x = dirog_kiboz, label = scales::percent(ratio_sum_num_ovedim, 0.01)),
            hjust=-0.15,
            size = 3) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.46),
                     breaks = seq(0, 1, 0.05),
                     labels = scales::percent(seq(0, 1, 0.05), 1)) +  
  coord_flip() +
  theme_classic() +
  labs(title = "",
       subtitle = "",
       y = "",
       x = "",
       caption = "") +
  easy_remove_x_axis(what = c("line")) +
  theme(panel.grid.major.x = element_line(size=0.5, colour = "#939393"))



plot_for_99_2_20{YEAR} <- 
  data_for_99_20{YEAR} %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = ratio_sum_num_ovedim) +
  geom_bar(stat = "identity", fill = "#283747") +
  geom_text(aes(x = dirog_kiboz, label = scales::percent(ratio_sum_num_ovedim, 0.01)),
            hjust=-0.15,
            size = 3) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.46),
                     breaks = seq(0, 1, 0.05),
                     labels = scales::percent(seq(0, 1, 0.05), 1)) + 
  coord_flip() +
  theme_classic() +
  labs(title = "",
       subtitle = "",
       y = "",
       x = "",
       caption = "") +
  easy_remove_x_axis(what = c("line")) +
  theme(panel.grid.major.x = element_line(size=0.5, colour = "#939393"))'))
}

#  הרצת הקוד
eval(parse(text = code_for_99(19)))
eval(parse(text = code_for_99(20)))



# 100 ----
# אין עשירונים בבני ציון

code_for_100 <- function(YEAR){
  return(glue::glue('
# עשירונים

# נוסיף לכל עובד את סכום חלקיות המשרה השנתית שלו
all_health_{YEAR}[from != "bnizion", sum_halki_misra := sum(halki_misra), by = id]


# נוסיף עמודה לכל אדם של השכר הנקי שלו, על פי הניקוי שמופיע באפיון הדוח
all_health_{YEAR}$asironim <- ifelse(is.na(all_health_{YEAR}$asironim), 0, all_health_{YEAR}$asironim)
all_health_{YEAR}[,
   mean_clean := ((sum(sal_bruto_shotef_hefrshim) - sum(asironim)) / sum(halki_misra)),
   by = .(id, dirog_kiboz)]


# נעבור לקיבוץ על פי תעודת זהות
all_health__by_id_100_{YEAR} <- 
  all_health_{YEAR}[,
     .SD[c(.N)],
     by = .(id)]

# נסנן חלקיות משרה קטנה מ 2
all_health__by_id_100_{YEAR} <- all_health__by_id_100_{YEAR}[sum_halki_misra > 2]

# נוסיף חיווי לעשירון שכר
all_health__by_id_100_{YEAR}[, ntile := ntile(mean_clean, 10), by = dirog_kiboz]

# ניצור את הנתונים
data_for_100_20{YEAR} <- 
merge(
merge(
merge(
all_health__by_id_100_{YEAR}[ntile == 10 & !(dirog_kiboz %in% c("אחר","בכירים")),
             .(high_mean = round(mean(mean_clean))),
             by = dirog_kiboz],
all_health__by_id_100_{YEAR}[ntile == 1 & !(dirog_kiboz %in% c("אחר","בכירים")),
             .(low_mean = round(mean(mean_clean))),
             by = dirog_kiboz],
by = "dirog_kiboz"),
all_health__by_id_100_{YEAR}[!(dirog_kiboz %in% c("אחר","בכירים")),
             .(mean = round(mean(mean_clean))),
             by = dirog_kiboz],
by = "dirog_kiboz"),
all_health__by_id_100_{YEAR}[!(dirog_kiboz %in% c("אחר","בכירים")),
             .(median = round(median(mean_clean))),
             by = dirog_kiboz],
by = "dirog_kiboz")



# סידור הלייבלים
# נמיין את הלבליים של המשתנה שלנו מהגדול לקטן
data_for_100_20{YEAR}$dirog_kiboz <- fct_reorder(data_for_100_20{YEAR}$dirog_kiboz,
                                     data_for_100_20{YEAR}$mean)




plot_for_100_20{YEAR} <-
  data_for_100_20{YEAR} %>% 
  ggplot() +
  geom_segment(aes(y = dirog_kiboz,
                   yend = dirog_kiboz,
                   x = low_mean,
                   xend = high_mean),
               size = 4,
               color = "#45B39D") +
  geom_point(aes(y = dirog_kiboz,
                 x = mean),
             color = "yellow") +
  geom_point(aes(y = dirog_kiboz,
                 x = median),
             color = "blue") +
  geom_text(aes(y = dirog_kiboz,
                x = low_mean,
                label = scales::comma(low_mean)),
            hjust = 1.1) +
  geom_text(aes(y = dirog_kiboz,
                x = high_mean,
                label = scales::comma(high_mean)),
            hjust = -0.1) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, max(data_for_100_20{YEAR}$high_mean) + 3000),
                     breaks = seq(0, max(data_for_100_20{YEAR}$high_mean) + 5000, 10000),
                     labels = scales::comma(seq(0, max(data_for_100_20{YEAR}$high_mean) + 5000, 10000),
                                            accuracy = 1,
                                            prefix = "\244")) +
  labs(title = "עשירוני שכר לפי דירוגים",
       subtitle = "שכר למשרה מלאה, מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, אייכלוב ובניציון), 20{YEAR}",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme_classic() +
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_text(face="bold", size = 12, color = "black"),
        #axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.x = element_line(size=0.5, colour = "#939393")) +
  easy_remove_x_axis(what = c("line"))'))
}

#  הרצת הקוד
eval(parse(text = code_for_100(19)))
eval(parse(text = code_for_100(20)))



# 101 ----

code_for_101 <- function(YEAR){
  return(glue::glue('
# הגרף הזה הולך להיות מבוסס על נתוני הגרף הקודם
# נוסיף את היחס בין העשירון העליון והתחתון לכל עשירון
data_for_100_20{YEAR}$ratio <- data_for_100_20{YEAR}$high_mean / data_for_100_20{YEAR}$low_mean

# נמיין את הלבליים של המשתנה שלנו מהגדול לקטן
data_for_100_20{YEAR}$dirog_kiboz <- fct_reorder(data_for_100_20{YEAR}$dirog_kiboz,
                                        data_for_100_20{YEAR}$ratio)


# שמירת הנתונים בשם
data_for_101_20{YEAR} <- data_for_100_20{YEAR}
# וניצור את הגרף

plot_for_101_20{YEAR} <- 
data_for_101_20{YEAR} %>% 
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
       subtitle = "היחס בין שכר העשירון העליון לשכר העשירון התחתון, מערכת הבריאות, נתונים מאוחדים, 20{YEAR}",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme_classic() +
  theme(axis.text.x = element_text( size = 12, color = "black"),
        #axis.text.y = element_blank(),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.x = element_line(size=0.5, colour = "#939393")) +
  coord_flip() +
  easy_remove_x_axis(what = c("line"))'))
}

#  הרצת הקוד
eval(parse(text = code_for_101(19)))
eval(parse(text = code_for_101(20)))



# 102 ----

code_for_102 <- function(YEAR){
  return(glue::glue('

# נוסיף חיווי על חלקיות המשרה של העובד
# יצירת קיבציי קבוצות היקף משרה
all_health_{YEAR}$halki_misra_chank <-
  fcase(all_health_{YEAR}$halki_misra < 0.25 , "25",
        all_health_{YEAR}$halki_misra < 0.5 , "25 to 50",
        all_health_{YEAR}$halki_misra < 0.75, "50 to 75",
        all_health_{YEAR}$halki_misra < 0.95, "75 to 95",
        all_health_{YEAR}$halki_misra >= 0.95, "95 +")


data_for_102_20{YEAR} <- 
  merge(
  all_health_{YEAR}[,
     .N,
     by = .(dirog_kiboz, halki_misra_chank)],
all_health_{YEAR}[,
      .(mean_h = mean(halki_misra)),
      by = .(dirog_kiboz)],
by = "dirog_kiboz")


data_for_102_20{YEAR}[,
           ratio := round(N / sum(N), 2),
           by = dirog_kiboz ]


data_for_102_20{YEAR}$dirog_kiboz <- fct_reorder(data_for_102_20{YEAR}$dirog_kiboz,
                                      data_for_102_20{YEAR}$mean_h)


plot_for_102_20{YEAR} <- 
  data_for_102_20{YEAR} %>% 
  ggplot() +
  aes(x = dirog_kiboz, y = ratio, fill = halki_misra_chank) +
  geom_bar(stat = "identity",
           position = "fill",
           width = 0.8) +
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
       subtitle = "מערכת הבריאות , מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, אייכלוב ובניציון) 20{YEAR}",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")) +
  scale_fill_manual(values = c("#2E86C1", "#1D8348", "#7DCEA0", "#1C2833", "#566573"))'))
}

eval(parse(text = code_for_102(19)))
eval(parse(text = code_for_102(20)))


# 103 ----
code_for_103 <- function(YEAR){
  return(glue::glue('

# הכנת הנתונים לגרף
data_for_103_20{YEAR} <- 
  all_health_{YEAR}[code_dirog == 31,
     .(NUM = .N,
       NUM_12 = .N /12,
       SAL = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra)),
     by = level_doc]

# מיון ליבליים
data_for_103_20{YEAR}$level_doc <- fct_reorder(data_for_103_20{YEAR}$level_doc,
                                      data_for_103_20{YEAR}$SAL)


data_for_103_20{YEAR}$zero <- 0


plot_for_103_20{YEAR} <- 
  data_for_103_20{YEAR} %>% ggplot() +
  aes(x = level_doc, y = SAL) +
  geom_bar(stat = "identity",
           width = 0.6,
           fill = "#212F3D",
           color = "white") +
  geom_text(aes(x = level_doc, label = round(SAL)),
            vjust = 0.3,
            hjust = 1.2,
            size = 3,
            color = "#FBFCFC") +
  geom_text(aes(x = level_doc, y = zero,label = round(NUM_12)),
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
       subtitle = "שכר ממוצע למשרה מלאה, מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, אייכלוב ובניציון), 20{YEAR}",
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
  coord_flip()'))
}

eval(parse(text = code_for_103(19)))
eval(parse(text = code_for_103(20)))


# 104 problem ----
all_health_12_to_20$level_doc_for_104
dt_104 <- 
  all_health_12_to_20[year %between% c(2012, 2020) & code_dirog == 31,
         .(sum_sal = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra)),
         by = .(year, level_doc_for_104)]

dt_104 <- 
merge(dt_104,
      dt_104[year == 2012, .(sum_2012 = sum_sal, level_doc_for_104)],
      by = "level_doc_for_104")

dt_104[,
       ":="(ratio = (sum_sal / sum_2012),
            ratio_sub_1 = (sum_sal / sum_2012) - 1)][
              , sum_12 := sum_sal/12]

data_for_104 <- dt_104


plot_for_104 <- 
  data_for_104[!level_doc_for_104 %in% c("", "תחומי")] %>% 
  ggplot() +
  aes(x = year, y = ratio_sub_1, color = level_doc_for_104) +
  geom_line(size = 1.5) +
  #geom_text(aes(x = 2019,
  #              y = ratio_2019,
  #              label = scales::percent(ratio_2019, 1)),
  #          hjust = -0.1) +
  scale_x_continuous(
    breaks = seq(2012, 2020, 1),
    labels = seq(2012, 2020, 1)) +
  #scale_y_continuous(expand = c(0, 0),
  #                   limits = c(-0.0, 0.91),
  #                   breaks = seq(0, 0.9, 0.1),
  #                   labels= scales::percent(seq(0, 0.9, 0.1),
  #                                           accuracy = 1)) +
  theme_classic() + 
  scale_fill_manual(values = c("#273746", "#45B39D")) +
  labs(title = "קצב גידול נומינלי בשכר הרופאים, לפי תפקידים",
       subtitle = "שיעור הגידול בשכר, מערכת הבריאות הממשלתית, 2012 - 2020",
       y = "",
       x = "",
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
# 105 ----



# 106 ----

# בגרף הזה אין הבדל בין מנהל לבין מנהל בית חולים,
# לכן אני אאחד אותם לאותו הלייבל
all_health_20$level_doc_for_106 <-
  ifelse(all_health_20$level_doc %in% c("מנהל בית חולים","מנהל"),
         "מנהל",
         all_health_20$level_doc)

# וכעת ניצור את הנתונים
data_for_106 <- 
  all_health_20[code_dirog == 31 & 
                  level_doc_for_106 != "סטזר" & 
                  level_doc_for_106 != "" &
                  ezor_agaf != "",
                .(NUM = .N,
                  NUM_12 = .N /12,
                  SAL = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra)),
                by = .(level_doc_for_106, ezor_agaf)]


data_for_106$level_doc_for_106 <- factor(data_for_106$level_doc_for_106,
                                         levels = c("מתמחה",
                                                    "תחומי",
                                                    "מומחה צעיר",
                                                    "מומחה בכיר",
                                                    "מנהל"))

data_for_106$ezor_agaf <- factor(data_for_106$ezor_agaf,
                                 levels = c("מרכז",
                                            "פריפריה 2+3",
                                            "פריפריה 1"))



# יצוא

plot_for_106 <- 
  data_for_106 %>% 
  ggplot() +
  aes(x = level_doc_for_106, y = SAL, fill = ezor_agaf) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single")) +
  geom_text(aes(x = level_doc_for_106, label = scales::comma(SAL, accuracy = 1)),
            position=position_dodge(width = 0.9),
            vjust = -0.5,
            hjust = 0.5,
            size = 3) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 80000),
                     breaks = seq(0, 80000, 10000),
                     labels= scales::comma(seq(0, 80000, 10000),
                                           accuracy = 1, prefix = "\244")) +
  theme_classic() +
  labs(title = "שכר רופאים, לפי דרגה ואזור בית חולים",
       subtitle = "שכר ממוצע למשרה מלאה, נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
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
  scale_fill_manual(values = c("#17A589", "#7FB3D5", "#2471A3", "#212F3D"))


# 107 ----
# 108 ----
# 109 ----
# 110 ----
# 111 ----
# 112 ----
# 113 ----


#------------------------------------------------------------------------------#

# 114 ----

num_misrot_20 <- sum(all_health_20$halki_misra) / 12 # 55069.5
tot_num_ovdim_20 <- sum(all_health_20$num_ovedim) / 12 # 61603.4


# צריך לאחד את השמות של המגדרים
all_health_20$sex <- 
  fcase(all_health_20$sex %in% c("זכר", "גברים"), 
        "גברים",
        all_health_20$sex %in% c("נקבה", "נשים"), 
        "נשים")


data_for_114_2 <-  
  all_health_20[, .(ratio = sum(num_ovedim) / tot_num_ovdim_20 / 12),
                by = sex]

data_for_114_1 <-  
  all_health_20[, .(mean_sal = sum(sal_bruto_shotef_hefrshim) / sum(num_ovedim)),
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
       subtitle = "נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
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
       subtitle = "נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
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
# הכנת הנתונים

dt_for_115 <- 
  all_health_20[dirog_kiboz != "אחר",
                .(count = .N),
                by = .(dirog_kiboz, sex)]

# סידור לייבלים
dt_for_115$sex <- factor(dt_for_115$sex,
                         levels = c("נשים",
                                    "גברים"))

#dt_for_115$dirog_kiboz <- 
#  factor(dt_for_115$dirog_kiboz, 
#         levels = c("רופאים ללא סטאזרים",
#                    "חוזים אישים בבתי חולים",
#                    "אחים ואחיות כולל",
#                    "מהנדסים",
#                    "עובדי מעבדה",
#                    "הנדסאים וטכנאים",
#                    "רוקחים",
#                    "רנטגנאים",
#                    "אקדמאים בהסכם קיבוצי",
#                    "מנהלי",
#                    "רופאים סטאזרים",
#                    "עובדים סוציאלים",
#                    "פזיותרפיסתים",
#                    "פארה רפואיים",
#                    "פסיכולוגים",
#                    "מרפאים בעיסוק"))
#

dt_for_115[, tot_count := sum(count),
           by = dirog_kiboz][, ratio := count / tot_count]


#dt_for_115 <- 
#  merge.data.table(dt_for_115,
#                   data_for_98_2020[dirog_kiboz != "אחר",
#                             .(dirog_kiboz, ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim)],
#                   by = "dirog_kiboz")
#
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
  #geom_text(aes(x = dirog_kiboz,
  #              y = one,
  #              label = round(ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim)),
  #          vjust = 1,
  #          size = 4,
  #          color = "black") +
  
  scale_fill_manual(values = c("#D68910", "#273746")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.01), 
                     breaks = seq(0, 1, 0.1),
                     labels= scales::percent(seq(0, 1, 0.1),
                                             accuracy = 1,
                                             suffix = "%")) +
  labs(title = "התפלגות מגדרית לפי דירוגים שונים",
       subtitle = "נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
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
  all_health_20[,
                .(ratio_sum_sal_bruto_shotef_hefrshim_sum_halki_misra = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra),
                  ratio_sum_sal_bruto_shotef_hefrshim_sum_num_ovedim = sum(sal_bruto_shotef_hefrshim) / sum(num_ovedim)),
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
       subtitle = "נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
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

# אין תוספוןת באיכילוב
# אז נעשה את הגרף הזה בלעדיו
all_health_no_ehiloc <- all_health_20[from != "ehilov"]


# ומתחיל ניתוח
all_health_no_ehiloc$hefrshim <- 
  (all_health_no_ehiloc$sal_bruto_shotef_hefrshim - all_health_no_ehiloc$sal_bruto_shotef)

dt_for_117 <- 
  all_health_no_ehiloc[dirog_kiboz %in% c("רופאים ללא סטאזרים", "אחים ואחיות כולל"), 
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

# יצוא
data_for_117 <- dt_for_117


plot_for_117 <- 
  data_for_117 %>% 
  ggplot() +
  aes(sex, value, fill = variable) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = floor(value)),
            position = position_stack(vjust = 0.5),
            size = 2.8) +
  facet_grid(.~ dirog_kiboz) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 42000),
                     breaks = seq(0, 42000, 5000),
                     labels= scales::comma(seq(0, 42000, 5000),
                                           accuracy = 1, prefix = "\244")) +
  theme_classic() + 
  #scale_fill_manual(values = c("#273746", "#45B39D")) +
  labs(title = "רכיבי שכר לפי מגדר ועיסוק",
       subtitle = "נתונים מאוחדים (ממשלתיים, כללית ובני ציון), 2020",
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
        legend.position = "bottom") +
  easy_remove_y_axis(what = c("line"))



#------------------------------------------------------------------------------#

# מספר עובדים ומספר משרות ----


# פיבוט עבור הגרף
data_for_num_misrot <- 
  all_health_20[, .(num_misrot = sum(halki_misra) / 12,
                    tot_num_ovdim = sum(num_ovedim) / 12),
                by = from_type] %>% melt("from_type")

# שינוי סדר הפקטורים
data_for_num_misrot$from_type <- factor(data_for_num_misrot$from_type,
                                        levels = c("ממשלתיים",
                                                   "כללית",
                                                   "עירוניים"))

data_for_num_misrot$variable <- factor(data_for_num_misrot$variable,
                                       levels = c("tot_num_ovdim",
                                                  "num_misrot"))

# יצירת הגרף
plot_for_num_misrot <- 
  data_for_num_misrot %>% 
  ggplot() +
  aes(x = from_type, y = value, fill = variable) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.5) +
  geom_text(aes(label = scales::comma(value,
                                      accuracy = 1)),
            position=position_dodge(width=0.5),
            vjust= -0.5,
            size = 3.5) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 35000), 
                     breaks = seq(0, 35000, 5000),
                     labels= scales::comma(seq(0, 35000, 5000),
                                           accuracy = 1)) +
  theme_classic() + 
  labs(title = "מספר עובדים לפי סוג גוף",
       subtitle = "נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.text.y = element_text(face="bold", size = 12, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position='top',
        legend.title=element_blank()) +
  scale_fill_manual(values = c("#273746", "#45B39D")) +
  easy_remove_y_axis(what = c("line"))



# שכר עבור סוג בית חולים ----
data_for_hos_type <- 
  all_health_20[,
                .(mean_sal = mean(sal_bruto_shotef_hefrshim)),
                by = .(from, hos_type)]


data_for_hos_type_docs <- 
  all_health_20[code_dirog == 31,
                .(mean_sal = mean(sal_bruto_shotef_hefrshim)),
                by = .(from, hos_type)]


plot_for_hos_type <- 
  data_for_hos_type[hos_type %in% c("כלליים", "פסיכיאטריים", "גריאטריים")] %>% 
  ggplot() +
  aes(x = hos_type, y = mean_sal, fill = from) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.7) +
  geom_text(aes(x = hos_type , label = scales::comma(mean_sal, accuracy = 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 6,
            color = "black") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 24100),
                     breaks = seq(0, 24100, 5000),
                     labels = scales::comma(seq(0, 24100, 5000), prefix = "\244")) +
  theme_classic() +
  scale_fill_manual(values = c("#D68910",
                               "#273746",
                               "#17A589",
                               "#0066cc")) + 
  labs(title = "שכר לפי סוג בית החולים - כלל האוכלוסיה",
       subtitle = "שכר למשרה מלאה, נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 16), 
        plot.title = element_text(hjust = 1, size = 20, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"))


plot_for_hos_type_doc <- 
  data_for_hos_type_docs[hos_type %in% c("כלליים", "פסיכיאטריים", "גריאטריים")] %>% 
  ggplot() +
  aes(x = hos_type, y = mean_sal, fill = from) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.7) +
  geom_text(aes(x = hos_type , label = scales::comma(mean_sal, accuracy = 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 6,
            color = "black") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 40100),
                     breaks = seq(0, 40100, 5000),
                     labels = scales::comma(seq(0, 40100, 5000), prefix = "\244")) +
  theme_classic() +
  scale_fill_manual(values = c("#D68910",
                               "#273746",
                               "#17A589",
                               "#0066cc")) + 
  labs(title = "שכר לפי סוג בית החולים - רופאים",
       subtitle = "שכר למשרה מלאה, נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 16), 
        plot.title = element_text(hjust = 1, size = 20, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"))


#----#

data_for_hos_type_no_from <- 
  all_health_20[,
                .(mean_sal = mean(sal_bruto_shotef_hefrshim)),
                by = .(hos_type)]


data_for_hos_type_docs_no_from <- 
  all_health_20[code_dirog == 31,
                .(mean_sal = mean(sal_bruto_shotef_hefrshim)),
                by = .(hos_type)]


plot_for_hos_type_no_from <- 
  data_for_hos_type_no_from[hos_type %in% c("כלליים", "פסיכיאטריים", "גריאטריים")] %>% 
  ggplot() +
  aes(x = hos_type, y = mean_sal, fill = hos_type) +
  geom_bar(stat = "identity",
           width = 0.7) +
  geom_text(aes(x = hos_type , label = scales::comma(mean_sal, accuracy = 1)),
            vjust = -0.5,
            size = 6,
            color = "black") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 24100),
                     breaks = seq(0, 24100, 5000),
                     labels = scales::comma(seq(0, 24100, 5000), prefix = "\244")) +
  theme_classic() +
  scale_fill_manual(values = c("#D68910",
                               "#273746",
                               "#17A589",
                               "#0066cc")) + 
  labs(title = "שכר לפי סוג בית החולים - כלל האוכלוסיה",
       subtitle = "שכר למשרה מלאה, נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 16), 
        plot.title = element_text(hjust = 1, size = 20, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "none")


plot_for_hos_type_doc_no_from <- 
  data_for_hos_type_docs_no_from[hos_type %in% c("כלליים", "פסיכיאטריים", "גריאטריים")] %>% 
  ggplot() +
  aes(x = hos_type, y = mean_sal, fill = hos_type) +
  geom_bar(stat = "identity",
           width = 0.7) +
  geom_text(aes(x = hos_type , label = scales::comma(mean_sal, accuracy = 1)),
            vjust = -0.5,
            size = 6,
            color = "black") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 40100),
                     breaks = seq(0, 40100, 5000),
                     labels = scales::comma(seq(0, 40100, 5000), prefix = "\244")) +
  theme_classic() +
  scale_fill_manual(values = c("#D68910",
                               "#273746",
                               "#17A589",
                               "#0066cc")) + 
  labs(title = "שכר לפי סוג בית החולים - רופאים",
       subtitle = "שכר למשרה מלאה, נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 16), 
        plot.title = element_text(hjust = 1, size = 20, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "none")

plot_for_hos_type_no_from + plot_for_hos_type_doc_no_from

# תורנויות מתמחים ----

# 1
# ezor_agaf
# בתי חולים כללים בלבד
data_for_cmut_toran_mitmaha_ezor_agaf_claliem <- 
  all_health_12_to_20[cmut_toran_mitmaha > 0 & level_doc == "מתמחה" & code_dirog == 31 & ezor_agaf != "" & hos_type == "כלליים", 
                      .(sum_cmut_toran_mitmaha = sum(cmut_toran_mitmaha),
                        N = .N,
                        sum_cmut_toran_mitmaha_per_mitmaha = sum(cmut_toran_mitmaha) / .N),
                      by = .(year, ezor_agaf)]


plot_for_cmut_toran_mitmaha_ezor_agaf_claliem <- 
  data_for_cmut_toran_mitmaha_ezor_agaf_claliem %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = sum_cmut_toran_mitmaha_per_mitmaha,
                group = ezor_agaf,
                color = ezor_agaf),
            size = 1.5) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(
    breaks = 2012:2020) +
  theme_classic() +
  labs(title = "מספר תורנויות למתמחה לפי אזור בית חולים, בתי חולים כללים בלבד",
       subtitle = "מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, איכילוב), 2012-2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 8, color = "black", angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(face="bold", size = 8, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")) 

plot_for_cmut_toran_mitmaha_ezor_agaf_claliem


# 2
# hos_type
# לפי סוג בית חולים
data_for_cmut_toran_mitmaha_hos_type <- 
  all_health_12_to_20[cmut_toran_mitmaha > 0 & level_doc == "מתמחה" & code_dirog == 31 & hos_type %not_in% c("מטה","לשכות",""), 
                      .(sum_cmut_toran_mitmaha = sum(cmut_toran_mitmaha),
                        N = .N,
                        sum_cmut_toran_mitmaha_per_mitmaha = sum(cmut_toran_mitmaha) / .N),
                      by = .(year, hos_type)]


plot_for_cmut_toran_mitmaha_hos_type <- 
  data_for_cmut_toran_mitmaha_hos_type %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = sum_cmut_toran_mitmaha_per_mitmaha,
                group = hos_type,
                color = hos_type),
            size = 1.5) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(
    breaks = 2012:2020) +
  theme_classic() +
  labs(title = "מספר תורנויות למתמחה לפי סוג בית חולים",
       subtitle = "מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, איכילוב), 2012-2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 8, color = "black", angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(face="bold", size = 8, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")) 

plot_for_cmut_toran_mitmaha_hos_type

# 3
# sex
# בתי חולים כללים בלבד

data_for_cmut_toran_mitmaha_sex_claliem <- 
  all_health_12_to_20[cmut_toran_mitmaha > 0 & level_doc == "מתמחה" & code_dirog == 31 & hos_type == "כלליים", 
                      .(sum_cmut_toran_mitmaha = sum(cmut_toran_mitmaha),
                        N = .N,
                        sum_cmut_toran_mitmaha_per_mitmaha = sum(cmut_toran_mitmaha) / .N),
                      by = .(year, sex)]


plot_for_cmut_toran_mitmaha_sex_claliem <- 
  data_for_cmut_toran_mitmaha_sex_claliem %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = sum_cmut_toran_mitmaha_per_mitmaha,
                group = sex,
                color = sex),
            size = 1.5) +
  scale_x_continuous(
    breaks = 2012:2020) +
  theme_classic() +
  labs(title = "מספר תורנויות למתמחה לפי אזור בית חולים, בתי חולים כללים בלבד",
       subtitle = "מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, איכילוב), 2012-2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 8, color = "black", angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(face="bold", size = 8, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")) +
  scale_color_manual(values = c("blue", "red"))



# 4
# נתוני כללית
# תורנויות לפי סוג התמחות
clalit_12_to_20$esok_k <- ifelse(clalit_12_to_20$esok %in% clalit_12_to_20[level_doc == "מתמחה" & code_dirog == 31,
                                                                           .N,
                                                                           by = esok][order(-N)]$esok[1:6],
                                 clalit_12_to_20$esok,
                                 "אחר")

data_for_cmut_toran_mitmaha_esok <- 
  clalit_12_to_20[cmut_toran_mitmaha > 0 & level_doc == "מתמחה" & code_dirog == 31, 
                      .(sum_cmut_toran_mitmaha = sum(cmut_toran_mitmaha),
                        N = .N,
                        sum_cmut_toran_mitmaha_per_mitmaha = sum(cmut_toran_mitmaha) / .N),
                      by = .(year, esok_k)][order(-year, -N)]


plot_for_cmut_toran_mitmaha_esok <- 
  data_for_cmut_toran_mitmaha_esok %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = sum_cmut_toran_mitmaha_per_mitmaha,
                group = esok_k,
                color = esok_k),
            size = 1.5) +
  scale_x_continuous(
    breaks = 2012:2020) +
  theme_classic() +
  labs(title = "מספר תורנויות למתמחה לפי סוג עיסוק",
       subtitle = "בתי חולים כללית, 2012-2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 8, color = "black", angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(face="bold", size = 8, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")) 


# נתוני שכר ברמה החודשית ----

# גרף מאוחד
data_sal_by_month_all <- 
  all_health_12_to_20[year %between% c(2017, 2020), 
                      .(sum_sal_bruto_shotef_hefrshim = sum(sal_bruto_shotef_hefrshim),
                        N = .N,
                        sum_halki_misra = sum(halki_misra),
                        sal_bruto_shotef_hefrshim_per_N = sum(sal_bruto_shotef_hefrshim) / .N,
                        sal_bruto_shotef_hefrshim_per_halki_misra = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra)),
                      by = date][,year := factor(year(date))][,month := factor(month(date))]

plot_sal_by_month_all <- 
  data_sal_by_month_all %>% 
  ggplot() +
  geom_line(aes(x = month, y = sal_bruto_shotef_hefrshim_per_halki_misra, group = year, color = year),
            size = 1.2) +
  theme_classic() +
  labs(title = "שכר חודשי, כלל האוכלוסיה",
       subtitle = "שכר ממוצע למשרה מלאה, מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2017-2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 8, color = "black"),
        axis.text.y = element_text(face="bold", size = 8, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")) +
  easy_remove_y_axis(what = c("line"))

plot_sal_by_month_all

# רופאים
data_sal_by_month_doc <- 
  all_health_12_to_20[year %between% c(2017, 2020) & dirog_kiboz == 'רופאים ללא סטאזרים', 
                      .(sum_sal_bruto_shotef_hefrshim = sum(sal_bruto_shotef_hefrshim),
                        N = .N,
                        sum_halki_misra = sum(halki_misra),
                        sal_bruto_shotef_hefrshim_per_N = sum(sal_bruto_shotef_hefrshim) / .N,
                        sal_bruto_shotef_hefrshim_per_halki_misra = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra)),
                      by = date][,year := factor(year(date))][,month := factor(month(date))]



plot_sal_by_month_doc <- 
  data_sal_by_month_doc %>% 
  ggplot() +
  geom_line(aes(x = month, y = sal_bruto_shotef_hefrshim_per_halki_misra, group = year, color = year),
            size = 1.2) +
  theme_classic() +
  labs(title = "שכר חודשי, רופאים",
       subtitle = "שכר ממוצע למשרה מלאה, מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2017-2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 8, color = "black"),
        axis.text.y = element_text(face="bold", size = 8, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        #legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")) +
  easy_remove_y_axis(what = c("line"))



#--------------------------------------------------------------------------------------#

# רופאים חדשים מול רופאים שעוזבים ----

unique_doc_12 <- unique(all_health_12_to_20[year == 2012 & code_dirog == 31]$id)
unique_doc_13 <- unique(all_health_12_to_20[year == 2013 & code_dirog == 31]$id)
unique_doc_14 <- unique(all_health_12_to_20[year == 2014 & code_dirog == 31]$id)
unique_doc_15 <- unique(all_health_12_to_20[year == 2015 & code_dirog == 31]$id)
unique_doc_16 <- unique(all_health_12_to_20[year == 2016 & code_dirog == 31]$id)
unique_doc_17 <- unique(all_health_12_to_20[year == 2017 & code_dirog == 31]$id)
unique_doc_18 <- unique(all_health_12_to_20[year == 2018 & code_dirog == 31]$id)
unique_doc_19 <- unique(all_health_12_to_20[year == 2019 & code_dirog == 31]$id)
unique_doc_20 <- unique(all_health_12_to_20[year == 2020 & code_dirog == 31]$id)


(unique_doc_12 %not_in% unique_doc_13) %>% sum()
(unique_doc_13 %not_in% unique_doc_12) %>% sum()


years_v <- 2013:2020
leave_v <- vector()
for(i in 12:19){
  leave_v <- c(leave_v, eval(parse(text = glue('unique_doc_{i} %not_in% unique_doc_{i+1}'))) %>% sum())
}

new_v <- vector()
for(i in 12:19){
  new_v <- c(new_v, eval(parse(text = glue('unique_doc_{i+1} %not_in% unique_doc_{i}'))) %>% sum())
}


new_and_leave <- as.data.table(cbind(years_v, leave_v, new_v))

data_for_new_and_leave <- 
  new_and_leave %>% melt("years_v") %>% 
  .[, var2 := ifelse(variable == "leave_v",
                     "עזבו",
                     "נכנסו")]
plot_for_new_and_leave <- 
  data_for_new_and_leave %>% 
  ggplot() +
  geom_line(aes(x = years_v, y = value, group = var2, color = var2),
            size = 2) +
  geom_point(aes(x = years_v, y = value, group = var2),
             size = 4) +
  
  scale_x_continuous(breaks = 2013:2020) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(800, 2200),
                     breaks = seq(800, 2200, 200)) + 
  theme_classic() +
  labs(title = "רופאים שנכנסים למערכת מול רופאים שעוזבים את המערכת",
       subtitle = "מערכת הבריאות, נתונים מאוחדים, (ממשלתיים, כללית, איכילוב ובני ציון), 2020-2013",
       y = "מספר עובדים",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  easy_remove_y_axis(what = c("line")) +
  scale_color_manual(values = c("#7FB3D5", "#890C0C")) +
  theme(axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "top", legend.direction = "horizontal",
        legend.background = element_rect(linetype = "blank"),
        legend.title=element_blank())



# מנורמל
new_and_leave <- as.data.table(cbind(years_v, leave_v = (leave_v / leave_v[1]), new_v = (new_v/ new_v[1])))

new_and_leave %>% melt("years_v") %>% 
  .[, var2 := ifelse(variable == "leave_v",
                     "עזבו",
                     "נכנסו")] %>%
  ggplot() +
  geom_line(aes(x = years_v, y = value, group = var2, color = var2),
            size = 2) +
  geom_point(aes(x = years_v, y = value, group = var2),
             size = 4) +
  
  scale_x_continuous(breaks = 2013:2020) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0.75, 2),
                     breaks = seq(0.75, 2, 0.25)) + 
  theme_classic() +
  labs(title = "רופאים שנכנסים למערכת מול רופאים שעוזבים את המערכת",
       subtitle = "מערכת הבריאות, נתונים מאוחדים, (ממשלתיים, כללית, איכילוב ובני ציון), 2020-2013",
       y = "מספר עובדים",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  easy_remove_y_axis(what = c("line")) +
  scale_color_manual(values = c("#7FB3D5", "#890C0C")) +
  theme(axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "top", legend.direction = "horizontal",
        legend.background = element_rect(linetype = "blank"),
        legend.title=element_blank())


# מתמידים ----

all_health_12_to_20[, .N, by = id] # 103K
all_health_12_to_20[, .N, by = id][N > 9*12*0.8] # 37507
matmidim_id <- all_health_12_to_20[, .N, by = id][N > 9*12*0.8]$id

# אמור להיות אותו הדבר
all_health_12_to_20[, .(.N, uniqueN(date)), by = id] # 103K



all_health_12_to_20[id %in% matmidim_id,
                    .(mean_sal = mean(sal_bruto_shotef_hefrshim)),
                    by = .(dirog_kiboz_for_97, year)] %>% 
  ggplot() +
  theme_classic() +
  geom_line(aes(x = year, y = mean_sal, group = dirog_kiboz_for_97 , color = dirog_kiboz_for_97 ),
            size = 2) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(2012,2020.2),
                     breaks = 2012:2020) +
  scale_y_continuous(breaks = seq(6000, 45000, 2000)) + 
  easy_remove_y_axis(what = c("line")) +
  theme(axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "top", legend.direction = "horizontal",
        legend.background = element_rect(linetype = "blank"),
        legend.title=element_blank())


data_change_sal_matmidim <- 
  all_health_12_to_20[id %in% matmidim_id,
                      .(mean_sal = mean(sal_bruto_shotef_hefrshim)),
                      by = .(dirog_kiboz_for_97, year)]

data_change_sal_matmidim <- 
  merge.data.table(data_change_sal_matmidim, 
                   data_change_sal_matmidim[year == 2012, .(dirog_kiboz_for_97 ,mean_sal_2012  = mean_sal)],
                   by = "dirog_kiboz_for_97")[, ratio := mean_sal / mean_sal_2012][,
                                                                                   ratio_2020 := ifelse(year == 2020, ratio, NA)]


plot_change_sal_matmidim <- 
  data_change_sal_matmidim %>% 
  ggplot() +
  theme_classic() +
  geom_line(aes(x = year, y = ratio, group = dirog_kiboz_for_97 , color = dirog_kiboz_for_97 ),
            size = 2) +
  geom_text(aes(x = 2020, y = ratio_2020, label = round(ratio_2020, 1)),
            hjust = -0.1) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(2012,2020.2),
                     breaks = 2012:2020) +
  scale_y_continuous(breaks = seq(0.8, 1.8, 0.1),
                     limits = c(0.97, 1.6)) + 
  easy_remove_y_axis(what = c("line")) +
  theme(axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "top", legend.direction = "horizontal",
        legend.background = element_rect(linetype = "blank"),
        legend.title=element_blank()) +
  labs(title = "שינוי בשכר של עובדים מתמידים",
       subtitle = "מערכת הבריאות, נתונים מאוחדים, (ממשלתיים, כללית, איכילוב ובני ציון), 2020-2012",
       y = "שינוי בשכר ממוצע",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר")

# לא מתמידים
data_change_sal_all <- 
  all_health_12_to_20[,
                      .(mean_sal = mean(sal_bruto_shotef_hefrshim)),
                      by = .(dirog_kiboz_for_97, year)]

data_change_sal_all <- 
  merge.data.table(data_change_sal_all, 
                   data_change_sal_all[year == 2012, .(dirog_kiboz_for_97 ,mean_sal_2012  = mean_sal)],
                   by = "dirog_kiboz_for_97")[, ratio := mean_sal / mean_sal_2012][,
                                                                                   ratio_2020 := ifelse(year == 2020, ratio, NA)]


plot_change_sal_all <- 
  data_change_sal_all %>% 
  ggplot() +
  theme_classic() +
  geom_line(aes(x = year, y = ratio, group = dirog_kiboz_for_97 , color = dirog_kiboz_for_97 ),
            size = 2) +
  geom_text(aes(x = 2020, y = ratio_2020, label = round(ratio_2020, 1)),
            hjust = -0.1) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(2012,2020.2),
                     breaks = 2012:2020) +
  scale_y_continuous(breaks = seq(0.8, 1.8, 0.1),
                     limits = c(0.97, 1.6)) + 
  easy_remove_y_axis(what = c("line")) +
  theme(axis.text.x = element_text(face="bold", size = 10, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "top", legend.direction = "horizontal",
        legend.background = element_rect(linetype = "blank"),
        legend.title=element_blank()) +
  labs(title = "שינוי בשכר של כלל העובדים",
       subtitle = "מערכת הבריאות, נתונים מאוחדים, (ממשלתיים, כללית, איכילוב ובני ציון), 2020-2012",
       y = "שינוי בשכר ממוצע",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר")



plot_change_sal_all + plot_change_sal_matmidim

# סוג התמחות ----

thom_by_id <- 
  fread("O:\\SacharSite\\Research\\דוח 2020\\בריאות מאוחד\\DATA\\מומחים\\output.csv",
        encoding = "UTF-8")


all_health_12_to_20 <- 
  merge(all_health_12_to_20, 
        thom_by_id[, .(id, last_thom)],
        by = 'id',
        all.x = T)


all_health_20_docs <- 
  all_health_12_to_20[year == 2020 & 
                        dirog_kiboz == "רופאים ללא סטאזרים"]


all_health_20_docs$last_thom_k <- ifelse(all_health_20_docs$last_thom %in%
                                           all_health_12_to_20[year == 2020 & 
                                                                 dirog_kiboz == "רופאים ללא סטאזרים",
                                                               .N,
                                                               by = last_thom][order(-N)]$last_thom[2:7],
                                         all_health_20_docs$last_thom,
                                         "אחר")


data_for_esok_type <- 
  all_health_20_docs[, .(mean_full_sal = mean(sal_bruto_shotef_hefrshim) / mean(halki_misra)),
                     by = last_thom_k][,last_thom_k := fct_reorder(last_thom_k, -mean_full_sal)]

plot_for_esok_type <- 
data_for_esok_type %>% 
  ggplot() +
  aes(x = last_thom_k, y = mean_full_sal) +
  geom_bar(stat = "identity",
           width = 0.6,
           fill = "#06608C") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 70000, 5000)) +
  easy_remove_y_axis(what = c("line")) +
  theme(axis.text.x = element_text(face="bold", size = 8, color = "black"),
        axis.text.y = element_text(face="bold", size = 10, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393"),
        legend.position = "top", legend.direction = "horizontal",
        legend.background = element_rect(linetype = "blank"),
        legend.title=element_blank()) +
  labs(title = "שכר ממצוע למשרה מלאה לפי סוג רישיון",
       subtitle = "מערכת הבריאות, נתונים מאוחדים, (ממשלתיים, כללית, איכילוב ובני ציון), 2020",
       y = "",
       x = "",
       caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר")
plot_for_esok_type








#------------------------------------------------------------------------------#
# יצוא נתונים ----
OUT <- createWorkbook()
options("openxlsx.numFmt" = "#,#0")


# 96
addWorksheet(OUT, "96")
writeDataTable(OUT, "96", data_for_96_2020)
print(plot_for_96_2020)
insertPlot(OUT, "96", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)


# 97
addWorksheet(OUT, "97")
writeDataTable(OUT, "97", data_for_97_2020)
print(plot_for_97_2020)
insertPlot(OUT, "97", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 98
addWorksheet(OUT, "98")
writeDataTable(OUT, "98", data_for_98_2020)
print(plot_for_98_2020)
insertPlot(OUT, "98", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 99
addWorksheet(OUT, "99")
writeDataTable(OUT, "99", data_for_99_2020)
print(plot_for_99_1_2020)
insertPlot(OUT, "99", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)
print(plot_for_99_2_2020)
insertPlot(OUT, "99", xy = c("E", 4), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 100
addWorksheet(OUT, "100")
writeDataTable(OUT, "100", data_for_100_2020)
print(plot_for_100_2020)
insertPlot(OUT, "100", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 101
# 
addWorksheet(OUT, "101")
writeDataTable(OUT, "101", data_for_101_2020)
print(plot_for_101_2020)
insertPlot(OUT, "101", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 102
addWorksheet(OUT, "102")
writeDataTable(OUT, "102", data_for_102_2020)
print(plot_for_102_2020)
insertPlot(OUT, "102", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 103
addWorksheet(OUT, "103")
writeData(OUT, "103", data_for_103_2020)
print(plot_for_103_2020)
insertPlot(OUT, "103", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 104
addWorksheet(OUT, "104")
writeDataTable(OUT, "104", data_for_104)
print(plot_for_104)
insertPlot(OUT, "104", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 105
# עמוד שאני לא מפיק
# מפיק אחד אחר על מתמידים במקום
addWorksheet(OUT, "105")
#writeDataTable(OUT, "105", data_for_105)
#print(plot_for_105)
#insertPlot(OUT, "105", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 106
addWorksheet(OUT, "106")
writeDataTable(OUT, "106", data_for_106)
print(plot_for_106)
insertPlot(OUT, "106", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 107 ----
# לא מוכן
addWorksheet(OUT, "107")
# writeDataTable(OUT, "107", data_for_107)
# print(plot_for_107)
# insertPlot(OUT, "107", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 108
addWorksheet(OUT, "108")
#writeDataTable(OUT, "108", data_for_108)
#print(plot_for_108)
#insertPlot(OUT, "108", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 109
addWorksheet(OUT, "109")
#writeDataTable(OUT, "109", data_for_109)
#print(plot_for_109)
#insertPlot(OUT, "109", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 110
addWorksheet(OUT, "110")
#writeDataTable(OUT, "110", data_for_110)
#print(plot_for_110)
#insertPlot(OUT, "110", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)


# 111
addWorksheet(OUT, "111")
#writeDataTable(OUT, "111", data_for_111)
#print(plot_for_111)
#insertPlot(OUT, "111", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 112
addWorksheet(OUT, "112")
#writeDataTable(OUT, "112", data_for_112)
#print(plot_for_112)
#insertPlot(OUT, "112", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 113
addWorksheet(OUT, "113")
#writeDataTable(OUT, "113", data_for_113)
#print(plot_for_113)
#insertPlot(OUT, "113", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 114
addWorksheet(OUT, "114")
writeDataTable(OUT, "114", data_for_114_1, xy = c("A", 1))
writeDataTable(OUT, "114", data_for_114_2, xy = c("A", 10))
print(plot_for_114_1)
insertPlot(OUT, "114", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)
print(plot_for_114_2)
insertPlot(OUT, "114", xy = c("E", 10), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)


# 115
addWorksheet(OUT, "115")
writeDataTable(OUT, "115", data_for_115)
print(plot_for_115)
insertPlot(OUT, "115", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 116
addWorksheet(OUT, "116")
writeDataTable(OUT, "116", data_for_116)
print(plot_for_116)
insertPlot(OUT, "116", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# 117
addWorksheet(OUT, "117")
writeDataTable(OUT, "117", data_for_117)
print(plot_for_117)
insertPlot(OUT, "117", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)


# מספר עובדים
addWorksheet(OUT, "מספר עובדים")
writeDataTable(OUT, "מספר עובדים", data_for_num_misrot)
print(plot_for_num_misrot)
insertPlot(OUT, "מספר עובדים", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# שכר לסוג גוף
# כללי אוכלוסיה
addWorksheet(OUT, "שכר לסוג גוף - כלל")
writeDataTable(OUT, "שכר לסוג גוף - כלל", data_for_hos_type_no_from)
print(plot_for_hos_type_no_from)
insertPlot(OUT, "שכר לסוג גוף - כלל", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)
# רופאים
addWorksheet(OUT, "שכר לסוג גוף - רופאים")
writeDataTable(OUT, "שכר לסוג גוף - רופאים", data_for_hos_type_doc_no_from)
print(plot_for_hos_type_doc_no_from)
insertPlot(OUT, "שכר לסוג גוף - רופאים", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# תורנויות מתמחים ----

# אזור
addWorksheet(OUT, "תורנויות לפי אזור")
writeDataTable(OUT, "תורנויות לפי אזור", data_for_cmut_toran_mitmaha_ezor_agaf_claliem)
print(plot_for_cmut_toran_mitmaha_ezor_agaf_claliem)
insertPlot(OUT, "תורנויות לפי אזור", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# סוג ביתחולים
addWorksheet(OUT, "תורנויות לפי סוג בית חולים")
writeDataTable(OUT, "תורנויות לפי סוג בית חולים", data_for_cmut_toran_mitmaha_hos_type)
print(plot_for_cmut_toran_mitmaha_hos_type)
insertPlot(OUT, "תורנויות לפי סוג בית חולים", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# מגדר
addWorksheet(OUT, "תורנויות לפי מגדר")
writeDataTable(OUT, "תורנויות לפי מגדר", data_for_cmut_toran_mitmaha_sex_claliem)
print(plot_for_cmut_toran_mitmaha_sex_claliem)
insertPlot(OUT, "תורנויות לפי מגדר", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# כללית
# עיסוק
addWorksheet(OUT, "כללית - תורנויות לפי עיסוק")
writeDataTable(OUT, "כללית - תורנויות לפי עיסוק", data_for_cmut_toran_mitmaha_esok)
print(plot_for_cmut_toran_mitmaha_esok)
insertPlot(OUT, "כללית - תורנויות לפי עיסוק", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)


# שכר ברמת החודש
# כולם
addWorksheet(OUT, "שכר ברמת החודש - כולם")
writeDataTable(OUT, "שכר ברמת החודש - כולם", data_sal_by_month_all)
print(plot_sal_by_month_all)
insertPlot(OUT, "שכר ברמת החודש - כולם", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)
#רופאים
addWorksheet(OUT, "שכר ברמת החודש - רופאים")
writeDataTable(OUT, "שכר ברמת החודש - רופאים", data_sal_by_month_doc)
print(plot_sal_by_month_doc)
insertPlot(OUT, "שכר ברמת החודש - רופאים", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# חדשים ועוזבים
addWorksheet(OUT, "חדשים ועוזבים")
writeDataTable(OUT, "חדשים ועוזבים", data_for_new_and_leave)
print(plot_for_new_and_leave)
insertPlot(OUT, "חדשים ועוזבים", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# מתמידים
plot_change_sal_all + plot_change_sal_matmidim
addWorksheet(OUT, "מתמידים")
writeDataTable(OUT, "מתמידים", data_change_sal_all)
print(plot_change_sal_all)
insertPlot(OUT, "מתמידים", xy = c("A", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)
writeDataTable(OUT, "מתמידים", data_change_sal_matmidim, xy = c("J", 2))
print(plot_change_sal_matmidim)
insertPlot(OUT, "מתמידים", xy = c("J", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)

# שכר לפי רישיון רופא
addWorksheet(OUT, "לפי רישיון רופא")
writeDataTable(OUT, "לפי רישיון רופא", data_for_esok_type)
print(plot_for_esok_type)
insertPlot(OUT, "לפי רישיון רופא", xy = c("E", 2), width = 30, height = 20, fileType = "png", units = "cm", dpi = 600)


#--------------------------------------------------------------#

# Export the file ----
saveWorkbook(OUT, "output_all_health_2020_2021_08_27.xlsx",
             overwrite = F)


#--------------------------------------------------------------#
clalit_17_20 <- 
  all_health_12_to_20[from == 'clalit' & year %in% c(2017, 2018, 2019, 2020)][order(id, date)]


clalit_17_20[
  , shift_sal := shift(sal_bruto_shotef_hefrshim, 1),
  by = id]

clalit_17_20$delta <- 
  clalit_17_20$sal_bruto_shotef_hefrshim - clalit_17_20$shift_sal


clalit_17_20[year == 2020, sum()]





clalit_17_20$sal

clalit_17_20[date == '2020-05-01'][order(delta)][, .(id, delta, sal_bruto_shotef_hefrshim)] %>% View()

data_sal_by_month <- 
  clalit_17_20[dirog_kiboz == 'רופאים ללא סטאזרים',
               .(sum_sal_bruto_shotef_hefrshim = sum(sal_bruto_shotef_hefrshim),
                 sum_isod_msolav = sum(isod_msolav),
                 sum_isod_msolav_per_halki_misra = sum(isod_msolav) / sum(halki_misra),
                 sum_delta = sum(delta, na.rm = T),
                 sum_avoda_noseft = sum(avoda_noseft),
                 sum_avoda_noseft_per_halki_misra = sum(avoda_noseft) / sum(halki_misra),
                 N = .N,
                 sum_halki_misra = sum(halki_misra),
                 sal_bruto_shotef_hefrshim_per_N = sum(sal_bruto_shotef_hefrshim) / .N,
                 sum_delta_per_halki_misra = sum(delta, na.rm = T) / sum(halki_misra),
                 sal_bruto_shotef_hefrshim_per_halki_misra = sum(sal_bruto_shotef_hefrshim) / sum(halki_misra),
                 sal_bruto_shotef_hefrshim_kzat_per_halki_misra = ((sum(sal_bruto_shotef_hefrshim) + sum(kzat)) / sum(halki_misra))),
               by = date][,year := factor(year(date))][,month := factor(month(date))]



plot_sal_by_month <- 
  data_sal_by_month %>% 
  ggplot() +
  geom_line(aes(x = month, y = sal_bruto_shotef_hefrshim_per_halki_misra, group = year, color = year),
            size = 1.2) +
  theme_classic() +
  #labs(title = "שכר חודשי, כלל האוכלוסיה",
  #     subtitle = "שכר ממוצע למשרה מלאה, מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2017-2020",
  #     y = "",
  #     x = "",
  #     caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 8, color = "black"),
        axis.text.y = element_text(face="bold", size = 8, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        #legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")) +
  easy_remove_y_axis(what = c("line"))

plot_sal_by_month


data_sal_by_month %>% View()


all_health_12_to_20$month <- month(all_health_12_to_20$date)
all_health_12_to_20[from == 'clalit' & year %in% c(2020) & month %in% c(2,3,4,5)][order(id, date)][, .(date,id, sal_bruto_shotef_hefrshim)] %>% View







plot_sal_by_month <- 
  data_sal_by_month %>% 
  ggplot() +
  geom_line(aes(x = month, y = sal_bruto_shotef_hefrshim_kzat_per_halki_misra, group = year, color = year),
            size = 1.2) +
  theme_classic() +
  #labs(title = "שכר חודשי, כלל האוכלוסיה",
  #     subtitle = "שכר ממוצע למשרה מלאה, מערכת הבריאות, נתונים מאוחדים (ממשלתיים, כללית, איכילוב ובני ציון), 2017-2020",
  #     y = "",
  #     x = "",
  #     caption = "מקור: אגף שכר והסכמי עבודה, משרד האוצר") +
  theme(axis.text.x = element_text(face="bold", size = 8, color = "black"),
        axis.text.y = element_text(face="bold", size = 8, color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        #legend.background = element_rect(fill = NA, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 1, size = 12), 
        plot.title = element_text(hjust = 1, size = 16, face="bold", color = "black"),
        panel.grid.major.y = element_line(size=0.5, colour = "#939393")) +
  easy_remove_y_axis(what = c("line"))

plot_sal_by_month

#-----# 

d <- 
all_health_12_to_20[, mean(sal_bruto_shotef_hefrshim) / mean(halki_misra),
                    by = .(id, year, dirog_kiboz)][, .(mean = mean(V1)),
                                                   by = .(year, dirog_kiboz)]

d %>% 
  ggplot() +
  aes(x = year, y = mean, color = dirog_kiboz) +
  geom_line()




all_health_12_to_20$last_thom

#----#
all_health_20_docs <- 
  all_health_12_to_20[year == 2020 & 
                        dirog_kiboz == "רופאים ללא סטאזרים"]
                      

all_health_20_docs$last_thom_k <- ifelse(all_health_20_docs$last_thom %in%
all_health_12_to_20[year == 2020 & 
                      dirog_kiboz == "רופאים ללא סטאזרים",
                    .N,
                    by = last_thom][order(-N)]$last_thom[2:7],
all_health_20_docs$last_thom,
"אחר")


d <- 
all_health_20_docs[, .(mean_full_sal = mean(sal_bruto_shotef_hefrshim) / mean(halki_misra)),
                   by = last_thom_k][,last_thom_k := fct_reorder(last_thom_k, -mean_full_sal)]
  
d %>% 
  ggplot() +
  aes(x = last_thom_k, y = mean_full_sal) +
  geom_bar(stat = "identity",
           width = 0.7,
           fill = "#06608C") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 70000, 10000))







#----#
all_health_docs <- 
  all_health_12_to_20[dirog_kiboz == "רופאים ללא סטאזרים"]


all_health_docs$last_thom_k <- ifelse(all_health_docs$last_thom %in%
                                           all_health_12_to_20[dirog_kiboz == "רופאים ללא סטאזרים",
                                                               .N,
                                                               by = last_thom][order(-N)]$last_thom[2:7],
                                      all_health_docs$last_thom,
                                         "אחר")


d <- 
  all_health_docs[, .(mean_full_sal = mean(sal_bruto_shotef_hefrshim) / mean(halki_misra)),
                     by = .(year, last_thom_k)]

d %>% 
  ggplot() +
  aes(x =year , y = mean_full_sal, color =last_thom_k ) +
  geom_line(size = 1.8) +
  geom_point(size = 3, color = "black") +
  theme_classic()
  
  
geom_bar(stat = "identity",
           width = 0.7,
           fill = "#06608C") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 70000, 10000))



all_health_docs[, .N, by = last_thom_k] %>% datatable()
