library(dplyr)
library(tidyr)
library(readr)
source("Data_Processing_EDA/missing_visualization.R")



mails = read_csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/maildir_table.csv")
visualize_missings(mails)

mails_B = read_csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/complete data.csv")
visualize_missings(mails_B, percent=TRUE)

# EDA
# 4:
(mails_B %>% filter(is.na(bcc), is.na(cc), is.na(subject), !is.na(to)))[1,1]

# 6:
(mails_B %>% filter(!is.na(bcc), !is.na(cc), is.na(to), !is.na(subject) ))[1,1]

# To y X-To vacios
(mails %>% filter( is.na(To), is.na(`X-To`) ))[1,1]



