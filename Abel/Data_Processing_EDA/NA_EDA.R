library(dplyr)
library(tidyr)
library(readr)
library(DescTools)
source("Data_Processing_EDA/missing_visualization.R")

############
# NA EDA  ##
############

mails = read_csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/complete_data.csv")
# Drop duplicates
mails = mails[!duplicated(mails[,-1]), ]
visualize_missings(mails, percent=TRUE)

# EDA
# 4:
(mails %>% filter(is.na(bcc), is.na(cc), is.na(subject), !is.na(to)))[1,1]

# 6:
(mails %>% filter(!is.na(bcc), !is.na(cc), is.na(to), !is.na(subject) ))[1,1]

####################
# Images, Tables  ##
####################

# From character vector, keep only first 5 words
cut_5 <- function(x){
  x = strsplit(x, " ")
  y = sapply(x, function(u){
    if(length(u)>5){
      return( paste(u[1:5], collapse=" ") )
    }else{
      return( paste(u, collapse=" ") )
    }
  })
  return(unlist(y))
}

# Links / http: allen-p\\all_documents\\1, buy-r\\inbox\\159
Links  = mails %>% filter(body %like% "%http%")

# Labeling: FP = 0
set.seed(123)
l = Links %>% sample_n(50) %>% pull(message_id)
Links %>% filter(message_id == l[1]) %>% pull(body)

# IMAGE: allen-p\\all_documents\\17, 259, 354, 355, 357
# png: allen-p\\all_documents\\469, 473
Image = mails %>% filter(body %like% "%IMAGE%")
Image = Image %>% mutate(subject_5 = cut_5(subject))
Image$main_folder = sapply(strsplit(Image$message_id, "\\\\"), function(x) x[1] )

# Labeling: FP = 0
set.seed(123)
i = Image %>% sample_n(50) %>% pull(message_id)
Image %>% filter( message_id == i[1] ) %>% pull(body)

par(mar=c(10,5,3,3))
(table(Image$subject_5) %>% sort(decreasing = T))[1:10] %>% barplot(cex.names=0.5,
                                                                    las=2, ylab="emails",
                                                                    main="Top 10 Subjects in Image emails (4849 total)")
par(mar=c(6,5,3,3))
(table(Image$main_folder) %>% sort(decreasing = T))[1:10] %>% barplot(cex.names=0.7, las=2, ylab="emails",
                                                                      main="Top 10 Users in Image emails")

# Topics
tidy(LDA_0, matrix = "beta") %>% group_by(topic) %>%
  slice_max(beta, n = 10) %>% ungroup() %>%
  arrange(topic, -beta) %>% mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

