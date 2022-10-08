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


# IMAGE: allen-p\\all_documents\\17, 259
Image = mails %>% filter(body %like% "%IMAGE%")
Image = Image %>% mutate(subject_5 = cut_5(subject))
Image$main_folder = sapply(strsplit(Image$message_id, "\\\\"), function(x) x[1] )

par(mar=c(10,5,3,3))
(table(Image$subject_5) %>% sort(decreasing = T))[1:10] %>% barplot(cex.names=0.5,
                                                                    las=2, ylab="emails",
                                                                    main="Top 10 Subjects in Image emails (4849 total)")
par(mar=c(6,5,3,3))
(table(Image$main_folder) %>% sort(decreasing = T))[1:10] %>% barplot(cex.names=0.7, las=2, ylab="emails",
                                                                      main="Top 10 Users in Image emails")


# Table: allen-p\\all_documents\\10, 359
Table = mails %>% filter(body %like% "%Table%")
Table = Table %>% mutate(subject_5 = cut_5(subject))
Table$main_folder = sapply(strsplit(Table$message_id, "\\\\"), function(x) x[1] )

par(mar=c(10,5,3,3))
(table(Table$subject_5) %>% sort(decreasing = T))[1:10] %>% barplot(cex.names=0.5, las=2, ylab="emails",
                                                                    main="Top 10 Subjects in Table emails (928 total)")
par(mar=c(6,5,3,3))
(table(Table$main_folder) %>% sort(decreasing = T))[1:10] %>% barplot(cex.names=0.7, las=2, ylab = "emails",
                                                                      main="Top 10 Users in Table emails")

#################
# Vanilla LDA  ##
#################

library(tidytext)
library(stopwords)
library(topicmodels)
library(ggplot2)

mails$body = tolower(mails$body)

# Remove stop words from bodies
non_stop_words = function(x){
  list = strsplit(x, " ")
  stop_w = stopwords("english")
  
  y = sapply(list, function(x){
    aux = x[! x %in% stop_w]
    return(paste(aux, collapse=" "))
  })
  
  return(y)
}

mails$body_clean = non_stop_words(mails$body)
write.csv(mails, file="complete_data_processed.csv", row.names = F)
mails = read.csv("complete_data_processed.csv", stringsAsFactors = F)

set.seed(123)
mails_words = mails %>% dplyr::sample_n(10000) %>% dplyr::select(message_id, body_clean) %>% unnest_tokens(word, body_clean) %>% 
  count(message_id, word) %>% cast_dfm(message_id, word, n)
  
# Vanilla LDA
LDA_0 = LDA(mails_words, k=5, control = list(seed=123)) 

# Topics
tidy(LDA_0, matrix = "beta") %>% group_by(topic) %>%
  slice_max(beta, n = 10) %>% ungroup() %>%
  arrange(topic, -beta) %>% mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

