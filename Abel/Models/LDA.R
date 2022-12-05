###################
# Modeling: LDA  ##
###################
library(dplyr)
library(tidyr)
library(tidytext)
library(stopwords)
library(topicmodels)
library(ggplot2)
library(DescTools)

# Data comes from 'Cleaning.ipynb'
# mails = read.csv("C:/Users/apere/Desktop/Data_Capstone/data_splited_nonstopwrods.csv", stringsAsFactors = F)
# mails = read.csv("C:/Users/apere/Desktop/Data_Capstone/data_splited_nonstopwrods_http2.csv", stringsAsFactors = F)
# mails = read.csv("C:/Users/apere/Desktop/Data_Capstone/data_splited_3.csv", stringsAsFactors = F)
# mails = read.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/data_clean_processed_sample.csv", stringsAsFactors = F)
# mails = read.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/data_clean_processed_sample.csv", stringsAsFactors = F)

mails = read.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/new_data_cleaned_sample.csv", stringsAsFactors = F)
mails_30 = read.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/30_selected_samples.csv", stringsAsFactors = F)

colnames(mails)[colnames(mails)=="email_id"] = "message_id"
colnames(mails)[colnames(mails)=="bow"] = "clean_body"
vocab = read.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/clean_vocab.csv", stringsAsFactors = F)

words_in = mails %>% dplyr::select(message_id, clean_body) %>%
  unnest_tokens(word, clean_body) %>% filter(word %in% vocab$term, !word %like% "%aaa%") %>%# Vocab filter 
  count(message_id, word)
words_in = words_in %>% cast_dtm( message_id, word, n )
  #cast_dfm( message_id, word, n )

remove(mails); gc()
set.seed(123)
mails = read.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/data_clean_processed.csv", stringsAsFactors = F)
colnames(mails)[colnames(mails)=="email_id"] = "message_id"
colnames(mails)[colnames(mails)=="bow"] = "clean_body"

words_out = mails %>% filter(! message_id %in% rownames(words_in) ) %>%
  dplyr::select(message_id, clean_body) %>% unnest_tokens(word, clean_body) %>% filter(word %in% colnames(words_in) ) %>% 
  group_by(word) %>% summarise( n=length(message_id))


# LDA log-mrginal likelihood (marginalized over latent indicators and documents)
# return pseudo log predictive likelihood over test set 'words_out', using estimated parameters
# (topics and topic proportions), present in tibble 'B'
log_LDA = function(words_out, B){
  B = B %>% inner_join(words_out, by = c('term'='word') )
  l = sum(B$weighted_prob*B$n)
  return(l)
}


# Grid
set.seed(1234)
grid = data.frame( k = c(5, 7, 10, 12, 15, 20, 25), log_pred = NA  )
inicio = Sys.time()
for(i in 1:length(grid$k) ){
  cappa = grid$k[i]
  print(cappa)
  mod = LDA(words_in, k=cappa, control = list(seed=123))
  B = tidy(mod, matrix="beta") %>% pivot_wider(names_from=topic, values_from=beta)
  t = tidy(mod, matrix="gamma") %>% group_by(topic) %>% summarise(gamma = mean(gamma))
  B$weighted_prob = log( as.vector( as.matrix(B[, 2:ncol(B)])%*%t$gamma ))
  
  grid$log_pred[i] = log_LDA(words_out, B)
}
fin = Sys.time()
fin - inicio

print(grid)

logs_p = grid$log_pred
logs_p[1] = (logs_p[2] + logs_p[5])/2
plot(ts(logs_p), xaxt = 'n', xlab = 'k', ylab = 'Lambda(k)' )
axis(1, at = 1:7, labels = c( 5, 7, 10, 12, 15, 20, 25) )
points(1:7,logs_p )


inicio = Sys.time()
LDA_opt = LDA(words_in, control = list(seed=123), k=7)#= grid$k[ which(logs_p==max(logs_p)) ] )
fin = Sys.time()
fin - inicio
# save(LDA_opt, file="LDA_sample_clean_opt.RData")
# save(LDA_opt, file="LDA_3_opt.RData")
save(LDA_opt, file="LDA_5.RData")
load("LDA_5.RData")


# Topics Visualization
tidy(LDA_opt, matrix = "beta") %>% group_by(topic) %>%
  slice_max(beta, n = 20) %>% ungroup() %>%
  arrange(topic, -beta) %>% mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Topic assignments LDA:
mails_class = tidy(LDA_opt, matrix = "gamma") %>% group_by(document) %>%
  summarise(topic = topic[ which(gamma == max(gamma)) ], gamma = max(gamma))
table(mails_class$topic)/nrow(mails_class)

mails_class = mails_class %>% left_join(dplyr::select(mails, message_id, email),
                                        by = c('document'='message_id') )
mails_class %>%
  write.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Models Comparison/LDA4_nomask_emails_assignments.csv",
            row.names = F)


mails_30 %>% dplyr::select(-X) %>% left_join( transmute(mails_class,
                                                     email_id=document,
                                                     topic_LDA=topic,
                                                     P_topic_LDA=gamma ) ) %>% 
  write.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/30_selected_samples_LDA.csv",
            row.names = F)
# 1: enron news
# 2: energy market reports
# 


# Topics top 20 words:
tidy(LDA_opt, matrix = "beta") %>% group_by(topic) %>%
  slice_max(beta, n = 100) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  write.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Models Comparison/LDA4_nomaks_topics_100.csv",
            row.names = F)

table(mails_class$topic)/nrow(mails_class)
mails_class %>% filter(topic==5) %>% arrange(desc(gamma)) %>% slice(1:15)
mails %>% filter(email_id=="scott-s\\sent_items\\142#1") %>% select(bow) %>% pull

# document                             topic gamma
# <chr>                                <int> <dbl>
#   1 "martin-t\\deleted_items\\79#1"         10  1.00
# 2 "dasovich-j\\all_documents\\11276#1"    13  1.00
# 3 "martin-t\\inbox\\6#1"                  10  1.00
# 4 "kitchen-l\\_americas\\esvl\\747#1"     10  1.00
# 5 "staab-t\\inbox\\112#1"                  5  1.00
# 6 "dasovich-j\\all_documents\\2310#1"     14  1.00
# 7 "grigsby-m\\deleted_items\\194#1"        5  1.00
# 8 "dasovich-j\\all_documents\\1326#1"     14  1.00
# 9 "cuilla-m\\deleted_items\\387#1"         5  1.00
# 10 "lucci-p\\deleted_items\\592#1"          3  1.00

# 10: Negative and scandal Enron emails (79)
# 13: Energy market (11276)
# 3: Fantasy league ads (592)
# 14: Enterprise financing news (2310)

# 10: Overall scandall and concequences to employees (martin-t\\deleted_items\\79#1)
# 13: Energy price manipulation (dasovich-j\\all_documents\\11276#1)
# 3: Fantasy league ads (lucci-p\\deleted_items\\592#1)
# 14: Enterprise financing news (dasovich-j\\all_documents\\2310#1)


# Vanilla LDA
LDA_0 = LDA(words_in, k=5, control = list(seed=123)) 

B = tidy(LDA_0, matrix="beta") %>% pivot_wider(names_from=topic, values_from=beta)
t = tidy(LDA_0, matrix="gamma") %>% group_by(topic) %>% summarise(gamma = mean(gamma))
B$weighted_prob = log( as.vector( as.matrix(B[, 2:6])%*%t$gamma ))

log_LDA(words_out, B)
