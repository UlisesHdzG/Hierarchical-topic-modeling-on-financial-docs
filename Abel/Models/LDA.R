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
mails = read.csv("C:/Users/apere/Desktop/Data_Capstone/data_splited_3.csv", stringsAsFactors = F)
colnames(mails)[colnames(mails)=="email_id"] = "message_id"

set.seed(123)
words_in = mails %>% dplyr::sample_n(10000) %>% dplyr::select(message_id, clean_body) %>%
  unnest_tokens(word, clean_body) %>% count(message_id, word) %>% cast_dfm(message_id, word, n)
words_in = words_in[,!colnames(words_in) %like% "%aaa%"]# remove trash tokens

words_out = mails %>% filter(! message_id %in% words_in$message_id) %>% dplyr::sample_frac(0.5) %>%
  dplyr::select(message_id, clean_body) %>% unnest_tokens(word, clean_body) %>%
  group_by(word) %>% summarise( n=length(message_id))

words_out = words_out %>% filter(word %in% colnames(words_in) )


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
grid = data.frame( k = c(5, 7, 10, 12, 15, 20, 25, 30), log_pred = NA  )
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

logs_p = grid$log_pred
logs_p[1] = (logs_p[2] + logs_p[5])/2
plot(ts(logs_p), xaxt = 'n', xlab = 'k', ylab = 'Lambda(k)' )
axis(1, at = 1:7, labels = c( 5, 10, 15, 20, 25, 30, 40) )
points(1:7,logs_p )


set.seed(123)
words_in = mails %>% dplyr::sample_n(35000) %>% dplyr::select(message_id, clean_body) %>%
  unnest_tokens(word, clean_body) %>% count(message_id, word) %>% cast_dfm(message_id, word, n)
words_in = words_in[,!colnames(words_in) %like% "%aaa%"]# remove trash tokens


inicio = Sys.time()
LDA_opt = LDA(words_in, k= grid$k[ which(logs_p==max(logs_p)) ] , control = list(seed=123))
fin = Sys.time()
fin - inicio
# save(LDA_opt, file="LDA_sample_clean_opt.RData")
# save(LDA_opt, file="LDA_3_opt.RData")
save(LDA_opt, file="LDA_3_2_opt.RData")
load("LDA_3_2_opt.RData")


# Topics Visualization
tidy(LDA_opt, matrix = "beta") %>% group_by(topic) %>%
  slice_max(beta, n = 10) %>% ungroup() %>%
  arrange(topic, -beta) %>% mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Topic assignments LDA:
mails_class = tidy(LDA_opt, matrix = "gamma") %>% group_by(document) %>%
  summarise(topic = topic[ which(gamma == max(gamma)) ], gamma = max(gamma))
mails_class = mails_class %>% left_join(dplyr::select(mails, message_id, email),
                                        by = c('document'='message_id') )
mails_class %>%
  write.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Models Comparison/LDA3_emails_assignments.csv",
            row.names = F)

# Topics top 20 words:
tidy(LDA_opt, matrix = "beta") %>% group_by(topic) %>%
  slice_max(beta, n = 100) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  write.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Models Comparison/LDA3_topics_100.csv",
            row.names = F)


mails_class %>% filter(topic==6) %>% arrange(desc(gamma)) %>% slice(1:15)
mails %>% filter(message_id=="scott-s\\sent_items\\3#1") %>% select(clean_body) %>% pull

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
