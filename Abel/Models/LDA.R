###################
# Modeling: LDA  ##
###################
library(dplyr)
library(tidyr)
library(tidytext)
library(stopwords)
library(topicmodels)
library(ggplot2)

# Data comes from 'Cleaning.ipynb'
mails = read.csv("C:/Users/apere/Desktop/Data_Capstone/data_splited_nonstopwrods.csv", stringsAsFactors = F)

set.seed(123)
words_in = mails %>% dplyr::sample_n(10000) %>% dplyr::select(message_id, clean_body) %>%
  unnest_tokens(word, clean_body) %>% count(message_id, word) %>% cast_dfm(message_id, word, n)

words_out = mails %>% filter(! message_id %in% words_in$message_id) %>% dplyr::sample_n(10000) %>%
  dplyr::select(message_id, clean_body) %>% unnest_tokens(word, clean_body) %>%
  group_by(word) %>% summarise( n=length(message_id))

words_out = words_out[, colnames(words_out)[colnames(words_out) %in% colnames(words_in)]]
words_out = sapply(words_out, sum)

# LDA log-mrginal likelihood (marginalized over latent indicators and documents)
# return pseudo log predictive likelihood over test set 'words_out', using estimated parameters
# (topics and topic proportions), present in tibble 'B'
log_LDA = function(words_out, B){
  B = B %>% inner_join(words_out, by = c('term'='word') )
  l = sum(B$weighted_prob*B$n)
  return(l)
}

# Vanilla LDA
LDA_0 = LDA(words_in, k=5, control = list(seed=123)) 

B = tidy(LDA_0, matrix="beta") %>% pivot_wider(names_from=topic, values_from=beta)
t = tidy(LDA_0, matrix="gamma") %>% group_by(topic) %>% summarise(gamma = mean(gamma))
B$weighted_prob = log( as.vector( as.matrix(B[, 2:6])%*%t$gamma ))

log_LDA(words_out, B)

# Grid
set.seed(1234)
grid = data.frame( k = c(2, 5, 10, 15, 20, 30, 50), log_pred = NA  )
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

LDA_opt = LDA(words_in, k=15, control = list(seed=123))
save(LDA_opt, file="LDA_sample_opt.RData")

# Topics Visualization
tidy(LDA_opt, matrix = "beta") %>% group_by(topic) %>%
  slice_max(beta, n = 15) %>% ungroup() %>%
  arrange(topic, -beta) %>% mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


