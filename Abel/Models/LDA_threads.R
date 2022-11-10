###################################
# Topic Evolution - Threads    ####
###################################

library(dplyr)
library(tidyr)
library(tidytext)
library(stopwords)
library(topicmodels)
library(ggplot2)
library(DescTools)
library(methods)

mails = read.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/Data_clean_processed.csv", stringsAsFactors = F)
colnames(mails)[colnames(mails)=="email_id"] = "message_id"
colnames(mails)[colnames(mails)=="bow"] = "clean_body"
mails$message = sapply( mails$message_id, function(x) unlist(strsplit(x, "#"))[1] )
vocab = read.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/clean_vocab.csv", stringsAsFactors = F)

# Sample threads
ids =  data.frame( message_id = mails$message )
ids = ids %>% group_by(message_id) %>% summarise(n=length(message_id)) 

# Weights selection
ids$w = (ids$n^10)/sum(ids$n^10)
set.seed(123)
s = sample(ids$message_id, 20000, prob = ids$w)

# Threads Dn
ids$n %>% hist(freq=FALSE, col=rgb(0,0,1,0.5), main="Thread Length Distribution")
( ids %>% filter(message_id %in% s, n>1) )$n %>% hist(add=T, freq=FALSE, col=rgb(1,0,0,0.5))
legend("topright", legend=c("Long Threads Sample", "Total Emails"), col=c(rgb(1,0,0,0.5), 
                                                      rgb(0,0,1,0.5)), pt.cex=2, pch=15 )
ids$n %>% summary
( ids %>% filter(message_id %in% s, n>1) )$n %>% summary

# Sample data
mails_s = mails %>% filter(message %in% s)
remove(mails); gc()

words_in = mails_s %>% dplyr::select(message_id, clean_body) %>%
  unnest_tokens(word, clean_body) %>% filter(word %in% vocab$term, !word %like% "%aaa%",
                                             !word %in% c("imagemasker", "urlmasker")) %>%# Vocab filter 
  count(message_id, word) %>% cast_dfm(message_id, word, n)

inicio = Sys.time()
mod = LDA(words_in, control = list(seed=123), k=7)
fin = Sys.time()
fin - inicio
save(mod, file="LDA_threads.RData")
load("LDA_threads.RData")

# Topics visualization
tidy(mod, matrix = "beta") %>% group_by(topic) %>%
  slice_max(beta, n = 10) %>% ungroup() %>%
  arrange(topic, -beta) %>% mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Topic labeling
tidy(mod, matrix = "gamma") %>% filter(topic == 1) %>% arrange(desc(gamma))

# Topics evolutions in threads
props = tidy(mod, matrix="gamma")
props$doc = sapply( props$document, function(x) unlist(strsplit(x, "#"))[1]  )
props$id = sapply( props$document, function(x) as.numeric( unlist(strsplit(x, "#"))[2])  )
ms = unique(props$doc)
plot_props = function(x){
  ggplot(x, aes(x=id, y=gamma, colour=factor(topic), group=factor(topic) )) + #geom_line() +
    stat_smooth(method = "loess", formula = y ~ x, size = 1,
                method.args = list(degree=1)) + labs(y="Proportion", x = "Thread number") +
    ggtitle("Smoothed Average Proportions" )
}
# props %>% filter(doc==ms[2]) %>% plot_props()
props %>% group_by(id, topic) %>% summarise(gamma = mean(gamma)) %>% plot_props()


