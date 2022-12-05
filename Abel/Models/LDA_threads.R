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
mails_s = read.csv("G:/.shortcut-targets-by-id/1pETyus-Qcj5yhAtbMb9lnWPa53WjHhHB/Capstone Project/Data/new_data_cleaned_sample.csv", stringsAsFactors = F)
# mails_s = mails %>% filter(message %in% s)
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

load("LDA_5.RData")
mod=LDA_opt
remove(LDA_opt); gc()

# Topics visualization

B = tidy(mod, matrix = "beta") 
B$Topic = B$topic
B$Topic[B$topic == 1] = "1 Enron News"
B$Topic[B$topic == 2] = "2 Market News"
B$Topic[B$topic == 3] = "3 Disclaimers"
B$Topic[B$topic == 4] = "4 Setting up Meetings"
B$Topic[B$topic == 5] = "5 Energy Markets"
B$Topic[B$topic == 6] = "6 Operational"
B$Topic[B$topic == 7] = "7 Chatting"
B$Topic = factor(B$Topic)

B %>% group_by(Topic) %>%
  slice_max(beta, n = 15) %>% ungroup() %>%
  arrange(Topic, -beta) %>% mutate(term = reorder_within(term, beta, Topic)) %>%
  ggplot(aes(beta, term, fill = Topic )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Topic, scales = "free") +
  scale_y_reordered() + labs(x="P(Term | Topic)", y="Term") 

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
    ggtitle("Smoothed Average Proportions" ) + labs(color="Topic") 
    # scale_colour_manual(labels =c("#F8766D", "#C59900", "#5BB300",
    #                             "#F8766D", "#F8766D", "#F8766D",
    #                             "#F8766D"),
    #                    values = c('Enron News'="#F8766D",'Market News'="#C59900", "Disclaimers"="#5BB300",
    #                               "Setting up Meetings"="#F8766D",
    #                               "Energy Markets"="#F8766D", "Operational"="#F8766D",
    #                               "Chatting"="#F8766D"))
}
plot_props2 = function(x){
  ggplot(x, aes(x=id, y=gamma, colour=Topic, group=Topic )) + #geom_line() +
    stat_smooth(method = "loess", formula = y ~ x, size = 1,
                method.args = list(degree=1)) + labs(y="Proportion", x = "Thread number") +
    ggtitle("Smoothed Average Proportions" ) + labs(color="Topic") 
  # scale_colour_manual(labels =c("#F8766D", "#C59900", "#5BB300",
  #                             "#F8766D", "#F8766D", "#F8766D",
  #                             "#F8766D"),
  #                    values = c('Enron News'="#F8766D",'Market News'="#C59900", "Disclaimers"="#5BB300",
  #                               "Setting up Meetings"="#F8766D",
  #                               "Energy Markets"="#F8766D", "Operational"="#F8766D",
  #                               "Chatting"="#F8766D"))
}
# props %>% filter(doc==ms[2]) %>% plot_props()
props$Topic = props$topic
props$Topic[props$Topic == 1] = "1 Enron News"
props$Topic[props$Topic == 2] = "2 Market News"
props$Topic[props$Topic == 3] = "3 Disclaimers"
props$Topic[props$Topic == 4] = "4 Setting up Meetings"
props$Topic[props$Topic == 5] = "5 Energy Markets"
props$Topic[props$Topic == 6] = "6 Operational"
props$Topic[props$Topic == 7] = "7 Chatting"
props$Topic = factor(props$Topic)
props %>% group_by(id, topic) %>% summarise(gamma = mean(gamma)) %>% plot_props()
props %>% group_by(id, Topic) %>% summarise(gamma = mean(gamma)) %>% plot_props2()

