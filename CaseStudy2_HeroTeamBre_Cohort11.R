##Predict 453 Cohort 11 Hero Team Bre

library(textreadr)
library(dplyr)
install.packages("rJava")
library(rJava)
install.packages("mallet")
library(mallet)
library(tidytext)
#read in the DSIs
df1 <- read_dir("C:/Users/Breanna/Documents/Predict 453/Case Study 2 DSIs", package="textreadr")

df2 <- df1 %>%
  group_by(document) %>%
  summarize(text=paste(content, collapse =" "))


df1_words <- df1 %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words) %>%
  count(document, word, sort=TRUE) %>%
  ungroup()

df1_words2 <- df1_words %>%
  bind_tf_idf(word, document, n) %>%
  inner_join(get_sentiments("bing"))

df_dfm <- df1_words %>%
  as.data.frame(cast_dfm(document, word, n))

library(ggplot2)
#frequency plot for entire df
df1_words %>% 
  filter(n>7) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col()+
  xlab(NULL) +
  coord_flip()

#####issues with comparison cloud#######
#create comparison cloud
df2_words <- df1_words[c(2:3)]

df2_words <- df2_words %>%
  group_by(word) %>%
  mutate(count=sum(n))
df2_words <- unique(df2_words[c(1,3)])

df2_words <- df2_words %>%
  inner_join(get_sentiments("bing"))

#Need to work this out.
df2_words %>%
  group_by(sentiment) %>%
  top_n(50, count)
  comparison.cloud(colors=c("gray20", "gray80"), max.words=50)

###try this
text_df2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("gray20", "gray80"), max.words=100)
warnings()

########

####df2 created for mallet lda
stopwords <- stop_words[,1]  
write.csv(stopwords, "stopwords.csv")
docsdf2 <- mallet.import(df2$document, df2$text, "stopwords.csv")

topic_model <- MalletLDA(num.topics=10)
topic_model$loadDocuments(docsdf2)
topic_model$train(200)

tidy_mallet <- tidy(topic_model)
tidy_mallet <- tidy(topic_model, matrix="gamma")
tidy_lda <- tidy(topic_model, matrix="beta")

#find the top n terms within each topic
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


vocabulary <- topic_model$getVocabulary()
word.freqs <- mallet.word.freqs(topic_model)

topics <- tidy_mallet %>%
     group_by(document) %>%
     mutate(max=max(gamma)) %>%
     mutate(indicator = ifelse(max==gamma, "yes", "no"))

topics2 <- filter(topics, indicator=="yes")
topics2 <- topics2[,1:3]


library(plyr)
term_counts <- rename(word_counts, term=word)
augment(mallet_model, term_counts)
##################################################################
#LDA topic modeling
install.packages("topicmodels")
library(topicmodels)

df1_dtm <- cast_dtm(df1_words, document, word, n)
df1_dtm

lda_model <- LDA(df1_dtm, k=20, control=list(seed=1234))
lda_topics <- tidy(lda_model, matrix="beta")

lda_top_terms <- lda_topics %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

lda_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales="free") +
  coord_flip()

lda_gamma <- tidy(lda_model, matrix="gamma")
lda_gamma %>%
  mutate(document=reorder(document, gamma*topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~document)

write.csv(lda_gamma, "LDA_TopicAssignments_20Topics.csv")
####################################################################
#sentiment analysis
no_trump = filter(df2_words, word!= "trump")

df2_words %>%
  group_by(sentiment) %>%
  top_n(10, count) %>%
  ungroup() %>%
  mutate(word=reorder(word,count)) %>%
  ggplot(aes(word, count, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Contribution to sentiment", x=NULL) +
  coord_flip() +
  ggtitle("Top 10 Negative & Positive Words")

overall_sentiment <- df2_words %>%
  group_by(sentiment) %>%
  summarise(total = sum(count))

overall_sentiment %>%
  ggplot(aes(sentiment, total, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  labs(y="Total Sentiment Contribution", x=NULL) +
  coord_flip() +
  ggtitle("Overall Sentiment")


########################################
#sentiment analysis without Trump
no_trump %>%
  group_by(sentiment) %>%
  top_n(10, count) %>%
  ungroup() %>%
  mutate(word=reorder(word,count)) %>%
  ggplot(aes(word, count, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Contribution to sentiment without Trump", x=NULL) +
  coord_flip() +
  ggtitle("Top 10 Negative & Positive Words without 'Trump'")

overall_sentiment_no_trump <- no_trump %>%
  group_by(sentiment) %>%
  summarise(total = sum(count))

overall_sentiment_no_trump %>%
  ggplot(aes(sentiment, total, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  labs(y="Total Sentiment Contribution without 'Trump'", x=NULL) +
  coord_flip() +
  ggtitle("Overall Sentiment without 'Trump'")

1229/1133
789/1133
###########################################################
#bi grams

df1_bigrams <- df1 %>%
  unnest_tokens(bigram, content, token="ngrams", n=2)

df1_bigrams %>%
  count(bigram, sort=TRUE)

library(tidyr)
bigrams_separated <- df1_bigrams %>%
  separate(bigram, c("word1", "word2"), sep=" ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort=TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort=TRUE)

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1=="not") %>%
  inner_join(AFINN, by=c(word2="word")) %>%
  count(word2, score, sort=TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution=n*score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2=reorder(word2, contribution)) %>%
  ggplot(aes(word2, n*score, fill=n*score >0))+
  geom_col(show.legend=FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by=c(word2="word")) %>%
  count(word1, word2, score, sort=TRUE) %>%
  ungroup()

install.packages("igraph")
library(igraph)
library(dplyr)
library(tidytext)
bigram_graph <- bigram_counts %>%
  filter(n>7) %>%
  graph_from_data_frame()

#install.packages("ggraph")
library(ggraph)
set.seed(2017)
library(ggplot)

ggraph(bigram_graph, layout="fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust=1, hjust=1)

####updated format
a<- grid::arrow(type="closed", length=unit(.15, "inches"))

ggraph(bigram_graph, layout="fr") +
  geom_edge_link(aes(edge_alpha=n), show.legend=FALSE, 
                arrow=a, end_cap=circle(.07, 'inches'))+
  geom_node_point(color="lightblue", size=5)+
  geom_node_text(aes(label=name), vjust=1, hjust=1) +
  theme_void()