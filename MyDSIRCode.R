install.packages("textreadr")
library(textreadr)
install.packages("antiword")
library(antiword)
install.packages("tidytext")
library(tidytext)
library(dplyr)
setwd("C:/Users/Breanna/Documents/Predict 453")
getwd()
doc <- read_docx("MyDSI.docx", skip=0, remove.empty=TRUE, trim=TRUE)
text_df <- data_frame(doc, text=doc)
text_df2 <- text_df %>% unnest_tokens(word, text)
text_df2 <- text_df2[2]

data(stop_words)
tidy_text <- text_df2 %>% anti_join(stop_words)

library(ggplot2)
tidy_text %>% count(word, sort=TRUE) %>%
  filter(n>2) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col()+
  xlab(NULL) +
  coord_flip()

library(tidytext)

afinn <-  text_df2 %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment=sum(score)) %>%
  mutate(method="AFINN")

bing <-  text_df2 %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(method="BING")

nrc <-  text_df2 %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  mutate(method="NRC")

#build word cloud
install.packages("wordcloud")
library(wordcloud)
text_df2 %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=100))

install.packages("reshape2")
detach("reshape")
library(reshape2)
library(dplyr)

text_df2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("gray20", "gray80"), max.words=100)

text_df3 <- text_df2 %>%
  count(word, sort=TRUE) %>%
  ungroup()

#frequency by sentiment

bing_word_counts <- bing %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Contribution to sentiment", x=NULL) +
  coord_flip()

overall_sentiment <- bing_word_counts %>%
  group_by(sentiment) %>%
  summarise(total = sum(n))

overall_sentiment %>%
  ggplot(aes(sentiment, total, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  labs(y="Total Sentiment Contribution", x=NULL) +
  coord_flip()

########################################################################
#without word "trump"
bing_word_counts_no_trump = filter(bing_word_counts, word!= "trump")


bing_word_counts_no_trump %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Contribution to sentiment without 'trump'", x=NULL) +
  coord_flip()

overall_sentiment_no_trump <- bing_word_counts_no_trump %>%
  group_by(sentiment) %>%
  summarise(total = sum(n))

overall_sentiment_no_trump %>%
  ggplot(aes(sentiment, total, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  labs(y="Total Sentiment Contribution without Trump", x=NULL) +
  coord_flip()

####################################################################

#these are all going to be 0 because I only have one document
mydsi_tfidf <- text_df3 %>%
  mutate(book="DSI80") %>%
  bind_tf_idf(word, book, n)
#Work on bringing in DSIs and counting words 70, 73, 75, 78, 79


