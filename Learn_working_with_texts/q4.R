View(gutenberg_metadata)
miserables <- gutenberg_download(c(48731,48732,48733,48734,48735))
View(miserables)
miserables_words <-
  miserables%>%
  unnest_tokens(word, text, token='words')
miserables_words<-
  miserables_words%>%
  anti_join(stop_words,bu='word')%>%
  select(word)

all_words <-miserables_words$word
positive <-get_sentiments("bing")
emo_words <-
  miserables_words %>%
  dplyr::mutate(order = row_number()) %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(index = floor(order%%200)) %>%
  group_by(index ,sentiment) %>%
  dplyr::count() %>%
  tidyr::spread(sentiment, n, fill = 1) %>%
  mutate(sentiment = positive - negative)

ggplot(emo_words, aes(x=index)) +
  geom_bar(stat = "identity",aes(y=sentiment,fill="sentiment"))
