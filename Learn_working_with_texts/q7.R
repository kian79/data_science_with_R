austin_novels<-gutenberg_download(c(105,121,141,158,161,1342,21839,42671))
austin<-austin_novels%>%
  group_by(gutenberg_id)%>%
  mutate(book=str_trim(first(text)))%>%ungroup()

chapters<-austin%>%
  unnest_lines(line,text)%>%
  group_by(book)%>%
  mutate(chapter = cumsum(str_detect(line, regex("^[IVX]+\\. |^[ivx]+\\. |^CHAPTER [IVX]+ |^CHAPTER [ONETWHRFUVSIXG]+|^chapter [ivx]+|^chapter \\d|^chapter [onetwhrfuvsixg]+"))))%>%
  group_by(book,chapter)%>%
  filter(n()>50)%>%
  summarise(text = paste0(line, collapse = " "))

unigrams_per_chapter<-chapters%>%
  unnest_tokens(unigram, text, token = "ngrams", n = 1)%>%
  group_by(book,chapter)%>%
  count(unigram)%>%
  separate(unigram, c("word"), sep = " ")%>%
  filter(!word %in% stop_words$word)

freq_rank_unigram_per_chapter<-unigrams_per_chapter%>%  
  group_by(book,chapter)%>%
  arrange(n)%>%
  mutate(frequency=n/n(),rank=last(row_number())-row_number())%>%
  arrange(book,chapter,rank)

bigrams_per_chapter<-chapters%>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  group_by(book,chapter)%>%
  count(bigram)%>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


freq_rank_bigram_per_chapter<-bigrams_per_chapter%>%  
  group_by(book,chapter)%>%
  arrange(n)%>%
  mutate(frequency=n/n(),rank=last(row_number())-row_number())%>%
  arrange(book,chapter,rank)

freqs_by_chapter<-
  rbind(freq_rank_bigram_per_chapter%>%select(rank,frequency,book,chapter)%>%mutate(ngram="bigram"),
        freq_rank_unigram_per_chapter%>%select(rank,frequency,book,chapter)%>%mutate(ngram="unigram"))

freqs_by_chapter<-freqs_by_chapter%>%
  mutate(book=str_trim(book))
ggplot(freqs_by_chapter,aes(rank,frequency,color=ngram))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~book,nrow=5)