chapters<-dickens_novels%>%
  unnest_lines(line,text)%>%
  group_by(book)%>%
  mutate(chapter = cumsum(str_detect(line, regex("^[IVX]+\\. |^[ivx]+\\. |^CHAPTER [IVX]+ |^CHAPTER [ONETWHRFUVSIXG]+|^chapter [ivx]+|^chapter \\d|^chapter [onetwhrfuvsixg]+"))))%>%
  group_by(book,chapter)%>%
  filter(n()>50)%>%
  summarise(text = paste0(line, collapse = " "))

unigrams<-chapters%>%
  unnest_tokens(unigram, text, token = "ngrams", n = 1)%>%
  group_by(book)%>%
  count(unigram)%>%
  separate(unigram, c("word1"), sep = " ")%>%
  filter(!word1 %in% stop_words$word) 

freq_rank_unigram<-unigrams%>%  
  group_by(book)%>%
  arrange(n)%>%
  mutate(frequency=n/n(),rank=last(row_number())-row_number())%>%
  arrange(book,rank)

bigrams<-chapters%>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  group_by(book)%>%
  count(bigram)%>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

freq_rank_bigram<-bigrams%>%  
  group_by(book)%>%
  arrange(n)%>%
  mutate(frequency=n/n(),rank=last(row_number())-row_number())%>%
  arrange(book,rank)

freqs<-
  rbind(freq_rank_bigram%>%select(rank,frequency,book)%>%mutate(ngram="bigram"),
        freq_rank_unigram%>%select(rank,frequency,book)%>%mutate(ngram="unigram"))

freqs<-freqs%>%
  mutate(book=str_trim(book))

ggplot(freqs,aes(rank,frequency,color=ngram))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~book,nrow = 5)+
  theme(strip.text = element_text())
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
  facet_wrap(~book,nrow = 5)
