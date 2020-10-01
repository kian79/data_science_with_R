library(quRan)
quran_words<-
  quran_ar_min%>%
  unnest_tokens(word, text, token='words')
all_words<-
  quran_words%>%
  group_by(word)%>%
  mutate(N=n())%>%
  select(word,N)%>%
  unique()
View(all_words)
