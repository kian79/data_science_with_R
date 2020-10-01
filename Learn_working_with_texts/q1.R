library(ggplot2)
library(stringr)
library(gutenbergr)
library(tidytext)
library(dplyr)
library(wordcloud2)
dickens_novels <-
  gutenbergr::gutenberg_download(c(98,1023,917,766,821,786,963,1400,730,883,564,700,968,580,967))
View(dickens_novels)
words <-
  dickens_novels%>%
  unnest_tokens(word, text, token='words')

words%>%
  anti_join(stop_words, by = 'word') -> meaning_words
head(words)
top20 <-
  meaning_words%>%
  filter(! is.na(word))%>%
  group_by(word)%>%
  dplyr::count()%>%
  arrange(desc(n))%>%
  ungroup()%>%
  mutate(top= seq_along(word))%>%
  head(20)
View(top20)
ggplot(top20,aes(x = top,y = n)) +
  geom_bar(stat = 'identity', col = "transparent") +
  geom_text(aes(y = n+50, label = word), hjust = "left") +
  coord_flip() +
  theme_minimal()+
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  labs(x='words')