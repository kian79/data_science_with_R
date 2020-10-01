dickens_novels<-dickens_novels%>%
  group_by(gutenberg_id)%>%
  mutate(book=first(text))%>%ungroup()
words <-
  dickens_novels%>%
  unnest_tokens(word, text, token='words',to_lower = FALSE)

common_names<-words%>%
  filter(str_detect(word,"[A-Z][a-z]+"))%>%
  filter(!(str_to_lower(word) %in% words$word))%>%
  filter(word!="Mr" & word!="Mrs")%>%
  group_by(book,word)%>%
  summarise(num=n())%>%
  arrange(book,num)%>%
  top_n(5)

ggplot(names,aes(x = word,y = num, fill = book)) +
  geom_col(stat = 'identity') +
  facet_wrap(. ~ book,nrow = 3,scales = "free") +
  geom_text(aes(label = word),hjust="left")+
  coord_flip() +
  scale_y_continuous(limits = c(0,max(names$num)*2))+
  theme(legend.position = 'none',
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())