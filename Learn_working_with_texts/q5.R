women_verbs <-
  words%>%
  filter(lag(word)=='she'|lag(word)=='She')
words <-
  dickens_novels%>%
  unnest_tokens(word, text, token='words')
women_verbs<-words%>%
  filter(lag(word)=="she" |lag(word)=="She")%>%
  group_by(word)%>%
  count()%>%
  arrange(-n)%>%
  head(20)%>%
  mutate(gender="woman")
men_verbs<-words%>%
  filter(lag(word)=="he" |lag(word)=="He")%>%
  group_by(word)%>%
  count()%>%
  arrange(-n)%>%
  head(20)%>%
  mutate(gender="man")
verbs = full_join(women_verbs,men_verbs)
verbs <-
  verbs%>%
  ungroup%>%
  group_by(gender)%>%
  mutate(index = 1:n())
ggplot(verbs, aes(x=index,y=n, fill=gender))+
  geom_bar(stat='identity',position = position_dodge(
    preserve = 'single'
  ))+
  theme_minimal()+
  geom_label(aes(label = word))
