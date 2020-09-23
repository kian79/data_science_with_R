mobile_data%>%filter(company=='Samsung')
samsungـflagships<-samsungـmodels%>%
  mutate(model=case_when(
    str_detect(device,"Galaxy A\\d")~"Galaxy A",
    str_detect(device,"Galaxy J\\d")~"Galaxy J",
    str_detect(device,"Galaxy S\\d")~"Galaxy S",
    str_detect(device,"Note")~"Galaxy Note")
  )%>%group_by(year,model)%>%summarise(average_price=mean(price,rm.na=T))
ggplot(samsungـflagships,aes(x=year,y=average_price,color=model))+
  geom_line()+
  geom_point()+xlim(2010,2017)
