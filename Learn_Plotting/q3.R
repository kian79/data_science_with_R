#3
View(mobile_data)
price_sim <-
  mobile_data%>%
  group_by(sim_no,LTE)%>%
  drop_na(price)%>%
  summarise(
    price = mean(price),
  )%>%
  unique()
View(price_sim)
ggplot(price_sim,aes(x=sim_no,y=price))+
geom_bar(stat = 'identity',aes(fill=LTE),position = position_dodge(
    preserve = "single"))
