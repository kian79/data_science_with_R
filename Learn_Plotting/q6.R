#6
View(mobile_data)
gooshkoobs <-
  mobile_data%>%
  filter(dim_thickness > 20 & dim_length < 130 & dim_breadth < 50 & weight < 120)%>%
  mutate(
    gooshkoobiat = dim_thickness * dim_length * dim_breadth / weight
  )%>%
  arrange(desc(gooshkoobiat))%>%
  head(20)
View(gooshkoobs)
ggplot(gooshkoobs,aes(x = paste(company,device, sep = " "), y = gooshkoobiat))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  xlab('Company and Device')
