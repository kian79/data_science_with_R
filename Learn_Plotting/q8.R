battery_weight <-
  mobile_data%>%
  drop_na(weight,battery_mah)

ggplot(battery_weight,aes(x=battery_mah,y=weight, color=year))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)
print(paste('Correlation:',cor(battery_weight$battery_mah,battery_weight$weight),sep = ' '))
