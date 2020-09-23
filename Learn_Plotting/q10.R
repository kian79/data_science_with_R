#1
size_year <-
  mobile_data%>%
  drop_na(year,price)%>%
  group_by(year)%>%
  summarise(
    mean_price = mean(price),
    mean_size = mean(display_size)
  )%>%
  filter(year<2017 & year > 2004)

ggplot(size_year,aes(x=year,y=mean_price))+
  geom_line()+
  geom_point()

#2
size_year <-
  mobile_data%>%
  drop_na(display_size,year)%>%
  group_by(year)%>%
  summarise(
    mean_price = mean(price),
    mean_size = mean(display_size)
  )

ggplot(size_year,aes(x=year,y=mean_size))+
  geom_line()+
  geom_point()

#3
size_year <-
  mobile_data%>%
  drop_na(dim_breadth, dim_length)%>%
  group_by(year)%>%
  summarise(
    mean_price = mean(price),
    mean_size = mean(display_size),
    area = mean(dim_breadth) * mean(dim_length)
  )

ggplot(size_year,aes(x=year,y=area))+
  geom_line()+
  geom_point()
