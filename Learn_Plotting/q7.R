#7
View(mobile_data)
density_water <-
  mobile_data%>%
  drop_na(dim_length,dim_breadth,dim_thickness,weight)%>%
  filter((weight*dim_length*dim_breadth*dim_thickness) > 0)%>%
  mutate(
    density = as.integer(1000000*(weight / (dim_length*dim_breadth*dim_thickness))),
    stay_afloat = density < 1000
  )%>%
  filter(density > 2500)
View(density_water)

ggplot(density_water, aes(density))+
  geom_bar(aes(fill= stay_afloat))
