#2
install.packages('tidyverse')
library(tidyverse)

info_year <-
  mobile_data%>%
  group_by(year)%>%
  drop_na(dim_breadth)%>%
  drop_na(dim_thickness)%>%
  drop_na(dim_length)%>%
  drop_na(cam_px)%>%
    summarise(
    mean_breadth = mean(dim_breadth),
    mean_thickness = mean(dim_thickness),
    mean_length = mean(dim_length),
    mean_cam_px = mean(cam_px)
      )
ggplot(info_year,aes(x=year))+
  geom_line(aes(y=mean_thickness,color = 'thckness'))+
  geom_line(aes(y=mean_length,color='length')) +
  geom_line(aes(y=mean_breadth,color = 'breadth'))+
  geom_line(aes(y=mean_cam_px,color='cam px'))
  

