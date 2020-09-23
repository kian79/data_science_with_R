#4
thickness_headphone <-
  mobile_data%>%
  filter(year == 2017)%>%
  drop_na(dim_thickness,audio_jack)

ggplot(thickness_headphone,aes(audio_jack, dim_thickness, color=audio_jack))+
  geom_boxplot() + theme_minimal()
