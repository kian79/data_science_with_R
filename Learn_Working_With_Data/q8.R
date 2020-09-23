View(spain)
library(dplyr)
library(tidyr)
library(devtools)
come_back_tb <-
  spain%>%
  separate(HT, c('ht_h','ht_v'),sep='-')%>%
  separate(FT, c('ft_h','ft_v'),sep='-')%>%
  filter(abs(as.numeric(ht_h) - as.numeric(ht_v)) == 2 )
View(come_back_tb)
come_back_tb <-
  come_back_tb%>%
  mutate( won = case_when(((as.numeric(ht_h) - as.numeric(ht_v)) * (as.numeric(ft_h)-as.numeric(ft_v)) > 0) ~ 'T', TRUE ~ 'F'))
View(come_back_tb)
come_back_tb <-
  come_back_tb%>%
  mutate(
    num = n()
  )%>%
  filter(won == 'T')%>%
  summarise(
    win_rate = n() / num
  )%>%
  distinct()
View(come_back_tb) # احتمال برنده شدن تیمی که در نیمه اول دو گل جلو افتاده است
