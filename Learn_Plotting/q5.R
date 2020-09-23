ppi_mobile <-
  mobile_data%>%
  drop_na(px_row,px_col,display_size)%>%
  mutate(
    PPI = (sqrt((px_row)**2+(px_col)**2) / display_size)
  )
ggplot(ppi_mobile,aes(PPI))+geom_histogram(fill='blue')+theme_minimal()
