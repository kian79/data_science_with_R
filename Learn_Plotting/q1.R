#1
install.packages('ggplot2')
library(ggplot2)
library(dplyr)
mobile_data <- readRDS("~/Desktop/hesaba/HW3/mobile_data.rds")
mobile_data <- # to separate watches add display sizze limits // gear saate!!!
  mobile_data%>%
  filter(
    sim_no!=0
  )%>%
  filter(battery_mah <= 5300)%>%
  filter(weight<300 & weight>50)%>%
  filter(display_size <7)

companies <-
  mobile_data%>%
  group_by(company)%>%
  summarise(
    number_of_products = n()
  )%>%
  arrange(desc(number_of_products))%>%
  head(20)
companies_for_plot <-
  mobile_data%>%
  filter(
    company %in% companies$company
  )
ggplot(companies, aes(x=company, y=number_of_products,fill = company, angle=180))+
  geom_bar(stat="identity")+theme_minimal()+coord_flip()
